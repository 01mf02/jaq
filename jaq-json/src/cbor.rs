//! CBOR parsing.
//!
//! We can test this as follows:
//!
//! ~~~ text
//! echo 0x826161a161626163 | xxd -r | cargo run -- --from cbor
//! ~~~
//!
//! The [examples](https://www.rfc-editor.org/rfc/rfc8949.html#section-appendix.a)
//! from the CBOR specification are quite helpful here.
//! They can be pasted directly into the command above.
use crate::{Num, Tag, Val};
use alloc::string::String;
use alloc::vec::Vec;
use ciborium_io::{Read, Write};
use ciborium_ll::{simple, tag, Decoder, Encoder, Error, Header};
use num_bigint::{BigInt, BigUint};

/// Error that may indicate end of file.
pub trait IsEof {
    /// Is the error caused by EOF?
    fn is_eof(&self) -> bool;
}

// TODO: implement IsEof for EndOfFile, which is only visible when
// `ciborium_io` is compiled without the `std` feature
impl IsEof for std::io::Error {
    fn is_eof(&self) -> bool {
        matches!(self.kind(), std::io::ErrorKind::UnexpectedEof)
    }
}

/// Decode a single CBOR value.
pub fn parse_one<R: Read>(read: R) -> Result<Val, Error<R::Error>> {
    let mut decoder = Decoder::from(read);
    let header = decoder.pull()?;
    parse(header, &mut decoder)
}

/// Decode a sequence of CBOR values.
pub fn parse_many<R: Read>(read: R) -> impl Iterator<Item = Result<Val, Error<R::Error>>>
where
    R::Error: IsEof,
{
    let mut decoder = Decoder::from(read);
    core::iter::from_fn(move || {
        let offset = decoder.offset();
        match decoder.pull() {
            Ok(header) => Some(parse(header, &mut decoder)),
            // The following is a hack which is unfortunately necessary because
            // ciborium does not currently give us any means of checking for EOF
            // (<https://github.com/enarx/ciborium/issues/159>).
            // I found that an impartial header, such as triggered by 0x18,
            // advances the offset, so by negation, I conclude that
            // if the offset has remained the same after header decoding
            // AND the error is EOF, that means that there was no input.
            // Note that we cannot omit the EOF check, because a different
            // I/O error might have been triggered, and we should propagate that.
            Err(Error::Io(e)) if e.is_eof() && offset == decoder.offset() => None,
            Err(e) => Some(Err(e)),
        }
    })
}

/// Encode a single value to CBOR.
pub fn write_one<W: Write>(v: &Val, write: W) -> Result<(), W::Error> {
    let mut encoder = Encoder::from(write);
    encode(v, &mut encoder)
}

fn with_size<R: Read, T>(
    size: Option<usize>,
    decoder: &mut Decoder<R>,
    f: impl Fn(Header, &mut Decoder<R>) -> Result<T, Error<R::Error>>,
) -> Result<Vec<T>, Error<R::Error>> {
    if let Some(size) = size {
        let mut a = Vec::with_capacity(size);
        for _ in 0..size {
            a.push(f(decoder.pull()?, decoder)?);
        }
        Ok(a)
    } else {
        let mut a = Vec::new();
        loop {
            match decoder.pull()? {
                Header::Break => break,
                header => a.push(f(header, decoder)?),
            }
        }
        Ok(a)
    }
}

fn parse_bytes<R: Read>(
    len: Option<usize>,
    decoder: &mut Decoder<R>,
) -> Result<Vec<u8>, Error<R::Error>> {
    let mut b = len.map_or_else(Vec::new, Vec::with_capacity);
    let mut segments = decoder.bytes(len);
    while let Some(mut segment) = segments.pull()? {
        let mut buffer = [0; 4096];
        while let Some(chunk) = segment.pull(&mut buffer)? {
            b.extend(chunk);
        }
    }
    Ok(b)
}

fn biguint<R: Read>(decoder: &mut Decoder<R>) -> Result<BigUint, Error<R::Error>> {
    match decoder.pull()? {
        Header::Bytes(len) => Ok(BigUint::from_bytes_be(&parse_bytes(len, decoder)?)),
        _ => Err(Error::Syntax(decoder.offset())),
    }
}

fn parse<R: Read>(header: Header, decoder: &mut Decoder<R>) -> Result<Val, Error<R::Error>> {
    match header {
        Header::Text(len) => {
            let mut s = len.map_or_else(String::new, String::with_capacity);
            let mut segments = decoder.text(len);
            while let Some(mut segment) = segments.pull()? {
                let mut buffer = [0; 4096];
                while let Some(chunk) = segment.pull(&mut buffer[..])? {
                    s.push_str(chunk);
                }
            }
            Ok(Val::from(s))
        }
        Header::Bytes(len) => Ok(Val::byte_str(parse_bytes(len, decoder)?)),
        Header::Simple(simple::NULL | simple::UNDEFINED) => Ok(Val::Null),
        Header::Simple(simple::FALSE) => Ok(Val::Bool(false)),
        Header::Simple(simple::TRUE) => Ok(Val::Bool(true)),
        Header::Simple(_) => panic!("simple"),
        Header::Tag(tag::BIGNEG) => {
            biguint(decoder).map(|u| Val::Num(Num::big_int(-BigInt::from(u) - 1)))
        }
        Header::Tag(tag::BIGPOS) => {
            biguint(decoder).map(|u| Val::Num(Num::big_int(BigInt::from(u))))
        }
        Header::Tag(_) => panic!("tag"),
        Header::Positive(pos) => Ok(Val::Num(Num::from_integral(pos))),
        Header::Negative(neg) => Ok(Val::Num(Num::from_integral(neg as i128 ^ !0))),
        Header::Float(f) => Ok(Val::from(f)),
        Header::Array(size) => Ok(Val::Arr(
            with_size(size, decoder, |h, d| parse(h, d))?.into(),
        )),
        Header::Map(size) => {
            let o = with_size(size, decoder, |h, d| {
                Ok((parse(h, d)?, parse(d.pull()?, d)?))
            })?;
            Ok(Val::obj(o.into_iter().collect()))
        }
        Header::Break => panic!("break"),
    }
}

fn encode<W: Write>(v: &Val, encoder: &mut Encoder<W>) -> Result<(), W::Error> {
    use num_bigint::Sign;
    use simple::{FALSE, NULL, TRUE};
    match v {
        Val::Null => encoder.push(Header::Simple(NULL)),
        Val::Bool(b) => encoder.push(Header::Simple(if *b { TRUE } else { FALSE })),
        Val::Num(Num::Int(i)) => {
            let neg_succ = |i: isize| i.checked_add(1).and_then(isize::checked_neg);
            if let Ok(p) = u64::try_from(*i) {
                encoder.push(Header::Positive(p))
            } else if let Some(Ok(n)) = neg_succ(*i).map(u64::try_from) {
                encoder.push(Header::Negative(n))
            } else {
                encode(&Val::Num(Num::big_int((*i).into())), encoder)
            }
        }
        Val::Num(Num::BigInt(i)) => {
            let (tag, u) = match i.sign() {
                Sign::Plus | Sign::NoSign => (tag::BIGPOS, i.to_bytes_be()),
                Sign::Minus => (tag::BIGNEG, (-&**i - 1_u8).to_bytes_be()),
            };
            encoder.push(Header::Tag(tag))?;
            encoder.bytes(&u.1, None)
        }
        Val::Num(Num::Float(f)) => encoder.push(Header::Float(*f)),
        Val::Num(Num::Dec(d)) => encode(&Val::Num(Num::from_dec_str(d)), encoder),
        Val::Str(s, Tag::Utf8) => encoder.text(&String::from_utf8_lossy(s), None),
        Val::Str(b, Tag::Bytes) => encoder.bytes(b, None),
        Val::Str(b, Tag::Inline) => encoder.write_all(b),
        Val::Arr(a) => {
            encoder.push(Header::Array(Some(a.len())))?;
            a.iter().try_for_each(|x| encode(x, encoder))
        }
        Val::Obj(o) => {
            encoder.push(Header::Map(Some(o.len())))?;
            o.iter().try_for_each(|(k, v)| {
                encode(k, encoder)?;
                encode(v, encoder)
            })
        }
    }
}
