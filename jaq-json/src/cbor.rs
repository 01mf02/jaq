//! CBOR parsing.
//!
//! We can test this as follows:
//!
//!     cargo run -- --from cbor . <(echo 0x826161a161626163 | xxd -r)
//!
//! The [examples](https://www.rfc-editor.org/rfc/rfc8949.html#section-appendix.a)
//! from the CBOR specification are quite helpful here.
//! They can be pasted directly into the command above.
use crate::{Num, Val};
use alloc::string::String;
use alloc::vec::Vec;
use ciborium_io::{EndOfFile, Read};
use ciborium_ll::{simple, tag, Decoder, Error, Header};

/// Parse a single CBOR value from a byte slice.
pub fn parse_slice(slice: &[u8]) -> Result<Val, Error<EndOfFile>> {
    let mut decoder = Decoder::from(slice);
    let header = decoder.pull()?;
    parse(header, &mut decoder)
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

fn parse<R: Read>(header: Header, decoder: &mut Decoder<R>) -> Result<Val, Error<R::Error>> {
    match header {
        Header::Text(len) => {
            let mut s = String::new();
            let mut segments = decoder.text(len);
            while let Some(mut segment) = segments.pull()? {
                let mut buffer = [0; 4096];
                while let Some(chunk) = segment.pull(&mut buffer[..])? {
                    s.push_str(chunk);
                }
            }
            Ok(Val::from(s))
        }
        Header::Simple(simple::NULL | simple::UNDEFINED) => Ok(Val::Null),
        Header::Simple(simple::FALSE) => Ok(Val::Bool(false)),
        Header::Simple(simple::TRUE) => Ok(Val::Bool(true)),
        Header::Simple(..) => panic!("simple"),
        Header::Tag(tag::BIGNEG) => todo!(),
        Header::Tag(tag::BIGPOS) => todo!(),
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
        _ => panic!("received unexpected value"),
    }
}
