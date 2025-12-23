//! CBOR support.
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
use alloc::string::String;
use ciborium_io::Write;
use ciborium_ll::{simple, tag, Encoder, Header};
use jaq_json::{Num, Tag, Val};

/// Write a value as CBOR.
pub fn write(w: &mut dyn std::io::Write, v: &Val) -> std::io::Result<()> {
    write_one(v, w)
}

/// Encode a single value to CBOR.
fn write_one<W: Write>(v: &Val, write: W) -> Result<(), W::Error> {
    let mut encoder = Encoder::from(write);
    encode(v, &mut encoder)
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
