//! CSV and TSV support.
use jaq_json::{read::parse_single_num, Val};

#[derive(Debug, Default)]
struct Field {
    /// normalised contents of the field
    bytes: Vec<u8>,
    /// character immediately after the field
    next: Option<u8>,
    /// were there any escape characters or quotes in the field?
    quote: bool,
}

impl Field {
    fn is_empty(&self) -> bool {
        self.bytes.is_empty() && !self.quote
    }
}

impl From<Field> for Val {
    fn from(field: Field) -> Self {
        if field.quote {
            Val::utf8_str(field.bytes)
        } else if field.bytes.is_empty() {
            Val::Null
        } else if &*field.bytes == b"true" {
            Val::Bool(true)
        } else if &*field.bytes == b"false" {
            Val::Bool(false)
        } else {
            parse_single_num(&field.bytes).map_or_else(|| Val::utf8_str(field.bytes), Val::Num)
        }
    }
}

fn field<E, I, F>(iter: &mut I, sep: u8, quote: u8, f: F) -> Result<Field, E>
where
    I: Iterator<Item = Result<u8, E>>,
    F: Fn(&mut I, &mut Field) -> Result<(), E>,
{
    let mut field = Field::default();
    field.next = loop {
        let next = field.next.take().map(Ok).or_else(|| iter.next());
        match next.transpose()? {
            None => break None,
            Some(c) if c == sep || c == b'\n' => break Some(c),
            Some(b'\r') => match iter.next().transpose()? {
                Some(c @ b'\n') => break Some(c),
                c @ (Some(_) | None) => {
                    field.bytes.push(b'\r');
                    field.next = c
                }
            },
            Some(c) if c == quote => {
                field.quote = true;
                f(iter, &mut field)?
            }
            Some(byte) => field.bytes.push(byte),
        }
    };
    Ok(field)
}

fn csv_field<E>(iter: &mut impl Iterator<Item = Result<u8, E>>) -> Result<Field, E> {
    field(iter, b',', b'"', |iter, field| loop {
        match iter.next().transpose()? {
            Some(b'"') => match iter.next().transpose()? {
                Some(b'"') => field.bytes.push(b'"'),
                c @ Some(_) => {
                    field.next = c;
                    return Ok(());
                }
                None => return Ok(()),
            },
            Some(c) => field.bytes.push(c),
            None => return Ok(()),
        }
    })
}

fn tsv_field<E>(iter: &mut impl Iterator<Item = Result<u8, E>>) -> Result<Field, E> {
    field(iter, b'\t', b'\\', |iter, field| {
        match iter.next().transpose()? {
            None => (),
            Some(b'n') => field.bytes.push(b'\n'),
            Some(b't') => field.bytes.push(b'\t'),
            Some(b'r') => field.bytes.push(b'\r'),
            Some(b'0') => field.bytes.push(b'\0'),
            Some(b'\\') => field.bytes.push(b'\\'),
            c @ Some(_) => {
                field.bytes.push(b'\\');
                field.next = c
            }
        };
        Ok(())
    })
}

fn lines<E, I: Iterator<Item = Result<u8, E>>>(
    mut iter: I,
    field: fn(&mut I) -> Result<Field, E>,
) -> impl Iterator<Item = Result<Val, E>> {
    core::iter::from_fn(move || {
        let mut fields = Vec::new();
        loop {
            let field = match field(&mut iter) {
                Ok(ok) => ok,
                Err(e) => return Some(Err(e)),
            };
            match field.next {
                None if fields.is_empty() && field.is_empty() => return None,
                Some(b'\n') if fields.is_empty() && field.is_empty() => break,
                None | Some(b'\n') => {
                    fields.push(field.into());
                    break;
                }
                _ => fields.push(field.into()),
            }
        }
        Some(Ok(Val::Arr(fields.into())))
    })
}

/// Read lines of a CSV file.
pub fn read_csv<E>(
    iter: impl Iterator<Item = Result<u8, E>>,
) -> impl Iterator<Item = Result<Val, E>> {
    lines(iter, csv_field)
}

/// Read lines of a TSV file.
pub fn read_tsv<E>(
    iter: impl Iterator<Item = Result<u8, E>>,
) -> impl Iterator<Item = Result<Val, E>> {
    lines(iter, tsv_field)
}
