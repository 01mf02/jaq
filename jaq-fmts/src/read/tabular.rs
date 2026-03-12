//! CSV and TSV support.
use jaq_json::{read::parse_single_num, Val};

#[derive(Default)]
struct Field {
    /// normalised contents of the field
    bytes: Vec<u8>,
    /// how the field was terminated
    last: Option<u8>,
    /// were there any escape characters or quotes in the field?
    quote: bool,
}

impl Field {
    fn is_empty(&self) -> bool {
        self.bytes.is_empty() && self.last.is_none() && !self.quote
    }
}

impl From<Field> for Val {
    fn from(field: Field) -> Self {
        // TODO: should we also convert "null"?
        if field.quote {
            Val::utf8_str(field.bytes)
        } else if &*field.bytes == b"true" {
            Val::Bool(true)
        } else if &*field.bytes == b"false" {
            Val::Bool(false)
        } else {
            parse_single_num(&field.bytes).map_or_else(|| Val::utf8_str(field.bytes), Val::Num)
        }
    }
}

fn csv_field<E>(iter: &mut impl Iterator<Item = Result<u8, E>>) -> Result<Field, E> {
    let mut field = Field::default();
    field.last = 'outer: loop {
        match iter.next() {
            None => break None,
            Some(Ok(c @ (b',' | b'\n'))) => break Some(c),
            Some(Ok(b'"')) => loop {
                field.quote = true;
                match iter.next() {
                    Some(Ok(b'"')) => match iter.next() {
                        Some(Ok(b'"')) => field.bytes.push(b'"'),
                        Some(Ok(c)) => {
                            field.bytes.push(c);
                            continue 'outer;
                        }
                        Some(Err(e)) => return Err(e),
                        None => break 'outer None,
                    },
                    Some(Ok(c)) => field.bytes.push(c),
                    Some(Err(e)) => return Err(e),
                    None => break 'outer None,
                }
            },
            Some(Ok(byte)) => field.bytes.push(byte),
            Some(Err(e)) => return Err(e),
        }
    };
    Ok(field)
}

fn tsv_field<E>(iter: &mut impl Iterator<Item = Result<u8, E>>) -> Result<Field, E> {
    let mut field = Field::default();
    field.last = loop {
        match iter.next() {
            None => break None,
            Some(Ok(c @ (b'\t' | b'\n'))) => break Some(c),
            Some(Ok(b'\\')) => {
                field.quote = true;
                match iter.next() {
                    None => break None,
                    Some(Ok(b'n')) => field.bytes.push(b'\n'),
                    Some(Ok(b't')) => field.bytes.push(b'\t'),
                    Some(Ok(b'r')) => field.bytes.push(b'\r'),
                    Some(Ok(b'0')) => field.bytes.push(b'\0'),
                    Some(Ok(b'\\')) => field.bytes.push(b'\\'),
                    Some(Ok(byte)) => field.bytes.extend([b'\\', byte]),
                    Some(Err(e)) => return Err(e),
                }
            }
            Some(Ok(byte)) => field.bytes.push(byte),
            Some(Err(e)) => return Err(e),
        }
    };
    Ok(field)
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
            match field.last {
                None if fields.is_empty() && field.is_empty() => return None,
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
