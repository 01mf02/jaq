use crate::bson::bson_stream;
use crate::cli::Cli;
use jaq_json::Val;
use std::io::{self, BufRead};
use std::path::Path;

/// Try to load file by memory mapping and fall back to regular loading if it fails.
pub fn load_file(path: impl AsRef<Path>) -> io::Result<Box<dyn core::ops::Deref<Target = [u8]>>> {
    let file = std::fs::File::open(path.as_ref())?;
    match unsafe { memmap2::Mmap::map(&file) } {
        Ok(mmap) => Ok(Box::new(mmap)),
        Err(_) => Ok(Box::new(std::fs::read(path)?)),
    }
}

pub fn invalid_data(e: impl std::error::Error + Send + Sync + 'static) -> std::io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}

fn json_slice(slice: &[u8]) -> impl Iterator<Item = io::Result<Val>> + '_ {
    let mut lexer = hifijson::SliceLexer::new(slice);
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        Some(Val::parse(lexer.ws_token()?, &mut lexer).map_err(invalid_data))
    })
}

fn json_read<'a>(read: impl BufRead + 'a) -> impl Iterator<Item = io::Result<Val>> + 'a {
    let mut lexer = hifijson::IterLexer::new(read.bytes());
    core::iter::from_fn(move || {
        use hifijson::token::Lex;
        let v = Val::parse(lexer.ws_token()?, &mut lexer);
        Some(v.map_err(|e| core::mem::take(&mut lexer.error).unwrap_or_else(|| invalid_data(e))))
    })
}

pub fn json_array(path: impl AsRef<Path>) -> io::Result<Val> {
    let bytes = load_file(path.as_ref())?;
    json_slice(&*bytes).collect()
}

pub fn buffered<'a, R>(cli: &Cli, read: R) -> Box<dyn Iterator<Item = io::Result<Val>> + 'a>
where
    R: BufRead + 'a,
{
    if cli.bson_input {
        let mut bytes = Vec::new();
        let mut reader = read;
        reader.read_to_end(&mut bytes).unwrap();
        let results: Vec<_> = bson_stream(&bytes).collect();
        return Box::new(collect_if(cli.slurp, results.into_iter()));
    }
    if cli.raw_input {
        Box::new(raw_input(cli.slurp, read).map(|r| r.map(Val::from)))
    } else {
        Box::new(collect_if(cli.slurp, json_read(read)))
    }
}

pub fn slice<'a>(cli: &Cli, slice: &'a [u8]) -> Box<dyn Iterator<Item = io::Result<Val>> + 'a> {
    if cli.bson_input {
        let results: Vec<_> = bson_stream(slice).collect();
        return Box::new(collect_if(cli.slurp, results.into_iter()));
    }
    if cli.raw_input {
        let read = io::BufReader::new(slice);
        Box::new(raw_input(cli.slurp, read).map(|r| r.map(Val::from)))
    } else {
        Box::new(collect_if(cli.slurp, json_slice(slice)))
    }
}

fn raw_input<'a, R>(slurp: bool, mut read: R) -> impl Iterator<Item = io::Result<String>> + 'a
where
    R: BufRead + 'a,
{
    if slurp {
        let mut buf = String::new();
        let s = read.read_to_string(&mut buf).map(|_| buf);
        Box::new(std::iter::once(s))
    } else {
        Box::new(read.lines()) as Box<dyn Iterator<Item = _>>
    }
}

fn collect_if<'a, T: FromIterator<T> + 'a, E: 'a>(
    slurp: bool,
    iter: impl Iterator<Item = Result<T, E>> + 'a,
) -> Box<dyn Iterator<Item = Result<T, E>> + 'a> {
    if slurp {
        Box::new(core::iter::once(iter.collect()))
    } else {
        Box::new(iter)
    }
}
