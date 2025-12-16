use jaq_ext::{data, load, read, write};
use std::io::{self, Error, ErrorKind};

fn main() -> io::Result<()> {
    let stdin = io::stdin().lock();
    let stdout = &mut io::stdout().lock();

    let filter = std::env::args()
        .nth(1)
        .ok_or_else(|| Error::new(ErrorKind::InvalidInput, "expected filter as first argument"))?;
    let filter = jaq_ext::data::compile(&filter).map_err(|file_reports| {
        for (file, reports) in file_reports {
            for e in reports {
                eprintln!("Error: {}", e.message);
                // no color because ANSI must be enabled manually on Windows
                let block = e.to_block(&file.code, |_, text| text);
                eprint!("{}", load::PathBlock::new("", block));
            }
        }
        Error::from(ErrorKind::InvalidInput)
    })?;
    let inputs = read::json::read_many(stdin).map(|r| r.map_err(|e| e.to_string()));

    let runner = Default::default();
    let vars = Default::default();
    let fi = |e| Error::new(ErrorKind::InvalidData, e);

    data::run(&runner, &filter, vars, inputs, fi, |v| {
        let pp = Default::default();
        let v = v.map_err(|e| Error::new(ErrorKind::Other, e.to_string()));
        write::json::write(stdout, &pp, 0, &v?)
    })
}
