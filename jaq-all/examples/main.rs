use jaq_all::{data, fmts, load};
use jaq_core::unwrap_valr;
use std::io::{self, Error, ErrorKind};

fn main() -> io::Result<()> {
    let filter = std::env::args()
        .nth(1)
        .ok_or_else(|| Error::new(ErrorKind::InvalidInput, "expected filter as first argument"))?;
    let filter = data::compile(&filter).map_err(|frs| {
        frs.iter()
            .for_each(|fr| eprint!("{}", load::FileReportsDisp::new(fr)));
        Error::from(ErrorKind::InvalidInput)
    })?;
    let input_filename = "<stdin>".to_string().into();
    let inputs = fmts::read::json::read_many(io::stdin().lock());
    let input = (&input_filename, inputs);
    let stdout = &mut io::stdout().lock();

    let runner = Default::default();
    let vars = Default::default();
    let fi = |e| Error::new(ErrorKind::InvalidData, e);

    data::run(&runner, &filter, vars, input, fi, |v| {
        let v = unwrap_valr(v).map_err(|e| Error::new(ErrorKind::Other, e.to_string()));
        fmts::write::write(stdout, &runner.writer, &v?)
    })
}
