use jaq_all::{data, fmts, load};
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
    let inputs = fmts::read::json::read_many(io::stdin().lock());
    let stdout = &mut io::stdout().lock();

    let runner = Default::default();
    let vars = Default::default();
    let fi = |e| Error::new(ErrorKind::InvalidData, e);

    data::run(&runner, &filter, vars, inputs, fi, |v| {
        let v = v.map_err(|e| {
            jaq_std::handle_exn_i32(
                e,
                |e| Error::new(ErrorKind::Other, e.to_string()),
                |exit_code| std::process::exit(exit_code),
            )
        });
        fmts::write::write(stdout, &runner.writer, &v?)
    })
}
