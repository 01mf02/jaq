use clap::Clap;
use jaq_core::{Filter, Val};
use std::io::Write;
use std::rc::Rc;

#[derive(Clap)]
struct Cli {
    #[clap(short)]
    /// Read (slurp) all input values into one array
    slurp: bool,

    /// Filter to execute
    filter: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let filter = match Filter::parse(&cli.filter) {
        Ok(filter) => filter,
        Err(e) => {
            eprintln!("Failed to parse filter:");
            eprintln!("{}", e.to_string());
            std::process::exit(1);
        }
    };
    //println!("Filter: {:?}", filter);

    let stdin = std::io::stdin();
    let stdin = stdin.lock();
    let deserializer = serde_json::Deserializer::from_reader(stdin);
    let iter = deserializer.into_iter::<serde_json::Value>();
    let iter = iter.map(|r| r.map(|x| Rc::new(Val::from(x))));

    let iter = if cli.slurp {
        let slurped: Result<Vec<_>, _> = iter.collect();
        Box::new(std::iter::once(slurped.map(|v| Rc::new(Val::Arr(v)))))
    } else {
        Box::new(iter) as Box<dyn Iterator<Item = _>>
    };

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    for item in iter {
        let input = match item {
            Ok(item) => item,
            Err(e) => {
                eprintln!("Failed to parse JSON: {}", e.to_string());
                std::process::exit(1);
            }
        };
        //println!("Got {:?}", input);
        for output in filter.run(input) {
            let output = (*output?).clone().into();
            colored_json::write_colored_json(&output, &mut stdout)?;
            println!();
        }
    }
    stdout.flush()?;

    Ok(())
}
