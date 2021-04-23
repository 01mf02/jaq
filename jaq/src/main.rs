use clap::Clap;
use jaq_core::{ClosedFilter, Main, Val};
use std::convert::TryFrom;
use std::io::Write;
use std::rc::Rc;

#[derive(Clap)]
struct Cli {
    /// Use null as single input value
    #[clap(short)]
    null: bool,

    /// Use the last output value as exit status code
    #[clap(short)]
    exit: bool,

    #[clap(short)]
    /// Read (slurp) all input values into one array
    slurp: bool,

    /// Filter to execute
    filter: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let main = Main::parse(&cli.filter).unwrap_or_else(|e| {
        eprintln!("Failed to parse filter:");
        eprintln!("{}", e.to_string());
        std::process::exit(3);
    });
    let filter = main.open(jaq_core::std()).unwrap_or_else(|e| {
        eprintln!("Error: {}", e.to_string());
        std::process::exit(3);
    });
    let filter = ClosedFilter::try_from(filter).unwrap();
    //println!("Filter: {:?}", filter);

    use std::iter::once;
    let stdin = std::io::stdin();
    let iter = if cli.null {
        Box::new(once(Ok(Rc::new(Val::Null)))) as Box<dyn Iterator<Item = _>>
    } else {
        let stdin = stdin.lock();
        let deserializer = serde_json::Deserializer::from_reader(stdin);
        let iter = deserializer.into_iter::<serde_json::Value>();
        Box::new(iter.map(|r| r.map(|x| Rc::new(Val::from(x)))))
    };

    let iter = if cli.slurp {
        let slurped: Result<Vec<_>, _> = iter.collect();
        Box::new(once(slurped.map(|v| Rc::new(Val::Arr(v)))))
    } else {
        Box::new(iter) as Box<dyn Iterator<Item = _>>
    };

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    let mut last = None;
    for item in iter {
        let input = item.unwrap_or_else(|e| {
            eprintln!("Failed to parse JSON: {}", e.to_string());
            std::process::exit(4);
        });
        //println!("Got {:?}", input);
        for output in filter.run(input) {
            let output = output.unwrap_or_else(|e| {
                eprintln!("Error: {}", e.to_string());
                std::process::exit(5);
            });
            last = Some(output.as_bool());
            let output = (*output).clone().into();
            colored_json::write_colored_json(&output, &mut stdout)?;
            println!();
        }
    }
    stdout.flush()?;

    if cli.exit {
        std::process::exit(last.map(|b| (!b).into()).unwrap_or(4));
    }
    Ok(())
}
