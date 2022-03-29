use clap::Parser;
use jaq_core::{Definitions, Val};
use mimalloc::MiMalloc;
use std::io::Write;
use std::rc::Rc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
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

    #[clap(short, long)]
    /// Write strings without escaping them with quotes
    raw_output: bool,

    #[clap(short, long)]
    /// Do not print a newline after each value
    join_output: bool,

    /// Filter to execute
    filter: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let main = jaq_core::parse::parse(&cli.filter, jaq_core::parse::main());
    let main = main.unwrap_or_else(|errors| {
        for err in errors {
            jaq::report(err)
                .eprint(ariadne::Source::from(&cli.filter))
                .unwrap();
        }
        std::process::exit(3);
    });

    let mut errs = Vec::new();
    let mut defs = Definitions::builtins();
    defs.add(jaq_core::std(), &mut errs);
    let filter = defs.finish(main, &mut errs);
    if !errs.is_empty() {
        for e in errs {
            eprintln!("Error: {}", e);
        }
        std::process::exit(3);
    }
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
            eprintln!("Failed to parse JSON: {}", e);
            std::process::exit(4);
        });
        //println!("Got {:?}", input);
        for output in filter.run(input) {
            let output = output.unwrap_or_else(|e| {
                eprintln!("Error: {}", e);
                std::process::exit(5);
            });
            last = Some(output.as_bool());
            match &*output {
                Val::Str(s) if cli.raw_output => print!("{}", s),
                _ => colored_json::write_colored_json(&(*output).clone().into(), &mut stdout)?,
            };
            if !cli.join_output {
                println!()
            }
        }
    }
    stdout.flush()?;

    if cli.exit {
        std::process::exit(last.map(|b| (!b).into()).unwrap_or(4));
    }
    Ok(())
}
