use std::io::{self, BufRead, Read};

fn main() -> io::Result<()> {
    let mut passed = 0;
    let mut failed = 0;

    let highlight = std::env::args().skip(1).any(|arg| arg == "--highlight");

    for tests in io::stdin().lock().split(b'\0') {
        let tests = tests?;
        let tests = core::str::from_utf8(&tests)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let tests = parse(tests.lines());

        let mut first = true;
        for test in tests {
            if highlight {
                if core::mem::take(&mut first) {
                    print!("[")
                } else {
                    print!(",")
                }
                print_test(&test);
                continue;
            }

            if run_test(&test)? {
                passed += 1
            } else {
                failed += 1
            };
        }

        if highlight {
            println!("]");
        }
    }

    if highlight {
        return Ok(());
    }

    let total = passed + failed;
    println!("{passed}/{total} tests passed");
    (failed == 0)
        .then_some(())
        .ok_or(io::Error::from(io::ErrorKind::Other))
}

type Test<S> = (Vec<S>, Vec<S>);

fn print_test((cmd, out_exp): &Test<&str>) {
    let tok = |typ, content| format!("{{\"t\": {typ:?}, \"c\": {content:?}}}");
    let out_exp = out_exp.iter().map(|s| (*s).to_owned() + "\n");
    print!(
        "[{}, {}, {}]",
        tok("prompt", "$ "),
        tok("command", &(cmd.join("\\\n") + "\n")),
        tok("output", &(out_exp.collect::<Vec<_>>().concat()))
    );
}

fn run_test((cmd, out_exp): &Test<&str>) -> io::Result<bool> {
    let cmd = cmd.join("");
    println!("Test: {cmd}");
    let (mut reader, writer) = io::pipe()?;
    let mut child = std::process::Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(writer.try_clone()?)
        .stderr(writer)
        .spawn()?;
    let mut out_rec = String::new();
    reader.read_to_string(&mut out_rec)?;
    // wait on the child to avoid zombies
    let exit = child.wait()?;
    if exit.success() && out_rec.lines().eq(out_exp.iter().map(|s| *s)) {
        Ok(true)
    } else {
        eprintln!("Fail: expected {out_exp:?}, received {out_rec}");
        Ok(false)
    }
}

fn parse<'a>(mut lines: impl Iterator<Item = &'a str>) -> impl Iterator<Item = Test<&'a str>> {
    let mut cmd = None;
    let mut out = Vec::new();

    core::iter::from_fn(move || {
        while let Some(line) = lines.next() {
            match line.strip_prefix("$ ") {
                Some(mut line) => {
                    let mut new_cmd = Vec::new();
                    while let Some(cont) = line.strip_suffix('\\') {
                        new_cmd.push(cont);
                        line = lines.next()?;
                    }
                    new_cmd.push(line);
                    if let Some(cmd) = cmd.replace(new_cmd) {
                        return Some((cmd, core::mem::take(&mut out)));
                    }
                }
                None => out.push(line),
            }
        }
        cmd.take().map(|cmd| (cmd, core::mem::take(&mut out)))
    })
}
