use std::io::{self, Read};

fn main() -> io::Result<()> {
    let stdin = io::read_to_string(io::stdin())?;
    let tests = parse(stdin.lines());
    let mut passed = 0;
    let mut failed = 0;
    for (cmd, out_exp) in tests {
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
            passed += 1;
        } else {
            eprintln!("Fail: expected {out_exp:?}, received {out_rec}");
            failed += 1;
        }
    }
    let total = passed + failed;
    println!("{passed}/{total} tests passed");
    (failed == 0)
        .then_some(())
        .ok_or(io::Error::from(io::ErrorKind::Other))
}

type Test<S> = (S, Vec<S>);

fn parse<'a>(mut lines: impl Iterator<Item = &'a str>) -> impl Iterator<Item = Test<&'a str>> {
    let mut cmd = None;
    let mut out = Vec::new();

    core::iter::from_fn(move || {
        for line in &mut lines {
            match line.strip_prefix("$ ") {
                Some(new_cmd) => {
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
