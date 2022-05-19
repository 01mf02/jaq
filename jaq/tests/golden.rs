use std::{env, fs, io, path, process};

fn golden_test(name: &str, args: &[&str]) {
    if let Err(e) = golden_test_err(name, args) {
        println!("Error: {}", e);
    }
}
fn golden_test_err(name: &str, args: &[&str]) -> io::Result<()> {
    let mut test_dir = path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    test_dir.extend(["tests", "golden", name]);
    let in_path = test_dir.join("in.jsonl");
    let out_path = test_dir.join("out.jsonl");
    assert!(
        in_path.exists(),
        "Input should be at {:?}, but is missing",
        in_path
    );
    assert!(
        out_path.exists(),
        "Output should be at {:?}, but is missing",
        out_path
    );

    let out_temp_file = tempfile::NamedTempFile::new()?;
    let jaq_path = env!("CARGO_BIN_EXE_jaq");
    let jaq_exit = process::Command::new(jaq_path)
        .args(args)
        .stdin(fs::File::open(in_path)?)
        .stdout(out_temp_file.reopen()?)
        .spawn()?
        .wait()?;
    assert!(jaq_exit.success());

    let out_ex = fs::read_to_string(out_path)?;
    let out_act = fs::read_to_string(out_temp_file.path())?;
    if out_ex.trim() != out_act.trim() {
        println!("Expected output:\n{}\n---", out_ex);
        println!("Actual output:\n{}\n---", out_act);
        process::exit(2);
    }
    Ok(())
}

#[test]
fn one() {
    golden_test("one", &["1"]);
}

#[test]
fn sparse() {
    golden_test("sparse", &["."]);
}

#[test]
fn compact() {
    golden_test("compact", &["-c", "."]);
}
