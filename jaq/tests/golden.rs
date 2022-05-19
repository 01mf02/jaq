use std::env;
use std::io;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use tempfile::NamedTempFile;

fn golden_test(name: &str, args: &[&str]) {
    if let Err(e) = golden_test_err(name, args) {
        println!("Error: {}", e);
    }
}
fn golden_test_err(name: &str, args: &[&str]) -> io::Result<()> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let in_path: PathBuf = format!("{}/tests/golden/{}/in.jsonl", manifest_dir, name).into();
    let out_path: PathBuf = format!("{}/tests/golden/{}/out.jsonl", manifest_dir, name).into();
    assert!(
        in_path.exists(),
        "Input should be at {}, but is missing",
        in_path.as_os_str().to_string_lossy()
    );
    assert!(
        out_path.exists(),
        "Output should be at {}, but is missing",
        out_path.as_os_str().to_string_lossy()
    );
    let out_temp_file = NamedTempFile::new()?;
    let jaq_path = env!("CARGO_BIN_EXE_jaq");
    let cat_child = Command::new("cat")
        .arg(in_path)
        .stdout(Stdio::piped())
        .spawn()?;
    let cat_stdout = cat_child.stdout.unwrap();
    let jaq_exit = Command::new(jaq_path)
        .args(args)
        .stdin(cat_stdout)
        .stdout(out_temp_file.reopen()?)
        .spawn()?
        .wait()?;
    assert!(jaq_exit.success());
    let out_ex = std::fs::read_to_string(out_path)?;
    let out_act = std::fs::read_to_string(out_temp_file.path())?;
    if out_ex.trim() != out_act.trim() {
        println!(
            "Expected output:\n{}\n---\nActual ouput:\n{}\n---",
            out_ex, out_act
        );
        std::process::exit(2);
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
