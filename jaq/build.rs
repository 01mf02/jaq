use std::process::Command;

fn main() {
    // Inject the abbreviated commit SHA as an environment variable
    let commit_sha = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_owned())
        .unwrap_or(String::from("unknown"));
    println!("cargo:rustc-env=COMMIT_SHA={}", commit_sha);
}
