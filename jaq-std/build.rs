//! Cache parsed standard library.

#[cfg(feature = "bincode")]
fn main() {
    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let dest_path = std::path::Path::new(&out_dir).join("std.bin");
    let buffer = std::fs::File::create(dest_path).unwrap();

    let std = include_str!("src/std.jq");
    let (std, errs) = jaq_parse::parse(std, jaq_parse::defs());
    assert_eq!(errs, Vec::new());
    let std = std.unwrap();
    bincode::serialize_into(buffer, &std).unwrap();
}

#[cfg(not(feature = "bincode"))]
fn main() {}
