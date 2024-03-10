//! Cache parsed standard library.

#[cfg(feature = "bincode")]
fn main() {
    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let std = include_str!("src/std.jq");

    {
        let dest_path = std::path::Path::new(&out_dir).join("std.bin");
        let buffer = std::fs::File::create(dest_path).unwrap();
        serialize_std(std, false, buffer);
    }
    #[cfg(feature = "unstable-flag")]
    {
        let dest_path = std::path::Path::new(&out_dir).join("std-unstable.bin");
        let buffer = std::fs::File::create(dest_path).unwrap();
        serialize_std(std, true, buffer);
    }
}

#[cfg(not(feature = "bincode"))]
fn main() {}

#[cfg(feature = "bincode")]
fn serialize_std(
    src: &str,
    #[cfg_attr(not(feature = "unstable-flag"), allow(unused))] unstable: bool,
    buffer: std::fs::File,
) {
    let (std, errs) = jaq_parse::parse(
        #[cfg(feature = "unstable-flag")]
        unstable,
        src,
        jaq_parse::defs(
            #[cfg(feature = "unstable-flag")]
            unstable,
        ),
    );
    assert_eq!(errs, Vec::new());
    let std = std.unwrap();
    bincode::serialize_into(buffer, &std).unwrap();
}
