//! Cache parsed definitions.

use jaq_core::load;
use std::io::{Result, Write};

fn main() -> Result<()> {
    let defs = load::parse(include_str!("src/defs.jq"), |p| p.defs()).unwrap();
    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let dest_path = std::path::Path::new(&out_dir).join("defs.rs");
    write!(&mut std::fs::File::create(dest_path)?, "{defs:?}")
}
