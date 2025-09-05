use jaq_json::{json, toml};

// TODO: test encoding!
#[test]
fn toml() {
    let json = include_bytes!("toml/test.json");
    let toml = include_str!("toml/test.toml");

    let json_val = json::parse_single(json).unwrap();
    let toml_val = toml::parse(toml).unwrap();

    assert_eq!(json_val, toml_val);
}
