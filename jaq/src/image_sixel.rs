use jaq_json::Val;
use base64::{engine::general_purpose, Engine as _};
use std::io::Cursor;
use image::{DynamicImage, ImageReader};
use viuer::{print, Config};

/// Attempts to decode and recognize the image from the provided `value`.
/// Returns Some(DynamicImage) if decoding is successful, otherwise returns None.
pub fn decode_image(value: &Val) -> Option<DynamicImage> {
    if let Val::Str(s) = value {
        if let Ok(decoded) = general_purpose::STANDARD.decode(s.as_bytes()) {
            let cursor = Cursor::new(decoded);
            if let Ok(reader) = ImageReader::new(cursor).with_guessed_format() {
                if let Ok(img) = reader.decode() {
                    return Some(img);
                }
            }
        }
    }
    None
}

/// Prints an already decoded `DynamicImage` in the terminal using `viuer`, which automatically
/// tries to detect Sixel, Kitty, iTerm, or fallback modes.
pub fn print_image_with_sixel(img: &DynamicImage, x: u16) {
    // Ensure we start on a new line before printing the image
    println!();

    // Configure `viuer`.
    let config = Config {
        use_sixel: true,
        absolute_offset: false,
        x,
        ..Default::default()
    };

    // Display the image
    if let Err(e) = print(img, &config) {
        eprintln!("Failed to print image: {:?}", e);
    }
}
