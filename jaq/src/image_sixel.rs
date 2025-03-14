use jaq_json::Val;
use base64::{engine::general_purpose, Engine as _};
// We need to read from memory and guess the image format to support multiple formats
use std::io::Cursor;
use image::ImageReader;
use viuer::{print, Config};

/// Checks if the provided `value` is a string containing valid base64 data and guessable image format.
/// If it can be decoded and recognized by the `image` crate, we return `true`.
/// 
/// # TODO
/// - Consider additional checks for specific formats if needed.
/// - Possibly extend for metadata-based checks in the future.
pub fn is_image(value: &Val) -> bool {
    // We try to decode the base64, then guess the image format.
    // If it succeeds, we consider it an image.
    if let Val::Str(s) = value {
        if let Ok(decoded) = general_purpose::STANDARD.decode(s.as_bytes()) {
            let cursor = Cursor::new(decoded);
            if let Ok(reader) = ImageReader::new(cursor).with_guessed_format() {
                if reader.decode().is_ok() {
                    return true;
                }
            }
        }
    }
    false
}

/// Prints an image in the terminal using `viuer`, which automatically tries to detect
/// Sixel, Kitty, iTerm, or fallback modes. Supports various formats via the `image` crate.
/// 
/// # Future Directions / TODO:
/// - Allow user configuration of the printing backend (Sixel, Kitty, etc.).
/// - Possibly rename this function if more backends or configuration options become available.
pub fn print_image_with_sixel(value: &Val, x: u16) {
    if let Val::Str(s) = value {
        // Attempt to decode from base64
        match general_purpose::STANDARD.decode(s.as_bytes()) {
            Ok(decoded) => {
                // Ensure we start on a new line before printing the image
                println!();

                let cursor = Cursor::new(decoded);
                // Attempt to read the image from memory
                let reader = match ImageReader::new(cursor).with_guessed_format() {
                    Ok(r) => r,
                    Err(e) => {
                        eprintln!("Failed to guess image format: {}", e);
                        return;
                    }
                };
                // Decode the image into an in-memory representation
                let img = match reader.decode() {
                    Ok(i) => i,
                    Err(e) => {
                        eprintln!("Failed to decode image: {}", e);
                        return;
                    }
                };

                // Configure `viuer`. Setting `use_sixel` to `true` tries to display
                // with Sixel if the terminal supports it. `viuer` can also automatically
                // detect other capabilities like Kitty or iTerm.
                let config = Config {
                    use_sixel: true,
                    absolute_offset: false,
                    x: x,
                    ..Default::default()
                };

                // Display the image from the temporary file
                if let Err(e) = print(&img, &config) {
                    eprintln!("Failed to print image: {:?}", e);
                }

                // Ensure we continue on a new line after printing the image
                println!();
            }
            Err(_) => {
                eprintln!("Invalid base64 or non-image data.");
            }
        }
    } else {
        eprintln!("Value is not a string.");
    }
}
