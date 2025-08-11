use jaq_json::Val;
use std::io;
use std::iter;

pub fn bson_stream(bytes: &[u8]) -> impl Iterator<Item = io::Result<Val>> + '_ {
    let mut cursor = io::Cursor::new(bytes);
    iter::from_fn(move || {
        if cursor.position() == bytes.len() as u64 {
            return None;
        }

        match bson::Document::from_reader(&mut cursor) {
            Ok(doc) => {
                let bson_val = bson::Bson::Document(doc);
                let serde_val = bson_val.into_canonical_extjson();
                Some(Ok(Val::from(serde_val)))
            }
            Err(e) => {
                cursor.set_position(bytes.len() as u64);
                Some(Err(io::Error::new(io::ErrorKind::InvalidData, e)))
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use bson::{doc, oid::ObjectId, DateTime};
    use jaq_json::Val;
    use serde_json::json;

    #[test]
    fn test_bson_stream_simple() {
        let doc = doc! { "hello": "world" };
        let mut bson_bytes = Vec::new();
        doc.to_writer(&mut bson_bytes).unwrap();

        let mut vals = bson_stream(&bson_bytes);
        let val = vals.next().unwrap().unwrap();

        let expected_val = Val::from(json!({"hello": "world"}));
        assert_eq!(val, expected_val);
        assert!(vals.next().is_none());
    }

    #[test]
    fn test_bson_extended_types() {
        let id = ObjectId::new();
        let dt = DateTime::now();
        let doc = doc! { "_id": id, "date": dt };
        let mut bson_bytes = Vec::new();
        doc.to_writer(&mut bson_bytes).unwrap();

        let mut vals = bson_stream(&bson_bytes);
        let val = vals.next().unwrap().unwrap();

        let expected_val = Val::from(json!({
            "_id": { "$oid": id.to_hex() },
            "date": { "$date": { "$numberLong": dt.timestamp_millis().to_string() } }
        }));
        assert_eq!(val, expected_val);
    }
}
