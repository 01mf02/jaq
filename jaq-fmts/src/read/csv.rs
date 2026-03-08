//! CSV support.
use bytes::Bytes;
use core::iter::Iterator;
use csv::{self, StringRecord};
use jaq_json::{Map, Num, Val};
use num_bigint::BigInt;
use std::io::Read;
use std::ops::Deref;
use std::str::FromStr;

fn convert_field(s: &str) -> Val {
    if let Ok(i) = s.parse() {
        Val::Num(Num::Int(i))
    } else if let Ok(bi) = BigInt::from_str(s) {
        Val::Num(Num::BigInt(bi.into()))
    } else if let Ok(f) = s.parse() {
        Val::Num(Num::Float(f))
    } else {
        Val::utf8_str(Vec::from(s))
    }
}

fn process_records(
    mut records: impl Iterator<Item = Result<StringRecord, csv::Error>>,
    has_header: bool,
) -> Result<Val, csv::Error> {
    if has_header {
        if let Some(fields) = records.next() {
            let fields: Vec<Val> = fields?.iter().map(Vec::from).map(Val::utf8_str).collect();
            let mut lines = Vec::new();
            for row in records {
                lines.push(Val::Obj(
                    fields
                        .iter()
                        .cloned()
                        .zip(row?.iter().map(convert_field))
                        .collect::<Map<Val, Val>>()
                        .into(),
                ))
            }
            Ok(Val::Arr(lines.into()))
        } else {
            Ok(Val::Arr(vec![].into()))
        }
    } else {
        let mut lines = Vec::new();
        for row in records {
            lines.push(Val::Arr(
                row?.iter().map(convert_field).collect::<Vec<Val>>().into(),
            ));
        }
        Ok(Val::Arr(lines.into()))
    }
}

/// Read a stream of CSV data.
pub fn read(b: impl Read, has_header: bool) -> Result<Val, csv::Error> {
    let mut reader = csv::ReaderBuilder::new().has_headers(false).from_reader(b);
    process_records(reader.records(), has_header)
}

/// Parse CSV data.
pub fn parse(b: &Bytes, has_header: bool) -> Result<Val, csv::Error> {
    read(b.deref(), has_header)
}
