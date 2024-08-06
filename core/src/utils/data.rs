use std::{
    any::Any,
    fmt::{self, Debug, Display},
    str::FromStr,
};

use bstr::BString;
use serde::Serialize;
use serde_json::Value as JsonValue;
use serde_yaml::Value as YamlValue;

/* -------------------- *
 *         JSON         *
 * -------------------- */
#[derive(Debug)]
pub struct Json {
    value: JsonValue,
}

impl Serialize for Json {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Serialize::serialize(&self.value, serializer)
    }
}

impl FromStr for Json {
    type Err = String;

    #[inline]
    fn from_str(json_str: &str) -> Result<Self, Self::Err> {
        Self::decode(json_str.as_bytes())
    }
}

impl Json {
    #[inline]
    #[must_use]
    pub fn new(value: JsonValue) -> Self {
        Self { value }
    }

    #[inline]
    #[must_use]
    pub fn decode<T: AsRef<[u8]>>(source: T) -> Result<Self, String> {
        serde_json::from_slice(source.as_ref())
            .map(Self::new)
            .map_err(capitalize)
    }

    #[inline]
    #[must_use]
    pub fn encode<T>(value: &T) -> Result<BString, String>
    where
        T: ?Sized + Serialize,
    {
        serde_json::to_vec(value)
            .map(BString::new)
            .map_err(capitalize)
    }

    #[must_use]
    pub fn get<T: AsRef<str> + Debug>(&self, keys: &[T]) -> Result<&JsonValue, String> {
        let mut value = &self.value;

        for i in 0..keys.len() {
            let key = keys[i].as_ref();
            let trace = || gen_trace(keys, i);

            match value {
                JsonValue::Object(x) => {
                    if let Some(v) = x.get(key) {
                        value = v;
                    } else {
                        return Err(format!("No value found in object{}: {:?}", trace(), key));
                    }
                }
                JsonValue::Array(x) => {
                    let idx = key
                        .parse::<usize>()
                        .map_err(|_| format!("Invalid index for array{}: {:?}", trace(), key))?;

                    if let Some(v) = x.get(idx) {
                        value = v;
                    } else {
                        return Err(format!("No value found in array{}: {}", trace(), idx));
                    }
                }
                _ => {
                    let json_type = match value {
                        JsonValue::Null => "null",
                        JsonValue::Bool(_) => "bool",
                        JsonValue::Number(_) => "number",
                        JsonValue::String(_) => "string",
                        _ => unreachable!(),
                    };

                    return Err(format!(
                        "Cannot index value of type {:?}{}: {:?}",
                        json_type, trace(), key,
                    ));
                }
            }
        }

        Ok(value)
    }


}


/* -------------------- *
 *         YAML         *
 * -------------------- */
#[derive(Debug)]
pub struct Yaml {
    value: YamlValue,
}


/* -------------------- *
 *         UTILS        *
 * -------------------- */
fn capitalize<T: ToString>(x: T) -> String {
    let mut bytes = x.to_string().into_bytes();

    if !bytes.is_empty() {
        bytes[0] = bytes[0].to_ascii_uppercase();
    }

    unsafe { String::from_utf8_unchecked(bytes) }
}

fn gen_trace<T: Debug>(x: &[T], idx: usize) -> String {
    const TRACE_LIMIT: usize = 3;

    if idx > 0 {
        if idx > TRACE_LIMIT {
            format!(" at [{:?}, ..., {:?}]", &x[0], &x[idx - 1])
        } else {
            format!(" at {:?}", &x[..idx])
        }
    } else {
        String::new()
    }
}

#[allow(unused)]
fn parse_pointer(pointer: &str) -> Result<Vec<String>, String> {
    todo!()
}
