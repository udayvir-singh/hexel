use std::{
    any::Any,
    fmt::{self, Debug, Display},
    str::FromStr,
};

use serde::Deserialize;
use serde_json::{
    from_str as decode_json,
    Value as JsonValue,
};
use serde_yaml::{
    from_str as decode_yaml,
    to_string as encode_yaml,
    Value as YamlValue,
};

/* -------------------- *
 *         JSON         *
 * -------------------- */
#[derive(Debug)]
pub struct Json {
    value: JsonValue,
}

impl Display for Json {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.value, f)
    }
}

// impl<'a, T: Deserialize<'a>> TryFrom<T> for Json {
    
// }

impl FromStr for Json {
    type Err = String;

    #[inline]
    fn from_str(json_str: &str) -> Result<Self, Self::Err> {
        decode_json(json_str).map(Self::new).map_err(capitalize)
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
    pub fn value(self) -> JsonValue {
        self.value
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

impl Display for Yaml {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Ok(x) = encode_yaml(&self.value) {
            write!(f, "{}", x)
        } else {
            write!(f, "<YAML UTF-8 ERROR>")
        }
    }
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
