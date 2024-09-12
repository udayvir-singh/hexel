//! todo

use std::{collections::HashMap, mem, rc::Rc, str::FromStr, sync::{Arc, RwLock}};

use bstr::{BStr, BString, ByteSlice};
use mlua::{
    Error as LuaError,
    Lua,
    LuaSerdeExt,
    Result as LuaResult,
    Scope as LuaScope,
    String as LuaString,
    Table as LuaTable,
    Value as LuaValue,
    Variadic as LuaVariadic,
};
use rand::{thread_rng, Rng};

use crate::types::err::*;
use super::{consts::*, num::*};

/* -------------------- *
 *         STATE        *
 * -------------------- */
/// Represents the state of a module that contains values associated with each identifier.
pub struct State {
    fileroot: String,
    printer: Box<dyn Fn(String)>,
    data: RwLock<HashMap<BString, StateValue>>,
}

#[derive(Debug)]
enum StateValue {
    String {
        value: BString,
    },
    Macro {
        params: Vec<BString>,
        body: BString,
        lua: Rc<Lua>,
    },
}

impl State {
    /* -------------------- *
     *        PUBLIC        *
     * -------------------- */
    /// todo
    #[inline]
    #[must_use]
    pub fn new(fileroot: String, printer: Box<dyn Fn(String)>) -> Self {
        Self {
            fileroot,
            printer,
            data: RwLock::default(),
        }
    }

    /// todo
    #[inline]
    pub fn set_string(&self, identifier: BString, value: BString) {
        self.data.write().unwrap().insert(identifier, StateValue::String { value });
    }

    /// todo
    #[inline]
    pub fn set_macro(&self, identifier: BString, params: Vec<BString>, body: BString) {
        let value = StateValue::Macro {
            params,
            body,
            lua: Rc::new(self.create_lua_runtime().unwrap()),
        };

        self.data.write().unwrap().insert(identifier, value);
    }

    /// todo
    pub fn merge(&self, other: Self, prefix: Option<&BString>) {
        let data = other.data.into_inner().unwrap();

        if let Some(p) = prefix {
            let iter = data.into_iter().map(|(x, y)| (Self::add_prefix(x, p), y));

            self.data.write().unwrap().extend(iter);
        } else {
            self.data.write().unwrap().extend(data);
       }
    }

    /// todo
    pub fn take(&self, other: &Self, mut identifier: BString, prefix: Option<&BString>) -> Result<(), ()> {
        if let Some(value) = other.data.write().unwrap().remove(&identifier) {
            if let Some(p) = prefix {
                identifier = Self::add_prefix(identifier, p);
            }

            self.data.write().unwrap().insert(identifier, value);
            Ok(())
        } else {
            Err(())
        }
    }

    /// todo
    pub fn eval(
        &self,
        identifier: &BString,
        args: &[BString],
    ) -> Result<BString, (ErrorCode, Option<String>)> {
        // early return if identifier name is invalid
        if !Self::is_identifier_name(identifier) {
            return Err((ErrorCode::CC01, None));
        }

        // try getting value from state
        let data_gaurd = self.data.read().unwrap();

        match data_gaurd.get(identifier) {
            // clone string value
            Some(StateValue::String { value }) if args.is_empty() => {
                Ok(value.clone())
            }
            // handle invalid arguments on string value
            Some(StateValue::String { .. }) => {
                Err((ErrorCode::CC03, None))
            }
            // evaluate lua macro
            Some(StateValue::Macro { params, body, lua })
                if args.len() >= params.iter().filter(|x| x[0] != b'*').count() =>
            {
                let body = body.clone();
                let lua = lua.clone();

                // set macro arguments
                Self::set_macro_args(&lua, params, args).unwrap();

                // drop read gaurd on data
                drop(data_gaurd);

                // run macro
                lua.scope(|scope| {
                    let data = unsafe { mem::transmute(&self.data) };
                    let printer = unsafe { mem::transmute(self.printer.as_ref()) };

                    Self::inject_state_api(&lua, scope, printer, data)?;

                    lua.load(body.as_bytes())
                        .set_name(format!("[macro {}]", identifier))
                        .call::<_, BString>(())
                }).map_err(|err| {
                    let mut desc = format!("lua {}", err);

                    while desc.ends_with('\n') {
                        desc.pop();
                    }

                    (ErrorCode::CC05, Some(desc))
                })
            }
            // handle missing arguments for macro
            Some(StateValue::Macro { params, .. }) => {
                let missing_args = params[args.len()..]
                    .iter()
                    .filter(|x| x[0] != b'*')
                    .map(|x| format!("{:?}", x.strip_prefix(&[b'+']).unwrap_or(x).as_bstr()))
                    .collect::<Vec<String>>();

                let desc = format!(
                    "Missing argument{0} for parameter{0}: {1}",
                    if missing_args.len() > 1 {"s"} else {""},
                    missing_args.join(", "),
                );

                Err((ErrorCode::CC04, Some(desc)))
            }
            // fallback to standard functions
            None => match STANDARD_FUNCTIONS.iter().find(|x| x.0 == identifier) {
                // evaluate standard function
                Some((_, params, func)) => {
                    if args.len() >= params.len() {
                        // call function
                        func(args).map_err(|err| (ErrorCode::CC07, Some(err)))
                    } else {
                        // handle missing arguments
                        let missing_args = params[args.len()..]
                            .iter()
                            .map(|x| format!("{x:?}"))
                            .collect::<Vec<String>>()
                            .join(", ");

                        let desc = format!(
                            "Missing argument{0} for parameter{0}: {1}",
                            if params.len() - args.len() > 1 {"s"} else {""},
                            missing_args,
                        );

                        Err((ErrorCode::CC06, Some(desc)))
                    }
                }
                // handle missing value
                None => {
                    Err((ErrorCode::CC02, None))
                }
            }
        }
    }


    /* -------------------- *
     *         UTILS        *
     * -------------------- */
    fn is_atom(x: &[u8]) -> bool {
        !x.is_empty() && !x.iter().any(|byte| {
            matches!(
                byte,
                b'\0'..=b'\x1F' | b'\x7F' | b' ' | b'\\' | b'\'' | b'"' |
                b'=' | b':' | b';' | b'$' | b'@' | b'#' | b'{' | b'}'
            )
        })
    }

    fn is_identifier_name(x: &[u8]) -> bool {
        x.split(|&x| x == b':').all(Self::is_atom)
    }

    fn add_prefix(mut identifier: BString, prefix: &BString) -> BString {
        let mut bytes = Vec::with_capacity(prefix.len() + 1 + identifier.len());

        bytes.extend_from_slice(prefix);
        bytes.push(b':');
        bytes.append(&mut identifier);

        BString::new(bytes)
    }

    fn debug_lua_sequence(table: LuaTable, max_width: usize) -> String {
        let mut string = String::from("{");

        for value in table.sequence_values::<LuaValue>().map(Result::unwrap) {
            if string.len() > 1 {
                string.push_str(", ");
            }

            if string.len() >= max_width {
                string.push_str("...");
                break;
            }

            string.push_str(&match value {
                LuaValue::Integer(x) => x.to_string(),
                LuaValue::Number(x) => x.to_string(),
                LuaValue::Boolean(x) => x.to_string(),
                LuaValue::String(x) => format!("{:?}", x),
                x => format!("<{}: 0x{:x}>", x.type_name(), x.to_pointer() as usize),
            });
        }

        string.push('}');

        string
    }

    fn parse_lua_identifer(identifier: LuaValue, err_heading: &str) -> LuaResult<BString> {
        let parse_string = |string: &LuaString| {
            let bytes = string.as_bytes();

            if Self::is_identifier_name(bytes) {
                Ok(bytes.into())
            } else {
                Err(())
            }
        };

        let parse_table = |table: LuaTable| {
            let mut vec = Vec::new();

            for value in table.sequence_values::<BString>() {
                match value {
                    Ok(xs) if Self::is_atom(&xs) => {
                        vec.push(xs);
                    }
                    _ => {
                        return Err(());
                    }
                }
            }

            if vec.len() > 0 {
                Ok(bstr::join(":", vec).into())
            } else {
                Err(())
            }
        };

        match identifier {
            LuaValue::String(string) => {
                parse_string(&string).map_err(|_| {
                    let msg = format!(
                        "{}:\n\tInvalid identifier name: {:?}",
                        err_heading,
                        string,
                    );

                    LuaError::RuntimeError(msg)
                })
            }
            LuaValue::Table(table) => {
                parse_table(table.clone()).map_err(|_| {
                    let msg = format!(
                        "{}:\n\tInvalid identifier name: {}",
                        err_heading,
                        Self::debug_lua_sequence(table, 40),
                    );

                    LuaError::RuntimeError(msg)
                })
            }
            _ => {
                let msg = format!(
                    "{}:\n\tInvalid type for identifier name: {:?}",
                    err_heading,
                    identifier.type_name(),
                );

                Err(LuaError::RuntimeError(msg))
            }
        }
    }


    /* -------------------- *
     *          LUA         *
     * -------------------- */
    fn set_macro_args(
        lua: &Lua,
        params: &[BString],
        args: &[BString],
    ) -> LuaResult<()> {
        let globals = lua.globals();

        // set macro arguments
        let mut idx = 0;

        for param in params {
            if let b'*' | b'+' = param[0] {
                let range_end = args.len() - (params.len() - 1 - idx);

                globals.set(&param[1..], &args[idx..range_end])?;

                idx = range_end;
            } else {
                globals.set(param.as_bstr(), args[idx].as_bstr())?;

                idx += 1;
            }
        }

        Ok(())
    }

    fn inject_state_api(
        lua: &Lua,
        scope: &LuaScope,
        printer: &'static dyn Fn(String),
        data: &'static RwLock<HashMap<BString, StateValue>>,
    ) -> LuaResult<()> {
        let globals = lua.globals();

        // inject printer
        let print = scope.create_function(move |_, args: LuaVariadic<LuaValue>| {
            let mut xs = Vec::with_capacity(args.len());

            for value in args {
                xs.push(value.to_string()?);
            }

            printer(xs.join("\t"));
            Ok(())
        })?;

        globals.set("print", print)?;

        // inject state getter
        let get = scope.create_function(move |lua, identifier: LuaValue| {
            let err_heading = "Invalid argument to 'get' function";
            let identifier = Self::parse_lua_identifer(identifier, err_heading)?;

            match data.read().unwrap().get(&identifier) {
                // clone string value
                Some(StateValue::String { value }) => {
                    let lua_str = lua.create_string(value)?;

                    Ok(LuaValue::String(lua_str))
                }
                // add lua wrapper around macro
                Some(StateValue::Macro { params, body, lua: macro_lua }) => {
                    let params = params.clone();
                    let body = body.clone();
                    let macro_lua = macro_lua.clone();

                    // create macro wrapper
                    let lua_func = lua.create_function(move |_, args: LuaVariadic<BString>| {
                        if args.len() >= params.iter().filter(|x| x[0] != b'*').count() {
                            // evaluate macro
                            macro_lua.scope(|macro_scope| {
                                Self::set_macro_args(&macro_lua, &params, &args)?;
                                Self::inject_state_api(&macro_lua, macro_scope, printer, data)?;

                                macro_lua
                                    .load(body.as_bytes())
                                    .set_name(format!("[macro {}]", identifier))
                                    .call::<_, String>(())
                            })
                        } else {
                            // handle missing arguments
                            let missing_args = params[args.len()..]
                                .iter()
                                .filter(|x| x[0] != b'*')
                                .map(|x| format!("{:?}", x.strip_prefix(&[b'+']).unwrap_or(x).as_bstr()))
                                .collect::<Vec<String>>();

                            let msg = format!(
                                concat!(
                                    "Error while reading arguments for Lua macro: {:?}",
                                    "\n\tMissing required argument{1} for parameter{1}: {2}",
                                ),
                                identifier,
                                if missing_args.len() > 1 {"s"} else {""},
                                missing_args.join(", "),
                            );

                            Err(LuaError::RuntimeError(msg))
                        }
                    })?;

                    Ok(LuaValue::Function(lua_func))
                }
                // fallback to standard functions
                None => match STANDARD_FUNCTIONS.iter().find(|x| x.0 == identifier) {
                    // add lua wrapper around standard function
                    Some((_, params, func)) => {
                        let lua_func = lua.create_function(move |_, args: LuaVariadic<BString>| {
                            if args.len() >= params.len() {
                                // call function
                                func(&args).map_err(|err| {
                                    let msg = format!(
                                        "Error while calling standard function: {:?}\n\t{}",
                                        identifier,
                                        err.replace('\n', "\n\t"),
                                    );

                                    LuaError::RuntimeError(msg)
                                })
                            } else {
                                // handle missing arguments
                                let msg = format!(
                                    concat!(
                                        "Error while reading arguments for standard function: {:?}",
                                        "\n\tMissing required argument{1} for parameter{1}: {2}",
                                    ),
                                    identifier,
                                    if params.len() - args.len() > 1 {"s"} else {""},
                                    params[args.len()..].join(", "),
                                );

                                Err(LuaError::RuntimeError(msg))
                            }
                        })?;

                        Ok(LuaValue::Function(lua_func))
                    }
                    // handle missing value
                    None => {
                        let msg = format!(
                            "{}:\n\tNo value found for identifier: {:?}",
                            err_heading,
                            identifier,
                        );

                        Err(LuaError::RuntimeError(msg))
                    }
                }
            }
        })?;

        globals.set("get", get)?;

        // inject state setter
        let set = scope.create_function(|_, (identifier, value): (LuaValue, BString)| {
            let err_heading = "Invalid argument to 'set' function";
            let identifier = Self::parse_lua_identifer(identifier, err_heading)?;

            data.write()
                .unwrap()
                .insert(identifier, StateValue::String { value });

            Ok(())
        })?;

        globals.set("set", set)?;

        Ok(())
    }

    fn create_lua_runtime(&self) -> LuaResult<Lua> {
        let lua = Lua::new();
        let globals = lua.globals();

        // create inspect function
        let inspect = lua.create_function(|_, value: LuaValue| {
            println!("{:#?}", value);
            Ok(())
        })?;

        globals.set("inspect", inspect)?;

        // create json helpers
        let json_encode = lua.create_function(|_, value: LuaValue| {
            serde_json::to_string(&value)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))
        })?;

        let json_decode = lua.create_function(|lua, xs: String| {
            let json_value = serde_json::from_str::<serde_json::Value>(&xs)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))?;

            lua.to_value(&json_value)
        })?;

        let json = lua.create_table_from([
            ("encode", LuaValue::Function(json_encode)),
            ("decode", LuaValue::Function(json_decode)),
        ])?;

        globals.set("json", json)?;

        // create json5 helpers
        let json5_encode = lua.create_function(|_, value: LuaValue| {
            json5::to_string(&value)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))
        })?;

        let json5_decode = lua.create_function(|lua, xs: String| {
            let json_value = json5::from_str::<serde_json::Value>(&xs)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))?;

            lua.to_value(&json_value)
        })?;

        let json5 = lua.create_table_from([
            ("encode", LuaValue::Function(json5_encode)),
            ("decode", LuaValue::Function(json5_decode)),
        ])?;

        globals.set("json5", json5)?;

        // create yaml helpers
        let yaml_encode = lua.create_function(|_, value: LuaValue| {
            serde_yaml::to_string(&value)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))
        })?;

        let yaml_decode = lua.create_function(|lua, xs: String| {
            let yaml_value = serde_yaml::from_str::<serde_yaml::Value>(&xs)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))?;

            lua.to_value(&yaml_value)
        })?;

        let yaml = lua.create_table_from([
            ("encode", LuaValue::Function(yaml_encode)),
            ("decode", LuaValue::Function(yaml_decode)),
        ])?;

        globals.set("yaml", yaml)?;

        // create form helpers
        let form_encode = lua.create_function(|_, value: LuaValue| {
            serde_urlencoded::to_string(&value)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))
        })?;

        let form_decode = lua.create_function(|lua, xs: String| {
            let form_value = serde_urlencoded::from_str::<HashMap<String, String>>(&xs)
                .map_err(|e| LuaError::RuntimeError(e.to_string()))?;

            lua.to_value(&form_value)
        })?;

        let form = lua.create_table_from([
            ("encode", LuaValue::Function(form_encode)),
            ("decode", LuaValue::Function(form_decode)),
        ])?;

        globals.set("form", form)?;

        drop(globals);

        Ok(lua)
    }
}


/* -------------------- *
 *       STANDARD       *
 * -------------------- */
macro_rules! count {
    () => (0);
    ($x:tt $($e:tt)*) => (1 + count!($($e)*));
}

macro_rules! standard_functions {
    ($($id:literal => ($($arg:ident),* $(,)?) = $fn:expr)+) => {
        /// An array containing Hexel's standard functions.
        ///
        /// Each tuple contains the following:
        /// - The function's identifier.
        /// - The function's parameters.
        /// - The function itself.
        pub static STANDARD_FUNCTIONS: [
            (&str, &[&str], fn(args: &[BString]) -> Result<BString, String>);
            count!($($id)*)
        ] = [
            $(($id, &[$(stringify!($arg),)*], $fn),)*
        ];
    };
}

standard_functions! {
    /* -------------------- *
     *        BOOLEAN       *
     * -------------------- */
    "bool:not" => (boolean) = |args| {
        let boolean = parse_boolean(&args[0], "boolean")?;

        Ok((!boolean).to_bstring())
    }

    "bool:and" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok(booleans.into_iter().all(|x| x).to_bstring())
    }

    "bool:or" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok(booleans.into_iter().any(|x| x).to_bstring())
    }

    "bool:nand" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok((!booleans.into_iter().all(|x| x)).to_bstring())
    }

    "bool:nor" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok((!booleans.into_iter().any(|x| x)).to_bstring())
    }


    /* -------------------- *
     *        STRING        *
     * -------------------- */
    "str:empty?" => (string) = |args| {
        let string = &args[0];

        Ok(string.is_empty().to_bstring())
    }

    "str:eq?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] == x[1]).to_bstring())
    }

    "str:lt?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] < x[1]).to_bstring())
    }

    "str:le?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] <= x[1]).to_bstring())
    }

    "str:gt?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] > x[1]).to_bstring())
    }

    "str:ge?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] >= x[1]).to_bstring())
    }

    "str:oneof?" => (string, pattern) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(patterns.iter().any(|x| x == string).to_bstring())
    }

    "str:prefix?" => (string, pattern) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(patterns.iter().any(|x| string.starts_with(x)).to_bstring())
    }

    "str:suffix?" => (string, pattern) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(patterns.iter().any(|x| string.ends_with(x)).to_bstring())
    }

    "str:len" => (string) = |args| {
        let string = &args[0];

        Ok(string.chars().count().to_bstring())
    }

    "str:blen" => (string) = |args| {
        let string = &args[0];

        Ok(string.len().to_bstring())
    }

    "str:upper" => (string) = |args| {
        let string = &args[0];

        Ok(string.to_uppercase().into())
    }

    "str:lower" => (string) = |args| {
        let string = &args[0];

        Ok(string.to_lowercase().into())
    }

    "str:concat" => (string1, string2) = |args| {
        Ok(args.concat().into())
    }

    "str:join" => (separator, string1, string2) = |args| {
        let separator = &args[0];
        let strings = &args[1..];

        Ok(bstr::join(separator, strings).into())
    }

    "str:repeat" => (string, count) = |args| {
        let string = &args[0];
        let count = parse_clamped_integer(&args[1], "count")?;

        Ok(string.repeat(count.max(0) as usize).into())
    }

    "str:pick" => (string1, string2) = |args| {
        Ok(args.iter().find(|x| !x.is_empty()).cloned().unwrap_or_default())
    }

    "str:slice" => (string, start) = |args| {
        let string = &args[0];
        let start = parse_index(&args[1], string.len(), "end")?;
        let end = if let Some(arg) = args.get(2) {
            parse_index(arg, string.len(),"end")?
        } else {
            usize::MAX
        };

        Ok(slice(string, start, end))
    }

    "str:replace" => (string, pattern, replacement) = |args| {
        let string = &args[0];
        let pattern = &args[1];
        let replacement = &args[2];
        let count = if let Some(arg) = args.get(3) {
            parse_clamped_integer(arg, "count")?.max(0) as usize
        } else {
            usize::MAX
        };

        Ok(string.replacen(pattern, replacement, count).into())
    }

    "str:trim-start" => (string) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(trim_start(string.as_bstr(), patterns).into())
    }

    "str:trim-end" => (string) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(trim_end(string.as_bstr(), patterns).into())
    }

    "str:trim" => (string) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(trim(string.as_bstr(), patterns).into())
    }


    /* -------------------- *
     *         MATH         *
     * -------------------- */
    "math:e" => () = |args| {
        let scale = parse_scale(args.get(0), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(parse_constant(BIG_E, scale))
    }

    "math:pi" => () = |args| {
        let scale = parse_scale(args.get(0), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(parse_constant(BIG_PI, scale))
    }

    "math:pos?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_positive().to_bstring())
    }

    "math:neg?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_negative().to_bstring())
    }

    "math:int?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_integer().to_bstring())
    }

    "math:zero?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_zero().to_bstring())
    }

    "math:one?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_one().to_bstring())
    }

    "math:eq?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] == x[1]).to_bstring())
    }

    "math:lt?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] < x[1]).to_bstring())
    }

    "math:le?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] <= x[1]).to_bstring())
    }

    "math:gt?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] > x[1]).to_bstring())
    }

    "math:ge?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] >= x[1]).to_bstring())
    }

    "math:oneof?" => (number, pattern) = |args| {
        let number = parse_number(&args[0], "number")?;
        let patterns = parse_numbers(&args[1..], "pattern")?;

        Ok(patterns.into_iter().any(|x| x == number).to_bstring())
    }

    "math:min" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(Number::min).unwrap().to_bstring())
    }

    "math:max" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(Number::max).unwrap().to_bstring())
    }

    "math:clamp" => (number, min, max) = |args| {
        let number = parse_number(&args[0], "number")?;
        let min = parse_number(&args[1], "min")?;
        let max = parse_number(&args[2], "max")?;

        Ok(number.clamp(min, max).to_bstring())
    }

    "math:neg" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok((-number).to_bstring())
    }

    "math:abs" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.abs().to_bstring())
    }

    "math:signum" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.signum().to_bstring())
    }

    "math:floor" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.floor().to_bstring())
    }

    "math:ceil" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.ceil().to_bstring())
    }

    "math:round" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;
        let mode = match args.get(2).map(|x| unsafe { x.to_str_unchecked() }).unwrap_or("half-up") {
            "up" => RoundingMode::Up,
            "down" => RoundingMode::Down,
            "floor" => RoundingMode::Floor,
            "ceil" => RoundingMode::Ceiling,
            "half-up" => RoundingMode::HalfUp,
            "half-down" => RoundingMode::HalfDown,
            "half-even" => RoundingMode::HalfEven,
            x => {
                let msg = format!("Invalid rounding mode: {:?}", x);

                return Err(arg_error("mode", msg));
            }
        };

        Ok(number.round(scale, mode).to_bstring())
    }

    "math:add" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x + y).unwrap().to_bstring())
    }

    "math:sub" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x - y).unwrap().to_bstring())
    }

    "math:mul" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x * y).unwrap().to_bstring())
    }

    "math:rem" => (number1, number2) = |args| {
        let numbers = parse_div_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x % y).unwrap().to_bstring())
    }

    "math:div" => (number1, number2) = |args| {
        let numbers = parse_div_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x / y).unwrap().to_bstring())
    }

    "math:xdiv" => (scale, number1, number2) = |args| {
        let scale = parse_integer(&args[0], "scale")?;
        let numbers = parse_div_numbers(&args[1..], "number")?;

        Ok(
            numbers
                .into_iter()
                .reduce(|x, y| x.div_with_scale(y, scale).unwrap())
                .unwrap()
                .to_bstring()
        )
    }

    "math:pow" => (number1, number2) = |args| {
        let numbers = parse_div_numbers(args, "number")?;

        Ok(
            numbers
                .into_iter()
                .reduce(|x, y| x.pow(y, DEFAULT_HARD_MATH_SCALE))
                .unwrap()
                .to_bstring()
        )
    }

    "math:xpow" => (scale, number1, number2) = |args| {
        let scale = parse_integer(&args[0], "scale")?;
        let numbers = parse_div_numbers(&args[1..], "number")?;

        Ok(
            numbers
                .into_iter()
                .reduce(|x, y| x.pow(y, scale))
                .unwrap()
                .to_bstring()
        )
    }

    "math:inv" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        number
            .inverse(scale)
            .map(|x| x.to_bstring())
            .ok_or_else(|| arg_error(
                "number",
                "Cannot calculate inverse of zero".to_string(),
            ))
    }

    "math:inc" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok((number + Number::one()).to_bstring())
    }

    "math:dec" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok((number - Number::one()).to_bstring())
    }

    "math:square" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.square().to_bstring())
    }

    "math:sqrt" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        number
            .sqrt(scale)
            .map(|x| x.to_bstring())
            .ok_or_else(|| arg_error(
                "number",
                format!("Cannot calculate sqrt of negative number: {:?}", &args[0]),
            ))
    }

    "math:cube" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.cube().to_bstring())
    }

    "math:cbrt" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(number.cbrt(scale).to_bstring())
    }

    "math:exp" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        Ok(number.exp(scale).to_bstring())
    }

    "math:ln" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        number.ln(scale).map(|x| x.to_bstring())
    }

    "math:log" => (number, base) = |args| {
        let number = parse_number(&args[0], "number")?;
        let base = parse_number(&args[1], "number")?;
        let scale = parse_scale(args.get(2), DEFAULT_HARD_MATH_SCALE)?;

        number.log(base, scale).map(|x| x.to_bstring())
    }

    "math:rad" => (degrees) = |args| {
        let degrees = parse_number(&args[0], "degrees")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(degrees.rad(scale).to_bstring())
    }

    "math:deg" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(radians.deg(scale).to_bstring())
    }

    "math:sin" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        Ok(radians.sin(scale).to_bstring())
    }

    "math:cos" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        Ok(radians.cos(scale).to_bstring())
    }

    "math:tan" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        radians
            .tan(scale)
            .map(|x| x.to_bstring())
            .ok_or_else(|| format!(
                "Tangent is not defined at pi/2 + n*pi: {:?}",
                &args[0],
            ))
    }

    "math:random" => (lbound, rbound) = |args| {
        let lbound = parse_number(&args[0], "lbound")?;
        let rbound = parse_number(&args[1], "rbound")?;

        lbound.random(&rbound).map(|x| x.to_bstring())
    }
}


/* -------------------- *
 *         UTILS        *
 * -------------------- */
trait ToBString {
    #[must_use]
    fn to_bstring(&self) -> BString;
}

impl<T: ToString> ToBString for T {
    #[inline]
    fn to_bstring(&self) -> BString {
        self.to_string().into()
    }
}

fn arg_error(arg_name: &str, msg: String) -> String {
    format!("Error while parsing argument {:?}:\n{}", arg_name, msg)
}


/* -------------------- *
 *     BOOLEAN UTILS    *
 * -------------------- */
fn parse_boolean(arg: &BString, arg_name: &str) -> Result<bool, String> {
    match unsafe { arg.to_str_unchecked() } {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(arg_error(
            arg_name,
            format!("Invalid value for a boolean: {:?}", arg),
        )),
    }
}

fn parse_booleans(args: &[BString], basename: &str) -> Result<Vec<bool>, String> {
    let mut booleans = Vec::with_capacity(args.len());

    for (arg, n) in args.iter().zip(1..) {
        booleans.push(parse_boolean(arg, &format!("{}{}", basename, n))?);
    }

    Ok(booleans)
}


/* -------------------- *
 *     STRING UTILS     *
 * -------------------- */
fn trim_start<'a>(mut string: &'a BStr, patterns: &[BString]) -> &'a BStr {
    if patterns.is_empty() {
        return string.trim_start().as_bstr();
    }

    while let Some(xs) = patterns
        .iter()
        .find_map(|p| string.strip_prefix(p.as_bytes()))
    {
        string = xs.as_bstr();
    }

    string
}

fn trim_end<'a>(mut string: &'a BStr, patterns: &[BString]) -> &'a BStr {
    if patterns.is_empty() {
        return string.trim_end().as_bstr();
    }

    while let Some(xs) = patterns
        .iter()
        .find_map(|p| string.strip_suffix(p.as_bytes()))
    {
        string = xs.as_bstr();
    }

    string
}

fn trim<'a>(string: &'a BStr, patterns: &[BString]) -> &'a BStr {
    trim_start(trim_end(string, patterns), patterns)
}

fn slice(string: &BString, start: usize, end: usize) -> BString {
    if start >= end {
        return BString::default();
    }

    let mut iter = string.char_indices();
    let idx = iter
        .nth(start)
        .map(|x| x.0)
        .unwrap_or(string.len());
    let jdx = iter
        .nth(end.saturating_sub(start + 1))
        .map(|x| x.0)
        .unwrap_or(string.len());

    string.get(idx..jdx).unwrap_or_default().into()
}


/* -------------------- *
 *     NUMBER UTILS     *
 * -------------------- */
fn parse_constant(raw: &str, scale: i64) -> BString {
    let int_digits = raw.bytes().take_while(|x| *x != b'.').count();

    if scale <= -(int_digits as i64) {
        "0".into()
    } else if scale < 0 {
        [
            &raw[..int_digits - scale.abs() as usize],
            "0".repeat(scale.abs() as usize).as_str(),
        ].concat().into()
    } else {
        raw[..(int_digits + (scale.min(1) + scale) as usize).min(raw.len())].into()
    }
}

fn parse_number(arg: &BString, arg_name: &str) -> Result<Number, String> {
    Number::from_bytes(arg).map_err(|err| arg_error(arg_name, err))
}

fn parse_numbers(args: &[BString], basename: &str) -> Result<Vec<Number>, String> {
    let mut numbers = Vec::with_capacity(args.len());

    for (arg, n) in args.iter().zip(1..) {
        numbers.push(parse_number(arg, &format!("{}{}", basename, n))?);
    }

    Ok(numbers)
}

fn parse_div_numbers(args: &[BString], basename: &str) -> Result<Vec<Number>, String> {
    let mut numbers = Vec::with_capacity(args.len());

    for (arg, n) in args.iter().zip(1..) {
        let arg_name = &format!("{}{}", basename, n);
        let number = parse_number(arg, arg_name)?;

        if n > 1 && number.is_zero() {
            return Err(arg_error(arg_name, "Cannot divide by zero".to_string()));
        }

        numbers.push(number);
    }

    Ok(numbers)
}

fn parse_integer(arg: &BString, arg_name: &str) -> Result<i64, String> {
    Number::from_bytes(arg)
        .and_then(|x| x.to_integer())
        .map_err(|err| arg_error(arg_name, err))
}

fn parse_clamped_integer(arg: &BString, arg_name: &str) -> Result<i64, String> {
    Number::from_bytes(arg)
        .and_then(|x| x.to_clamped_integer())
        .map_err(|err| arg_error(arg_name, err))
}

fn parse_index(arg: &BString, len: usize, arg_name: &str) -> Result<usize, String> {
    let int = parse_clamped_integer(arg, arg_name)?;

    if int.is_negative() {
        Ok(len.saturating_sub(int.abs() as usize))
    } else {
        Ok(int as usize)
    }
}

fn parse_scale(scale: Option<&BString>, default: i64) -> Result<i64, String> {
    scale
        .map(|x| parse_integer(x, "scale"))
        .transpose()
        .map(|x| x.unwrap_or(default))
}
