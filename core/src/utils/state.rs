//! todo

use std::{mem, collections::HashMap, rc::Rc, str::FromStr, sync::RwLock};

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

use super::{consts::*, error::*, num::*};

/* -------------------- *
 *         STATE        *
 * -------------------- */
/// Represents the state of a module that contains values associated with each identifier.
pub struct State {
    fileroot: String,
    printer: Option<Box<dyn Fn(String)>>,
    data: RwLock<HashMap<String, StateValue>>,
}

#[derive(Debug)]
enum StateValue {
    String {
        value: String,
    },
    Macro {
        params: Vec<String>,
        body: String,
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
    pub fn new(fileroot: String, printer: Option<Box<dyn Fn(String)>>) -> Self {
        Self {
            fileroot,
            printer,
            data: RwLock::default(),
        }
    }

    /// todo
    #[inline]
    pub fn set_string(&self, identifier: String, value: String) {
        self.data.write().unwrap().insert(identifier, StateValue::String {value});
    }

    /// todo
    #[inline]
    pub fn set_macro(&self, identifier: String, params: Vec<String>, body: String) {
        let value = StateValue::Macro {
            params,
            body,
            lua: Rc::new(self.create_lua_runtime().unwrap()),
        };

        self.data.write().unwrap().insert(identifier, value);
    }

    /// todo
    pub fn merge(&self, other: Self, prefix: Option<&str>) {
        let data = other.data.into_inner().unwrap();

        if let Some(p) = prefix {
            let iter = data.into_iter().map(|(x, y)| (format!("{}:{}", p, x), y));

            self.data.write().unwrap().extend(iter);
        } else {
            self.data.write().unwrap().extend(data);
       }
    }

    /// todo
    pub fn take(&self, other: &Self, mut identifier: String, prefix: Option<&str>) -> Result<(), ()> {
        if let Some(value) = other.data.write().unwrap().remove(&identifier) {
            if let Some(p) = prefix {
                identifier = format!("{}:{}", p, identifier);
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
        identifier: &str,
        args: &[String],
    ) -> Result<String, (ErrorCode, Option<String>)> {
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
                if args.len() >= params.iter().filter(|x| !x.starts_with('*')).count() =>
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
                    let printer = self.printer
                        .as_ref()
                        .map(|x| unsafe { mem::transmute(x.as_ref()) });

                    Self::inject_state_api(&lua, scope, printer, data)?;

                    lua.load(body)
                        .set_name(format!("[macro {}]", identifier))
                        .eval::<String>()
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
                    .filter(|x| !x.starts_with('*'))
                    .map(|x| format!("{:?}", x.strip_prefix('+').unwrap_or(x)))
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
    fn is_atom(x: &str) -> bool {
        !x.is_empty() && !x.bytes().any(|byte| {
            matches!(
                byte,
                b'\0'..=b'\x1F' | b'\x7F' | b' ' | b'\\' | b'\'' | b'"' |
                b'=' | b':' | b';' | b'$' | b'@' | b'#' | b'{' | b'}'
            )
        })
    }

    fn is_identifier_name(x: &str) -> bool {
        x.split(':').all(Self::is_atom)
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

    fn parse_lua_identifer(identifier: LuaValue, err_heading: &str) -> LuaResult<String> {
        let parse_string = |string: &LuaString| {
            match string.to_str() {
                Ok(xs) if Self::is_identifier_name(xs) => {
                    Ok(xs.to_string())
                }
                _ => {
                    Err(())
                }
            }
        };

        let parse_table = |table: LuaTable| {
            let mut vec = Vec::new();

            for value in table.sequence_values::<String>() {
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
                Ok(vec.join(":"))
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
        params: &[String],
        args: &[String],
    ) -> LuaResult<()> {
        let globals = lua.globals();

        // set macro arguments
        let mut idx = 0;

        for param in params {
            if let b'*' | b'+' = param.as_bytes()[0] {
                let range_end = args.len() - (params.len() - 1 - idx);

                globals.set(&param[1..], &args[idx..range_end])?;

                idx = range_end;
            } else {
                globals.set(param.as_str(), args[idx].as_str())?;

                idx += 1;
            }
        }

        Ok(())
    }

    fn inject_state_api(
        lua: &Lua,
        scope: &LuaScope,
        printer: Option<&'static dyn Fn(String)>,
        data: &'static RwLock<HashMap<String, StateValue>>,
    ) -> LuaResult<()> {
        let globals = lua.globals();

        // inject printer
        if let Some(func) = printer {
            let print = scope.create_function(move |_, args: LuaVariadic<LuaValue>| {
                let mut xs = Vec::with_capacity(args.len());

                for value in args {
                    xs.push(value.to_string()?);
                }

                func(xs.join("\t"));
                Ok(())
            })?;

            globals.set("print", print)?;
        }

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
                    let lua_func = lua.create_function(move |_, args: LuaVariadic<String>| {
                        if args.len() >= params.iter().filter(|x| !x.starts_with('*')).count() {
                            // evaluate macro
                            macro_lua.scope(|macro_scope| {
                                Self::set_macro_args(&macro_lua, &params, &args)?;
                                Self::inject_state_api(&macro_lua, macro_scope, printer, data)?;

                                macro_lua
                                    .load(&body)
                                    .set_name(format!("[macro {}]", identifier))
                                    .eval::<String>()
                            })
                        } else {
                            // handle missing arguments
                            let missing_args = params[args.len()..]
                                .iter()
                                .filter(|x| !x.starts_with('*'))
                                .map(|x| format!("{:?}", x.strip_prefix('+').unwrap_or(x)))
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
                        let lua_func = lua.create_function(move |_, args: LuaVariadic<String>| {
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
        let set = scope.create_function(|_, (identifier, value): (LuaValue, String)| {
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
            (&str, &[&str], fn(args: &[String]) -> Result<String, String>);
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

        Ok((!boolean).to_string())
    }

    "bool:and" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok(booleans.into_iter().all(|x| x).to_string())
    }

    "bool:or" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok(booleans.into_iter().any(|x| x).to_string())
    }

    "bool:nand" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok((!booleans.into_iter().all(|x| x)).to_string())
    }

    "bool:nor" => (boolean1, boolean2) = |args| {
        let booleans = parse_booleans(args, "boolean")?;

        Ok((!booleans.into_iter().any(|x| x)).to_string())
    }


    /* -------------------- *
     *        STRING        *
     * -------------------- */
    "str:empty?" => (string) = |args| {
        let string = &args[0];

        Ok(string.is_empty().to_string())
    }

    "str:eq?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] == x[1]).to_string())
    }

    "str:lt?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] < x[1]).to_string())
    }

    "str:le?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] <= x[1]).to_string())
    }

    "str:gt?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] > x[1]).to_string())
    }

    "str:ge?" => (string1, string2) = |args| {
        Ok(args.windows(2).all(|x| x[0] >= x[1]).to_string())
    }

    "str:oneof?" => (string, pattern) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(patterns.iter().any(|x| x == string).to_string())
    }

    "str:prefix?" => (string, pattern) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(patterns.iter().any(|x| string.starts_with(x)).to_string())
    }

    "str:suffix?" => (string, pattern) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(patterns.iter().any(|x| string.ends_with(x)).to_string())
    }

    "str:len" => (string) = |args| {
        let string = &args[0];

        Ok(string.chars().count().to_string())
    }

    "str:blen" => (string) = |args| {
        let string = &args[0];

        Ok(string.len().to_string())
    }

    "str:upper" => (string) = |args| {
        let string = &args[0];

        Ok(string.to_uppercase())
    }

    "str:lower" => (string) = |args| {
        let string = &args[0];

        Ok(string.to_lowercase())
    }

    "str:concat" => (string1, string2) = |args| {
        Ok(args.concat())
    }

    "str:join" => (separator, string1, string2) = |args| {
        let separator = &args[0];
        let strings = &args[1..];

        Ok(strings.join(separator))
    }

    "str:repeat" => (string, count) = |args| {
        let string = &args[0];
        let count = parse_clamped_integer(&args[1], "count")?;

        Ok(string.repeat(count.max(0) as usize))
    }

    "str:pick" => (string1, string2) = |args| {
        Ok(args.iter().find(|x| !x.is_empty()).cloned().unwrap_or_default())
    }

    "str:min" => (string1, string2) = |args| {
        Ok(extract(args, |x, y| x < y))
    }

    "str:pmin" => (string1, string2) = |args| {
        Ok(extract_non_empty(args, |x, y| x < y))
    }

    "str:rmin" => (string1, string2) = |args| {
        Ok(extract(args, |x, y| x <= y))
    }

    "str:rpmin" => (string1, string2) = |args| {
        Ok(extract_non_empty(args, |x, y| x <= y))
    }

    "str:max" => (string1, string2) = |args| {
        Ok(extract(args, |x, y| x > y))
    }

    "str:pmax" => (string1, string2) = |args| {
        Ok(extract_non_empty(args, |x, y| x > y))
    }

    "str:rmax" => (string1, string2) = |args| {
        Ok(extract(args, |x, y| x >= y))
    }

    "str:rpmax" => (string1, string2) = |args| {
        Ok(extract_non_empty(args, |x, y| x >= y))
    }

    "str:slice" => (string, start) = |args| {
        let string = &args[0];
        let start = parse_index(string, &args[1], "end")?;
        let end = if let Some(arg) = args.get(2) {
            parse_index(string, arg, "end")?
        } else {
            usize::MAX
        };

        Ok(string.chars().skip(start).take(end.saturating_sub(start)).collect())
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

        Ok(string.replacen(pattern, replacement, count).to_string())
    }

    "str:trim-start" => (string) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(trim_start(string, patterns).to_string())
    }

    "str:trim-end" => (string) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(trim_end(string, patterns).to_string())
    }

    "str:trim" => (string) = |args| {
        let string = &args[0];
        let patterns = &args[1..];

        Ok(trim(string, patterns).to_string())
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

        Ok(number.is_positive().to_string())
    }

    "math:neg?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_negative().to_string())
    }

    "math:int?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_integer().to_string())
    }

    "math:zero?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_zero().to_string())
    }

    "math:one?" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.is_one().to_string())
    }

    "math:eq?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] == x[1]).to_string())
    }

    "math:lt?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] < x[1]).to_string())
    }

    "math:le?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] <= x[1]).to_string())
    }

    "math:gt?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] > x[1]).to_string())
    }

    "math:ge?" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.windows(2).all(|x| x[0] >= x[1]).to_string())
    }

    "math:oneof?" => (number, pattern) = |args| {
        let number = parse_number(&args[0], "number")?;
        let patterns = parse_numbers(&args[1..], "pattern")?;

        Ok(patterns.into_iter().any(|x| x == number).to_string())
    }

    "math:min" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(Number::min).unwrap().to_string())
    }

    "math:max" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(Number::max).unwrap().to_string())
    }

    "math:clamp" => (number, min, max) = |args| {
        let number = parse_number(&args[0], "number")?;
        let min = parse_number(&args[1], "min")?;
        let max = parse_number(&args[2], "max")?;

        Ok(number.clamp(min, max).to_string())
    }

    "math:neg" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok((-number).to_string())
    }

    "math:abs" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.abs().to_string())
    }

    "math:signum" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.signum().to_string())
    }

    "math:floor" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.floor().to_string())
    }

    "math:ceil" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.ceil().to_string())
    }

    "math:round" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;
        let mode = match args.get(2).map(String::as_str).unwrap_or("half-up") {
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

        Ok(number.round(scale, mode).to_string())
    }

    "math:add" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x + y).unwrap().to_string())
    }

    "math:sub" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x - y).unwrap().to_string())
    }

    "math:mul" => (number1, number2) = |args| {
        let numbers = parse_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x * y).unwrap().to_string())
    }

    "math:rem" => (number1, number2) = |args| {
        let numbers = parse_div_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x % y).unwrap().to_string())
    }

    "math:div" => (number1, number2) = |args| {
        let numbers = parse_div_numbers(args, "number")?;

        Ok(numbers.into_iter().reduce(|x, y| x / y).unwrap().to_string())
    }

    "math:xdiv" => (scale, number1, number2) = |args| {
        let scale = parse_integer(&args[0], "scale")?;
        let numbers = parse_div_numbers(&args[1..], "number")?;

        Ok(
            numbers
                .into_iter()
                .reduce(|x, y| x.div_with_scale(y, scale).unwrap())
                .unwrap()
                .to_string()
        )
    }

    "math:pow" => (number1, number2) = |args| {
        let numbers = parse_div_numbers(args, "number")?;

        Ok(
            numbers
                .into_iter()
                .reduce(|x, y| x.pow(y, DEFAULT_HARD_MATH_SCALE))
                .unwrap()
                .to_string()
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
                .to_string()
        )
    }

    "math:inv" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        number
            .inverse(scale)
            .map(|x| x.to_string())
            .ok_or_else(|| arg_error(
                "number",
                "Cannot calculate inverse of zero".to_string(),
            ))
    }

    "math:inc" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok((number + Number::one()).to_string())
    }

    "math:dec" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok((number - Number::one()).to_string())
    }

    "math:square" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.square().to_string())
    }

    "math:sqrt" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        number
            .sqrt(scale)
            .map(|x| x.to_string())
            .ok_or_else(|| arg_error(
                "number",
                format!("Cannot calculate sqrt of negative number: {:?}", &args[0]),
            ))
    }

    "math:cube" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;

        Ok(number.cube().to_string())
    }

    "math:cbrt" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(number.cbrt(scale).to_string())
    }

    "math:exp" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        Ok(number.exp(scale).to_string())
    }

    "math:ln" => (number) = |args| {
        let number = parse_number(&args[0], "number")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        number.ln(scale).map(|x| x.to_string())
    }

    "math:log" => (number, base) = |args| {
        let number = parse_number(&args[0], "number")?;
        let base = parse_number(&args[1], "number")?;
        let scale = parse_scale(args.get(2), DEFAULT_HARD_MATH_SCALE)?;

        number.log(base, scale).map(|x| x.to_string())
    }

    "math:rad" => (degrees) = |args| {
        let degrees = parse_number(&args[0], "degrees")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(degrees.rad(scale).to_string())
    }

    "math:deg" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_NORMAL_MATH_SCALE)?;

        Ok(radians.deg(scale).to_string())
    }

    "math:sin" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        Ok(radians.sin(scale).to_string())
    }

    "math:cos" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        Ok(radians.cos(scale).to_string())
    }

    "math:tan" => (radians) = |args| {
        let radians = parse_number(&args[0], "radians")?;
        let scale = parse_scale(args.get(1), DEFAULT_HARD_MATH_SCALE)?;

        radians
            .tan(scale)
            .map(|x| x.to_string())
            .ok_or_else(|| format!(
                "Tangent is not defined at pi/2 + n*pi: {:?}",
                &args[0],
            ))
    }

    "math:random" => (lbound, rbound) = |args| {
        let lbound = parse_number(&args[0], "lbound")?;
        let rbound = parse_number(&args[1], "rbound")?;

        lbound.random(&rbound).map(|x| x.to_string())
    }


    /* -------------------- *
     *         UUID         *
     * -------------------- */
    /* -------------------- *
     *         JSON         *
     * -------------------- */
    /* -------------------- *
     *         JSON5        *
     * -------------------- */
    /* -------------------- *
     *         YAML         *
     * -------------------- */
    /* -------------------- *
     *         FORM         *
     * -------------------- */

}


/* -------------------- *
 *         UTILS        *
 * -------------------- */
#[allow(unused)]
pub(crate) fn test(args: &[String]) -> Result<String, String> {
    todo!()
}

fn arg_error(arg_name: &str, msg: String) -> String {
    format!("Error while parsing argument {:?}:\n{}", arg_name, msg)
}


/* -------------------- *
 *     BOOLEAN UTILS    *
 * -------------------- */
fn parse_boolean(arg: &str, arg_name: &str) -> Result<bool, String> {
    match arg {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(arg_error(
            arg_name,
            format!("Invalid value for a boolean: {:?}", arg),
        )),
    }
}

fn parse_booleans(args: &[String], basename: &str) -> Result<Vec<bool>, String> {
    let mut booleans = Vec::with_capacity(args.len());

    for (arg, n) in args.iter().zip(1..) {
        booleans.push(parse_boolean(arg, &format!("{}{}", basename, n))?);
    }

    Ok(booleans)
}


/* -------------------- *
 *     STRING UTILS     *
 * -------------------- */
fn extract(args: &[String], cond: fn(usize, usize) -> bool) -> String {
    args
        .iter()
        .reduce(|y, x| if cond(x.len(), y.len()) { x } else { y })
        .unwrap()
        .clone()
}

fn extract_non_empty(args: &[String], cond: fn(usize, usize) -> bool) -> String {
    args
        .iter()
        .filter(|x| !x.is_empty())
        .reduce(|y, x| if cond(x.len(), y.len()) { x } else { y })
        .cloned()
        .unwrap_or_default()
}

fn trim_start<'a>(mut string: &'a str, patterns: &[String]) -> &'a str {
    if patterns.is_empty() {
        return string.trim_start();
    }

    while let Some(xs) = patterns.iter().find_map(|p| string.strip_prefix(p)) {
        string = xs;
    }

    string
}

fn trim_end<'a>(mut string: &'a str, patterns: &[String]) -> &'a str {
    if patterns.is_empty() {
        return string.trim_end();
    }

    while let Some(xs) = patterns.iter().find_map(|p| string.strip_suffix(p)) {
        string = xs;
    }

    string
}

fn trim<'a>(string: &'a str, patterns: &[String]) -> &'a str {
    trim_start(trim_end(string, patterns), patterns)
}


/* -------------------- *
 *     NUMBER UTILS     *
 * -------------------- */
fn parse_constant(raw: &str, scale: i64) -> String {
    let int_digits = raw.bytes().take_while(|x| *x != b'.').count();

    if scale <= -(int_digits as i64) {
        "0".to_string()
    } else if scale < 0 {
        [
            &raw[..int_digits - scale.abs() as usize],
            "0".repeat(scale.abs() as usize).as_str(),
        ].concat()
    } else {
        raw[..(int_digits + (scale.min(1) + scale) as usize).min(raw.len())].to_string()
    }
}

fn parse_number(arg: &str, arg_name: &str) -> Result<Number, String> {
    Number::from_str(arg).map_err(|err| arg_error(arg_name, err))
}

fn parse_numbers(args: &[String], basename: &str) -> Result<Vec<Number>, String> {
    let mut numbers = Vec::with_capacity(args.len());

    for (arg, n) in args.iter().zip(1..) {
        numbers.push(parse_number(arg, &format!("{}{}", basename, n))?);
    }

    Ok(numbers)
}

fn parse_div_numbers(args: &[String], basename: &str) -> Result<Vec<Number>, String> {
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

fn parse_integer(arg: &str, arg_name: &str) -> Result<i64, String> {
    Number::from_str(arg)
        .and_then(|x| x.to_integer())
        .map_err(|err| arg_error(arg_name, err))
}

fn parse_clamped_integer(arg: &str, arg_name: &str) -> Result<i64, String> {
    Number::from_str(arg)
        .and_then(|x| x.to_clamped_integer())
        .map_err(|err| arg_error(arg_name, err))
}

fn parse_index(string: &str, arg: &str, arg_name: &str) -> Result<usize, String> {
    let int = parse_clamped_integer(arg, arg_name)?;

    if int.is_negative() {
        Ok(string.len().saturating_sub(int.abs() as usize))
    } else {
        Ok(int as usize)
    }
}

fn parse_scale(scale: Option<&String>, default: i64) -> Result<i64, String> {
    scale
        .map(|x| parse_integer(x, "scale"))
        .transpose()
        .map(|x| x.unwrap_or(default))
}
