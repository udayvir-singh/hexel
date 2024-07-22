// #![warn(missing_docs)]
#![allow(dead_code, unused_imports)]

pub mod lang;
pub mod utils;

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::iter::{Chain, Map};
use std::ops::{DerefMut, Mul};
use std::rc::Rc;
use std::slice::Iter;
use std::str::FromStr;
use std::sync::mpsc::channel;
use std::thread;

use crate::lang::{lexer::*, parser::*};
use crate::utils::{ast::*, consts::*, num::*, position::*, state::*, token::*};

macro_rules! v {($($x:expr),* $(,)?) => {vec![$($x.to_string(),)*]}}
macro_rules! vr {($($x:expr),* $(,)?) => {&v![$($x,)*]}}

// TODO: add module for data

// STD VARIABLES
// request:seq request:method request:url request:version
// script:dir script:name script:path

#[allow(unused)]
fn main() {
    println!("Hello, world!");

    // println!("{}", STANDARD_FUNCTIONS.iter().map(|x| x.0).collect::<Vec<_>>().join("\n"));

    // test_state();
    // test_parser();
    // test_lazy_parser();
}

#[allow(unused)]
fn test_state() {
    let mut foo = Rc::new(RefCell::new(vec![1]));
    let mut fr = foo.clone();
    let printer = move |x: String| {
        fr.borrow_mut().push(1);
        println!("{}", x);
    };
    let mut state = State::new(
        "/home/user/cool/hexel/".to_string(),
        Some(Box::new(printer)),
    );

    state.set_string("foo".to_string(), "hello".to_string());

    state.set_macro(
        "lol".to_string(),
        v!["x", "*y"],
        "print('lol')
        return 0"
            .to_string(),
    );

    state.set_macro(
        "test".to_string(),
        v!["x", "*y"],
        "local n = n and n + 1 or 0
        print('n = ' .. n)
        return n"
            .to_string(),
    );

    println!("r = {:#?}", state.eval("math:random", vr!["0", "2"]));
}

/*
yaml:get
yaml:minify
yaml:to_json
yaml:to_form
*/

#[allow(unused)]
fn test_parser() {
    let source = b"\
#var foo = ${math:div ${math:add 10 ${foo}}
                      ${test ${name}}}

#var foo = ${cond
             ${bool:and ${math:eq? ${foo} 10}
                        ${math:eq? ${baz} 1}} 10
             ;;
             ${bool:or ${math:eq? ${math:add ${lol} 10} 10}
                       ${math:eq? ${baz} 1}} 10
             ;;
             'default value'}

GET foo

; a comment
\n";

    let mut parser = Parser::new(source, "foo.hxl", None, None);

    for node in parser {
        println!("node = {:#?}", node);
    }
}

fn test_lexer() {
    let source = b"#require foo @{a:b x:}";

    let mut lexer = Lexer::new(source, "foo", None, None);
    let mut tokens = Vec::new();

    while lexer.active() {
        tokens.push(lexer.next_token(true, true))
    }

    println!("tokens = {:#?}", tokens)
}

#[allow(unused)]
fn test_lazy_parser() {
    let source1 = b"\
; 1234
;foo
#var a = 1
; baz
; tar
; yolo
";

    let source2 = b"\
; 1234
;foo
#var a = 2
; tar
; yolo";

    let old_tokens = Parser::new(source1, "foo", None, None).parse();

    let mut parser = LazyParser::new(source2, old_tokens, "foo");

    for (node, status) in parser {
        println!("node = {:#?}", node.to_string());
        println!("status = {:#?}", status);
        println!("position = {:#?}", node.position());
    }
}
