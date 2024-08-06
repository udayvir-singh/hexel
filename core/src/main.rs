// #![warn(missing_docs)]
#![allow(dead_code, unused_imports)]

mod utils;

pub mod types;
pub mod lang;

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::iter::{Chain, Map};
use std::ops::{DerefMut, Mul};
use std::rc::Rc;
use std::slice::Iter;
use std::str::FromStr;
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::thread;

use crate::lang::{lexer::*, parser::*, collapser::*};
use crate::types::{ast::*, error::*, position::*, token::*};
use crate::utils::{consts::*, state::*};

macro_rules! v {($($x:expr),* $(,)?) => {vec![$($x.into(),)*]}}
macro_rules! vr {($($x:expr),* $(,)?) => {&v![$($x,)*]}}

// TODO: write collapser
// TODO: add module for data

// STD VARIABLES
// request:seq request:method request:url request:version
// script:dir script:name script:path

use bstr::{BString, ByteSlice, ByteVec};

#[allow(unused)]
#[tokio::main]
async fn main() {
    println!("Hello, world!");

    test_collapser();
    // test_state();
    // test_parser();
    // test_lazy_parser();
}

#[allow(unused)]
fn test_collapser() {
    let filepath = "/home/user/cool/hexel/main.hxl".to_string();
    let collapser = Collapser::new(filepath.clone());

    let source = b"\
#var foo = baz
";

    let rx = collapser.collapse(
        IterativeParser::new(source, filepath, None, None)
    );
}

#[allow(unused)]
fn test_state() {
    let mut foo = Rc::new(RefCell::new(vec![1]));
    let mut fr = foo.clone();

    let mut state = State::new(
        "/home/user/cool/hexel/".to_string(),
        Box::new(move |x: String| {
            fr.borrow_mut().push(1);
            println!("{}", x);
        })
    );

    state.set_string("foo".into(), "hello".into());

    state.set_macro(
        "lol".into(),
        v!["x", "*y"],
        "print('lol')
        return 0"
            .into(),
    );

    state.set_macro(
        "test".into(),
        v!["x", "*y"],
        "local n = n and n + 1 or 0
        print('n = ' .. n)
        return n"
            .into(),
    );

    println!("r = {:#?}", state.eval(
        &"str:slice".into(),
        vr!["a\u{AA}foo", "0", "3"]
    )
        // .map(|x| Vec::from(x))
    );
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
#var foo = ${cond
  ${bool:and ${math:eq? ${foo} 10}
             ${math:eq? ${baz} 1}}
  'first value'
  ;;
  ${bool:or ${math:eq? ${math:add ${lol} 10} 20}
            ${math:eq? ${baz} 1}}
  'second value'
  ;;
  'default value'
}

GET foo

; a comment
\n";

    let mut parser = Parser::new(source, "foo.hxl".to_string(), None, None);

    for node in parser {
        println!("node = {:#?}", node);
    }
}

fn test_lexer() {
    let source = b"#require foo @{a:b x:}";

    let mut lexer = Lexer::new(source, "foo".to_string(), None, None);
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

    let old_tokens = Parser::new(source1, "foo".to_string(), None, None).parse();

    let mut parser = LazyParser::new(source2, old_tokens, "foo".to_string());

    for (node, status) in parser {
        println!("node = {:#?}", node.to_string());
        println!("status = {:#?}", status);
        println!("position = {:#?}", node.position());
    }
}
