//! todo

use std::collections::VecDeque;
use std::marker::Send;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use tokio::sync::mpsc::{self, UnboundedReceiver, UnboundedSender};
use tokio::task::JoinHandle;

use crate::types::{ast::*, error::*, position::*, token::*};
use crate::utils::{consts::*, state::*};

pub struct Collapser {
    filepath: String,
}

#[derive(Clone)]
struct Controller {
    sender: UnboundedSender<i32>,
    tasks: Arc<Mutex<VecDeque<JoinHandle<i32>>>>,
}

struct Context {
    filepath: String,
    state: State,
}

impl Controller {
    fn new(sender: UnboundedSender<i32>) -> Self {
        Self {
            sender,
            tasks: Default::default(),
        }
    }

    fn cleanup(&self) {
        for task in self.tasks.lock().unwrap().iter_mut() {
            task.abort();
        }
    }

    fn send(&self, x: i32) {
        if let Err(_) = self.sender.send(x) {
            self.cleanup();
        }
    }
}

impl Context {
    fn new(collapser: Collapser, controller: Controller) -> Self {
        let fileroot = collapser.filepath.clone();
        let state = State::new(fileroot, Box::new(move |msg| {
            controller.send(1);

            println!("MSG: {msg}");
        }));

        Self {
            filepath: collapser.filepath,
            state,
        }
    }
}

impl Collapser {
    pub fn new(filepath: String) -> Self {
        Self {
            filepath,
        }
    }

    #[allow(unused)]
    pub fn collapse(
        self,
        nodes: impl Iterator<Item = Arc<AstNode>> + Send + 'static,
    ) -> UnboundedReceiver<i32> {
        let (tx, rx) = mpsc::unbounded_channel();

        tokio::spawn(async move {
            let controller = Controller::new(tx);
            let context = Arc::new(Context::new(self, controller.clone()));

            for node in nodes {
                println!("{:?}", node);
            }
        });

        rx
    }
}
