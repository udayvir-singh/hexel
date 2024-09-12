//! todo

use std::collections::VecDeque;
use std::marker::Send;
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};

use tokio::task::JoinHandle;

use crate::types::{ast::*, err::*, pos::*, token::*, msg::*};
use crate::utils::{consts::*, state::*};

/* -------------------- *
 *       COLLAPSER      *
 * -------------------- */
pub struct Collapser {
    filepath: String,
}

impl Collapser {
    /* -------------------- *
     *        PUBLIC        *
     * -------------------- */
    pub fn new(filepath: String) -> Self {
        Self {
            filepath,
        }
    }

    #[allow(unused)]
    pub fn collapse<T: Iterator<Item = Arc<AstNode>> + Send + 'static>(
        self,
        nodes: T,
    ) -> MessageReceiver {
        let (sender, receiver) = message_channel();

        tokio::spawn(async move {
            let mut controller = Controller::new(sender.clone());
            let context = Context::new(self, sender);

            for node in nodes {
                match *node {
                    AstNode::Statement(ref statement, _) => {
                        context.collapse_statement(statement)
                    }
                    AstNode::Request(ref request, _) => {
                        controller.queue(context.clone().collapse_request(request))
                    }
                    _ => {}
                }
            }
        });

        receiver
    }
}


/* -------------------- *
 *      CONTROLLER      *
 * -------------------- */
struct Controller {
    sender: MessageSender,
    tasks: VecDeque<JoinHandle<Response>>,
}

impl Controller {
    fn new(sender: MessageSender) -> Self {
        Self {
            sender,
            tasks: Default::default(),
        }
    }

    fn close(&mut self) {
        for task in &mut self.tasks {
            task.abort();
        }
    }

    fn send(&mut self, msg: Message) {
        if let Err(_) = self.sender.send(msg) {
            self.close();
        }
    }

    fn queue(&mut self, mut tasks: VecDeque<JoinHandle<Response>>) {
        self.tasks.append(&mut tasks)
    }

    async fn dequeue_finished(&mut self) {
        while let Some(x) = self.tasks.front() {
            if !x.is_finished() {
                return;
            }

            let res = self.tasks.pop_front().unwrap().await.unwrap();

            self.send(Message::Response(res));
        }
    }
}


/* -------------------- *
 *        CONTEXT       *
 * -------------------- */
#[derive(Clone)]
struct Context {
    filepath: Arc<String>,
    state: Arc<State>,
}

impl Context {
    /* -------------------- *
     *          NEW         *
     * -------------------- */
    fn new(collapser: Collapser, sender: MessageSender) -> Self {
        let fileroot = collapser.filepath.clone();
        let state = Arc::new(State::new(fileroot, Box::new(move |msg| {
            let _ = sender.send(Message::Print(msg));
        })));

        Self {
            filepath: Arc::new(collapser.filepath),
            state,
        }
    }


    /* -------------------- *
     *       STATEMENT      *
     * -------------------- */
    #[allow(unused)]
    fn collapse_statement(&self, statement: &Statement) {
        println!("statement = {:#?}", statement);
        todo!()
    }


    /* -------------------- *
     *        REQUEST       *
     * -------------------- */
    #[allow(unused)]
    fn collapse_request(self, request: &Request) -> VecDeque<JoinHandle<Response>> {
        todo!()
    }


    /* -------------------- *
     *         TOKEN        *
     * -------------------- */
}
