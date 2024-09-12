use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

use super::ast::AstNode;

/* -------------------- *
 *       RESPONSE       *
 * -------------------- */
#[derive(Debug, Clone)]
pub struct Response {}


/* -------------------- *
 *        MESSAGE       *
 * -------------------- */
#[derive(Debug, Clone)]
pub enum Message {
    Print(String),
    Debug(String),
    Response(Response),
    Error(AstNode),
}


/* -------------------- *
 *         MPSC         *
 * -------------------- */
pub type MessageSender = UnboundedSender<Message>;
pub type MessageReceiver = UnboundedReceiver<Message>;

#[inline(always)]
pub fn message_channel() -> (MessageSender, MessageReceiver) {
    unbounded_channel()
}
