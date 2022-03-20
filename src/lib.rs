use abi_stable::std_types::{RBoxError, RResult, ROption, RArc};
use emacs::Transfer;
use to_lisp::ToLispConvert;

pub mod to_lisp;

#[abi_stable::sabi_trait]
pub trait NotificationHandlerImpl:Send+Sync+'static{
    fn submit(&self,value:RResult<ToLispConvert,RBoxError>,id:i64)->RResult<(),RBoxError>;
    fn register(&self)->i64;
    #[sabi(last_prefix_field)]
    fn retrieve(&self,id:i64)->ROption<RResult<ToLispConvert,RBoxError>>;
}

pub type NotificationHandler=NotificationHandlerImpl_TO<RArc<()>>;

impl Transfer for NotificationHandler {}
