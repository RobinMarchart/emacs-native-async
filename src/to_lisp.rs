use std::{
    cell::RefCell,
    panic::RefUnwindSafe,
    ptr::null_mut,
    sync::{Arc, Mutex, RwLock},
};

use abi_stable::{
    erased_types::TD_Opaque,
    sabi_trait,
    std_types::{RBox, RStr, RString},
    StableAbi,
};
use emacs::{func::HandleCall, Env, IntoLisp, Transfer, Value};
use libc::c_void;

#[derive(StableAbi)]
#[repr(C)]
pub struct ToLispConvert {
    inner: ToLispConvertInner,
}

#[derive(Debug, Clone)]
pub struct ToLispConversionError {
    message: String,
}
impl std::fmt::Display for ToLispConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.message.fmt(f)
    }
}

impl std::error::Error for ToLispConversionError {}

#[sabi_trait]
trait ToLispFn: 'static + Send {
    #[sabi(last_prefix_field)]
    fn convert(&mut self, env: *mut c_void) -> *mut c_void;
}

struct ToLispFnFromFn<F: Send + 'static + FnOnce(&Env) -> emacs::Result<Value> + Send> {
    fun: Option<F>,
}

struct UnwindSafeWrapper<'e> {
    inner: Result<Value<'e>, RefCell<Option<emacs::Error>>>,
}

impl<'e> RefUnwindSafe for UnwindSafeWrapper<'e> {}

impl<F: Send + 'static + FnOnce(&Env) -> emacs::Result<Value> + Send> ToLispFn
    for ToLispFnFromFn<F>
{
    fn convert(&mut self, env: *mut c_void) -> *mut c_void {
        let cenv = unsafe { emacs::CallEnv::new(Env::new(env.cast()), 0, null_mut()) };
        let val = UnwindSafeWrapper {
            inner: std::mem::take(&mut self.fun).unwrap()(&cenv).map_err(|e| RefCell::new(Some(e))),
        };
        cenv.handle_call(|_| {
            let val = &val;
            match &val.inner {
                Ok(v) => Ok(*v),
                Err(e) => Err(std::mem::take(&mut *e.borrow_mut()).unwrap()),
            }
        })
        .cast()
    }
}

#[derive(StableAbi)]
#[repr(u16)]
enum ToLispConvertInner {
    Unit,
    I64(i64),
    ISize(isize),
    U64(u64),
    USize(usize),
    Bool(bool),
    F64(f64),
    Str(RStr<'static>),
    String(RString),
    Lazy(ToLispFn_TO<RBox<()>>),
}

impl ToLispConvert {
    pub fn to_value(self, env: &Env) -> emacs::Result<Value> {
        match self.inner {
            ToLispConvertInner::Unit => ().into_lisp(env),
            ToLispConvertInner::I64(v) => v.into_lisp(env),
            ToLispConvertInner::ISize(v) => v.into_lisp(env),
            ToLispConvertInner::U64(v) => v.into_lisp(env),
            ToLispConvertInner::USize(v) => v.into_lisp(env),
            ToLispConvertInner::Bool(v) => v.into_lisp(env),
            ToLispConvertInner::F64(v) => v.into_lisp(env),
            ToLispConvertInner::Str(v) => v.into_lisp(env),
            ToLispConvertInner::String(v) => v.into_lisp(env),
            ToLispConvertInner::Lazy(mut f) => {
                Ok(unsafe { Value::new(f.convert(env.raw().cast()).cast(), env) })
            }
        }
    }

    pub fn lazy<F: Send + 'static + FnOnce(&Env) -> emacs::Result<Value> + Send>(
        f: F,
    ) -> ToLispConvert {
        Self {
            inner: ToLispConvertInner::Lazy(ToLispFn_TO::from_value(
                ToLispFnFromFn { fun: Some(f) },
                TD_Opaque,
            )),
        }
    }
}

impl From<()> for ToLispConvert {
    fn from(_: ()) -> Self {
        Self {
            inner: ToLispConvertInner::Unit,
        }
    }
}
impl From<i8> for ToLispConvert {
    fn from(v: i8) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::I64(v.into()),
        }
    }
}
impl From<i16> for ToLispConvert {
    fn from(v: i16) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::I64(v.into()),
        }
    }
}
impl From<i32> for ToLispConvert {
    fn from(v: i32) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::I64(v.into()),
        }
    }
}
impl From<i64> for ToLispConvert {
    fn from(v: i64) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::I64(v),
        }
    }
}
impl From<isize> for ToLispConvert {
    fn from(v: isize) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::ISize(v),
        }
    }
}
impl From<u8> for ToLispConvert {
    fn from(v: u8) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::U64(v.into()),
        }
    }
}
impl From<u16> for ToLispConvert {
    fn from(v: u16) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::U64(v.into()),
        }
    }
}
impl From<u32> for ToLispConvert {
    fn from(v: u32) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::U64(v.into()),
        }
    }
}
impl From<u64> for ToLispConvert {
    fn from(v: u64) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::U64(v),
        }
    }
}
impl From<usize> for ToLispConvert {
    fn from(v: usize) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::USize(v),
        }
    }
}
impl From<bool> for ToLispConvert {
    fn from(v: bool) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::Bool(v),
        }
    }
}
impl From<f64> for ToLispConvert {
    fn from(v: f64) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::F64(v),
        }
    }
}
impl From<&'static str> for ToLispConvert {
    fn from(v: &'static str) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::Str(v.into()),
        }
    }
}
impl From<String> for ToLispConvert {
    fn from(v: String) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::String(v.into()),
        }
    }
}

impl<T: Transfer + Send> From<Box<T>> for ToLispConvert {
    fn from(v: Box<T>) -> Self {
        ToLispConvert::lazy(move |env| v.into_lisp(env))
    }
}

impl<T: 'static + Send> From<Mutex<T>> for ToLispConvert {
    fn from(v: Mutex<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static + Send + Sync> From<RwLock<T>> for ToLispConvert {
    fn from(v: RwLock<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static + Send + Sync> From<Arc<T>> for ToLispConvert {
    fn from(v: Arc<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static + Send> From<RefCell<T>> for ToLispConvert {
    fn from(c: RefCell<T>) -> Self {
        Box::new(c).into()
    }
}

impl<'e> IntoLisp<'e> for ToLispConvert {
    fn into_lisp(self, env: &'e Env) -> emacs::Result<Value<'e>> {
        self.to_value(env)
    }
}

#[cfg(test)]
mod tests {
    use super::ToLispConvert;

    #[allow(unused)]
    struct IsSendSync<T: Send + 'static> {
        t: T,
    }

    #[allow(unused)]
    fn test_is_send_sync(_: IsSendSync<ToLispConvert>) {}
}
