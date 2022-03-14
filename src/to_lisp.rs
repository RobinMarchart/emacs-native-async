use std::{
    mem::{replace, take},
    ptr::null_mut,
    sync::{Arc, Mutex, RwLock},
};

use emacs::{Env, IntoLisp, Transfer, Value};
use libc::c_void;

pub struct ToLispConvert {
    inner: ToLispConvertInner,
}

enum ToLispConvertInner {
    Unit,
    I64(i64),
    ISize(isize),
    U64(u64),
    USize(usize),
    Bool(bool),
    F64(f64),
    Str(&'static str),
    String(String),
    Ptr(Option<unsafe extern "C" fn(arg1: *mut c_void)>, *mut c_void),
    Lazy(Option<Box<dyn Send + Sync + FnOnce(&Env) -> emacs::Result<Value> + Send + Sync>>),
}

unsafe impl Sync for ToLispConvertInner {}
unsafe impl Send for ToLispConvertInner {}

impl ToLispConvert {
    pub fn to_value(mut self, env: &Env) -> emacs::Result<Value> {
        match &mut self.inner {
            ToLispConvertInner::Unit => ().into_lisp(env),
            ToLispConvertInner::I64(v) => v.into_lisp(env),
            ToLispConvertInner::ISize(v) => v.into_lisp(env),
            ToLispConvertInner::U64(v) => v.into_lisp(env),
            ToLispConvertInner::USize(v) => v.into_lisp(env),
            ToLispConvertInner::Bool(v) => v.into_lisp(env),
            ToLispConvertInner::F64(v) => v.into_lisp(env),
            ToLispConvertInner::Str(v) => v.into_lisp(env),
            ToLispConvertInner::String(v) => take(v).into_lisp(env),
            ToLispConvertInner::Ptr(fin, val) => unsafe {
                env.make_user_ptr(take(fin), replace(val, null_mut()))
            },
            ToLispConvertInner::Lazy(f) => match take(f) {
                Some(f) => f(env),
                None => Err(anyhow::anyhow!("empty value")),
            },
        }
    }
    pub fn lazy<F: Send + Sync + 'static + FnOnce(&Env) -> emacs::Result<Value> + Send + Sync>(
        f: F,
    ) -> ToLispConvert {
        Self {
            inner: ToLispConvertInner::Lazy(Some(Box::new(f))),
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
            inner: ToLispConvertInner::Str(v),
        }
    }
}
impl From<String> for ToLispConvert {
    fn from(v: String) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::String(v),
        }
    }
}

//copied from Emacs crate
/// Finalizes an embedded pointer. This is called by the GC when it discards a `user-ptr`.
///
/// This function also serves as a form of runtime type tag, relying on Rust's mono-morphization.
unsafe extern "C" fn finalize<T: Transfer>(ptr: *mut c_void) {
    #[cfg(build = "debug")]
    println!("Finalizing {:#?} {}", ptr, T::type_name());
    drop(Box::from_raw(ptr as *mut T));
}

impl<T: Transfer + Send + Sync> From<Box<T>> for ToLispConvert {
    fn from(v: Box<T>) -> Self {
        ToLispConvert {
            inner: ToLispConvertInner::Ptr(
                Some(finalize::<T>),
                std::boxed::Box::<T>::into_raw(v).cast(),
            ),
        }
    }
}

impl<T: 'static+Send> From<Mutex<T>> for ToLispConvert {
    fn from(v: Mutex<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static+Send+Sync> From<RwLock<T>> for ToLispConvert {
    fn from(v: RwLock<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static+Send+Sync> From<Arc<T>> for ToLispConvert {
    fn from(v: Arc<T>) -> Self {
        Box::new(v).into()
    }
}

impl<'e> IntoLisp<'e> for ToLispConvert {
    fn into_lisp(self, env: &'e Env) -> emacs::Result<Value<'e>> {
        self.to_value(env)
    }
}

impl Drop for ToLispConvertInner {
    fn drop(&mut self) {
        match self {
            &mut Self::Ptr(Some(fin), val) => unsafe { fin(val) },
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ToLispConvert;

    #[allow(unused)]
    struct IsSendSync<T: Send + Sync + 'static> {
        t: T,
    }

    #[allow(unused)]
    fn test_is_send_sync(_: IsSendSync<ToLispConvert>) -> () {
        return ();
    }
}
