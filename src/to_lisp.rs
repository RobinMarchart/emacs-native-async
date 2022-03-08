use std::sync::{Arc, Mutex, RwLock};

use emacs::{Env, IntoLisp, Transfer, Value};
use libc::c_void;

pub enum ToLispConvert {
    Unit,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    Bool(bool),
    F64(f64),
    Str(&'static str),
    String(String),
    Ptr(Option<unsafe extern "C" fn(arg1: *mut c_void)>, *mut c_void),
}
impl ToLispConvert {
    pub fn to_value(self, env: &Env) -> emacs::Result<Value> {
        match self {
            ToLispConvert::Unit => ().into_lisp(env),
            ToLispConvert::I8(v) => v.into_lisp(env),
            ToLispConvert::I16(v) => v.into_lisp(env),
            ToLispConvert::I32(v) => v.into_lisp(env),
            ToLispConvert::I64(v) => v.into_lisp(env),
            ToLispConvert::ISize(v) => v.into_lisp(env),
            ToLispConvert::U8(v) => v.into_lisp(env),
            ToLispConvert::U16(v) => v.into_lisp(env),
            ToLispConvert::U32(v) => v.into_lisp(env),
            ToLispConvert::U64(v) => v.into_lisp(env),
            ToLispConvert::USize(v) => v.into_lisp(env),
            ToLispConvert::Bool(v) => v.into_lisp(env),
            ToLispConvert::F64(v) => v.into_lisp(env),
            ToLispConvert::Str(v) => v.into_lisp(env),
            ToLispConvert::String(v) => v.into_lisp(env),
            ToLispConvert::Ptr(fin, val) => unsafe { env.make_user_ptr(fin, val) },
        }
    }
}

impl From<()> for ToLispConvert {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}
impl From<i8> for ToLispConvert {
    fn from(v: i8) -> Self {
        Self::I8(v)
    }
}
impl From<i16> for ToLispConvert {
    fn from(v: i16) -> Self {
        Self::I16(v)
    }
}
impl From<i32> for ToLispConvert {
    fn from(v: i32) -> Self {
        Self::I32(v)
    }
}
impl From<i64> for ToLispConvert {
    fn from(v: i64) -> Self {
        Self::I64(v)
    }
}
impl From<isize> for ToLispConvert {
    fn from(v: isize) -> Self {
        Self::ISize(v)
    }
}
impl From<u8> for ToLispConvert {
    fn from(v: u8) -> Self {
        Self::U8(v)
    }
}
impl From<u16> for ToLispConvert {
    fn from(v: u16) -> Self {
        Self::U16(v)
    }
}
impl From<u32> for ToLispConvert {
    fn from(v: u32) -> Self {
        Self::U32(v)
    }
}
impl From<u64> for ToLispConvert {
    fn from(v: u64) -> Self {
        Self::U64(v)
    }
}
impl From<usize> for ToLispConvert {
    fn from(v: usize) -> Self {
        Self::USize(v)
    }
}
impl From<bool> for ToLispConvert {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}
impl From<f64> for ToLispConvert {
    fn from(v: f64) -> Self {
        Self::F64(v)
    }
}
impl From<&'static str> for ToLispConvert {
    fn from(v: &'static str) -> Self {
        Self::Str(v)
    }
}
impl From<String> for ToLispConvert {
    fn from(v: String) -> Self {
        Self::String(v)
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

impl<T: Transfer> From<Box<T>> for ToLispConvert {
    fn from(v: Box<T>) -> Self {
        Self::Ptr(
            Some(finalize::<T>),
            std::boxed::Box::<T>::into_raw(v).cast(),
        )
    }
}

impl<T: 'static> From<Mutex<T>> for ToLispConvert {
    fn from(v: Mutex<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static> From<RwLock<T>> for ToLispConvert {
    fn from(v: RwLock<T>) -> Self {
        Box::new(v).into()
    }
}
impl<T: 'static> From<Arc<T>> for ToLispConvert {
    fn from(v: Arc<T>) -> Self {
        Box::new(v).into()
    }
}
impl<'e> IntoLisp<'e> for ToLispConvert {
    fn into_lisp(self, env: &'e Env) -> emacs::Result<Value<'e>> {
        self.to_value(env)
    }
}
