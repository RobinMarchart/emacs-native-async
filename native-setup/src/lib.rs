use emacs::{defun, Env, IntoLisp, Value};
use emacs_native_async::{to_lisp::ToLispConvert, NotificationHandler};
use libc::{
    c_int, c_uchar, iovec, msghdr, pid_t, recvmsg, socketpair, AF_UNIX, CMSG_DATA, CMSG_FIRSTHDR,
    CMSG_SPACE, SOCK_STREAM,
};
use std::{
    io,
    mem::size_of,
    ptr::null_mut,
    sync::Arc,
};

#[allow(unused)]
fn main() {
    emacs::plugin_is_GPL_compatible!();

    #[emacs::module(name = "emacs-native-async-impl", separator = "/")]
    fn init(env: &Env) -> emacs::Result<()> {
        #[defun]
        fn setup(env: &Env, setup_callback: Value) -> emacs::Result<Arc<NotificationHandler>> {
            let mut fds: [i32; 2] = [0; 2];
            if -1 == unsafe { socketpair(AF_UNIX, SOCK_STREAM, 0, fds.as_mut_ptr()) } {
                return Err(io::Error::last_os_error().into());
            }
            setup_callback.call([fds[0].into_lisp(env)?])?;
            let mut buf: Vec<c_uchar> =
                vec![0; unsafe { CMSG_SPACE(size_of::<c_int>().try_into()?) }.try_into()?];
            let mut pid: pid_t = 0;
            let mut vec = iovec {
                iov_base: (&mut pid as *mut pid_t).cast(),
                iov_len: size_of::<pid_t>(),
            };
            let mut msg = msghdr {
                msg_name: null_mut(),
                msg_namelen: 0,
                msg_iov: (&mut vec as *mut iovec).cast(),
                msg_iovlen: 1,
                msg_flags: 0,
                msg_control: buf.as_mut_ptr().cast(),
                msg_controllen: buf.len(),
            };
            if -1 == unsafe { recvmsg(fds[1], &mut msg, 0) } {
                return Err(io::Error::last_os_error().into());
            }
            let cmsg = &mut unsafe { *CMSG_FIRSTHDR(&msg) };
            let fd: c_int = unsafe { *CMSG_DATA(cmsg).cast() };

            Ok(Arc::new(NotificationHandler::new(fd, pid)))
        }

        #[defun]
        fn retrieve(
            env: &Env,
            id: u64,
            notifications: &Arc<NotificationHandler>,
        ) -> emacs::Result<ToLispConvert> {
            notifications.retrieve(id)
        }
        Ok(())
    }
}
