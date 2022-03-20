use std::{collections::HashMap, io, sync::atomic::AtomicI64};

use abi_stable::{
    rtry,
    std_types::{RBoxError, ROption, RResult},
};
use emacs_native_async::{to_lisp:: ToLispConvert, NotificationHandlerImpl};
use libc::{close, kill, pid_t, write, SIGUSR1};
use parking_lot::Mutex;
use RResult::ROk;

pub struct NotificationHandler {
    pid: pid_t,
    notify_fd: i32,
    pending: Mutex<HashMap<i64, RResult<ToLispConvert, RBoxError>>>,
    id_gen: AtomicI64,
}

impl NotificationHandler {
    pub fn new(fd: i32, pid: pid_t) -> NotificationHandler {
        NotificationHandler {
            pid,
            notify_fd: fd,
            pending: Mutex::new(HashMap::new()),
            id_gen: AtomicI64::new(0),
        }
    }
}

impl Drop for NotificationHandler {
    fn drop(&mut self) {
        if -1 == unsafe { kill(self.pid, SIGUSR1) } {
            eprintln!("error killing child: {}", io::Error::last_os_error())
        }
        if -1 == unsafe { close(self.notify_fd) } {
            eprintln!("error closing pipe: {}", io::Error::last_os_error())
        }
    }
}

impl NotificationHandlerImpl for NotificationHandler {
    fn submit(
        &self,
        value: RResult<ToLispConvert, RBoxError>,
        id: i64,
    ) -> RResult<(), RBoxError> {
        self.pending.lock().insert(id, value);
        let id = id.to_le_bytes();
        let mut index: usize = 0;
        while index != id.len() {
            let written = unsafe {
                write(
                    self.notify_fd,
                    id.as_ptr().add(index).cast(),
                    id.len() - index,
                )
            };
            index += rtry!(usize::try_from(rtry!(if written == -1 {
                Err(RBoxError::new(std::io::Error::last_os_error()))
            } else {
                Ok(written)
            })).map_err(RBoxError::new));
        }
        ROk(())
    }

    fn register(&self) -> i64 {
        self.id_gen
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }

    fn retrieve(&self, id: i64) -> ROption<RResult<ToLispConvert, RBoxError>> {
        self.pending.lock().remove(&id).into()
    }
}
