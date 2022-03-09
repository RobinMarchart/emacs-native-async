use libc::{kill, pid_t, write, SIGUSR1};
use parking_lot::Mutex;
use std::{collections::HashMap, io, sync::atomic::AtomicI64};

pub mod to_lisp;

#[repr(C)]
pub struct NotificationHandler {
    pid: pid_t,
    notify_fd: i32,
    pending: Mutex<HashMap<i64, emacs::Result<to_lisp::ToLispConvert>>>,
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

    pub fn submit<T: Into<to_lisp::ToLispConvert>>(
        &self,
        value: emacs::Result<T>,
        id: i64,
    ) -> anyhow::Result<()> {
        self.pending.lock().insert(id, value.map(|v| v.into()));
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
            index += usize::try_from(if written == -1 {
                Err(std::io::Error::last_os_error())
            } else {
                Ok(written)
            }?)?;
        }
        Ok(())
    }
    pub fn register(&self) -> i64 {
        self.id_gen
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }
    pub fn retrieve(&self, id: i64) -> emacs::Result<to_lisp::ToLispConvert> {
        self.pending
            .lock()
            .remove(&id)
            .ok_or_else(|| anyhow::anyhow!("id not available"))?
    }
}

impl Drop for NotificationHandler {
    fn drop(&mut self) {
        if -1 == unsafe { kill(self.pid, SIGUSR1) } {
            eprintln!("error killing child: {}", io::Error::last_os_error())
        }
    }
}
