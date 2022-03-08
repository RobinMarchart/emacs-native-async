use std::{
    io::stdout,
    mem::{size_of, zeroed},
    os::unix::io::AsRawFd,
    ptr::{copy_nonoverlapping, null_mut},
};

use libc::{
    c_int, c_uchar, getpid, iovec, msghdr, pid_t, sendmsg, sigaction, sigaddset, sigemptyset,
    sigset_t, sigwait, CMSG_DATA, CMSG_FIRSTHDR, CMSG_LEN, CMSG_SPACE, SCM_RIGHTS, SIGUSR1,
    SIG_IGN, SOL_SOCKET,
};

fn main() -> anyhow::Result<()> {
    let action: sigaction = unsafe {
        let mut set: sigset_t = zeroed();
        sigemptyset(&mut set);
        let mut action = sigaction {
            sa_sigaction: SIG_IGN,
            sa_flags: 0,
            sa_mask: set,
            sa_restorer: None,
        };
        sigaction(SIGUSR1, &action, null_mut());
        sigaddset(&mut action.sa_mask, SIGUSR1);
        action
    };
    let mut buf: Vec<c_uchar> =
        vec![0; unsafe { CMSG_SPACE(size_of::<c_int>().try_into()?) }.try_into()?];
    let mut pid = unsafe { getpid() };
    let mut vec = iovec {
        iov_base: (&mut pid as *mut pid_t).cast(),
        iov_len: size_of::<pid_t>(),
    };
    let msg = msghdr {
        msg_name: null_mut(),
        msg_namelen: 0,
        msg_iov: (&mut vec as *mut iovec).cast(),
        msg_iovlen: 1,
        msg_flags: 0,
        msg_control: buf.as_mut_ptr().cast(),
        msg_controllen: buf.len(),
    };
    let cmsg = &mut unsafe { *CMSG_FIRSTHDR(&msg) };
    cmsg.cmsg_level = SOL_SOCKET;
    cmsg.cmsg_type = SCM_RIGHTS;
    cmsg.cmsg_len = unsafe { CMSG_LEN(size_of::<c_int>().try_into()?).try_into()? };
    unsafe { copy_nonoverlapping(&stdout().as_raw_fd(), CMSG_DATA(cmsg).cast(), 1) };
    let fd: i32 = std::env::args()
        .nth(1)
        .ok_or_else(|| anyhow::anyhow!("expected fd from command line argument"))?
        .parse()?;
    unsafe {
        sendmsg(fd, &msg, 0);
        let mut s: i32 = 0;
        sigwait(&action.sa_mask, &mut s);
    }
    Ok(())
}
