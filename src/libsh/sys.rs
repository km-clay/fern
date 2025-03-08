use std::{fmt::Display, os::fd::AsRawFd};

use nix::sys::termios;

use crate::prelude::*;

pub const SIG_EXIT_OFFSET: i32 = 128;

pub fn get_bin_path(command: &str, shenv: &ShEnv) -> Option<PathBuf> {
	let env = shenv.vars().env();
	let path_var = env.get("PATH")?;
	let mut paths = path_var.split(':');
	while let Some(raw_path) = paths.next() {
		let mut path = PathBuf::from(raw_path);
		path.push(command);
		//TODO: handle this unwrap
		if path.exists() {
			return Some(path)
		}
	}
	None
}

pub fn write_out(text: impl Display) -> ShResult<()> {
	write(borrow_fd(1), text.to_string().as_bytes())?;
	Ok(())
}

pub fn write_err(text: impl Display) -> ShResult<()> {
	write(borrow_fd(2), text.to_string().as_bytes())?;
	Ok(())
}

/// Return is `readpipe`, `writepipe`
/// Contains all of the necessary boilerplate for grabbing two pipe fds using libc::pipe()
pub fn c_pipe() -> Result<(RawFd,RawFd),Errno> {
	let mut pipes: [i32;2] = [0;2];
	let ret = unsafe { libc::pipe(pipes.as_mut_ptr()) };
	if ret < 0 {
		return Err(Errno::from_raw(ret))
	}
	Ok((pipes[0],pipes[1]))
}

pub fn sh_quit(code: i32) -> ! {
	write_jobs(|j| {
		for job in j.jobs_mut().iter_mut().flatten() {
			job.killpg(Signal::SIGTERM).ok();
		}
	});
	if let Some(termios) = crate::get_saved_termios() {
		termios::tcsetattr(std::io::stdin(), termios::SetArg::TCSANOW, &termios).unwrap();
	}
	exit(code);
}

pub fn read_to_string(fd: i32) -> ShResult<String> {
	let mut buf = Vec::with_capacity(4096);
	let mut temp_buf = [0u8;1024];

	loop {
		match read(fd, &mut temp_buf) {
			Ok(0) => break, // EOF
			Ok(n) => buf.extend_from_slice(&temp_buf[..n]),
			Err(Errno::EINTR) => continue, // Retry on EINTR
			Err(e) => return Err(e.into()), // Return other errors
		}
	}

	Ok(String::from_utf8_lossy(&buf).to_string())
}

pub fn execvpe(cmd: String, argv: Vec<String>, envp: Vec<String>) -> Result<(),Errno> {
	let cmd_raw = CString::new(cmd).unwrap();

	let argv = argv.into_iter().map(|arg| CString::new(arg).unwrap()).collect::<Vec<CString>>();
	let envp = envp.into_iter().map(|var| CString::new(var).unwrap()).collect::<Vec<CString>>();

	nix::unistd::execvpe(&cmd_raw, &argv, &envp).unwrap();
	Ok(())
}
