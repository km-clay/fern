use crate::{libsh::{error::ShResult, term::{Style, Styled}}, prelude::*, procio::borrow_fd, state::{self, set_status, write_jobs}};

pub const SIG_EXIT_OFFSET: i32 = 128;

bitflags! {
	#[derive(Debug, Copy, Clone)]
	pub struct JobCmdFlags: u8 {
		const LONG     = 0b0000_0001; // 0x01
		const PIDS     = 0b0000_0010; // 0x02
		const NEW_ONLY = 0b0000_0100; // 0x04
		const RUNNING  = 0b0000_1000; // 0x08
		const STOPPED  = 0b0001_0000; // 0x10
		const INIT     = 0b0010_0000; // 0x20
	}
}

#[derive(Debug)]
pub struct DisplayWaitStatus(pub WtStat);

impl fmt::Display for DisplayWaitStatus {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match &self.0 {
			WtStat::Exited(_, code) => {
				match code {
					0 => write!(f, "done"),
					_ => write!(f, "failed: {}", code),
				}
			}
			WtStat::Signaled(_, signal, _) => {
				write!(f, "signaled: {:?}", signal)
			}
			WtStat::Stopped(_, signal) => {
				write!(f, "stopped: {:?}", signal)
			}
			WtStat::PtraceEvent(_, signal, _) => {
				write!(f, "ptrace event: {:?}", signal)
			}
			WtStat::PtraceSyscall(_) => {
				write!(f, "ptrace syscall")
			}
			WtStat::Continued(_) => {
				write!(f, "continued")
			}
			WtStat::StillAlive => {
				write!(f, "running")
			}
		}
	}
}

#[derive(Clone,Debug)]
pub enum JobID {
	Pgid(Pid),
	Pid(Pid),
	TableID(usize),
	Command(String)
}

#[derive(Debug,Clone)]
pub struct ChildProc {
	pgid: Pid,
	pid: Pid,
	command: Option<String>,
	stat: WtStat
}

impl ChildProc {
	pub fn new(pid: Pid, command: Option<&str>, pgid: Option<Pid>) -> ShResult<Self> {
		let command = command.map(|str| str.to_string());
		let stat = if kill(pid,None).is_ok() {
			WtStat::StillAlive
		} else {
			WtStat::Exited(pid, 0)
		};
		let mut child = Self { pgid: pid, pid, command, stat };
		if let Some(pgid) = pgid {
			child.set_pgid(pgid).ok();
		}
		flog!(TRACE, "new child: {:?}", child);
		Ok(child)
	}
	pub fn pid(&self) -> Pid {
		self.pid
	}
	pub fn pgid(&self) -> Pid {
		self.pgid
	}
	pub fn cmd(&self) -> Option<&str> {
		self.command.as_deref()
	}
	pub fn stat(&self) -> WtStat {
		self.stat
	}
	pub fn wait(&mut self, flags: Option<WtFlag>) -> Result<WtStat,Errno> {
		let result = waitpid(self.pid, flags);
		if let Ok(stat) = result {
			self.stat = stat
		}
		result
	}
	pub fn kill<T: Into<Option<Signal>>>(&self, sig: T) -> ShResult<()> {
		Ok(kill(self.pid, sig)?)
	}
	pub fn set_pgid(&mut self, pgid: Pid) -> ShResult<()> {
		setpgid(self.pid, pgid)?;
		self.pgid = pgid;
		Ok(())
	}
	pub fn set_stat(&mut self, stat: WtStat) {
		self.stat = stat
	}
	pub fn is_alive(&self) -> bool {
		self.stat == WtStat::StillAlive
	}
	pub fn is_stopped(&self) -> bool {
		matches!(self.stat,WtStat::Stopped(..))
	}
	pub fn exited(&self) -> bool {
		matches!(self.stat,WtStat::Exited(..))
	}
}

#[derive(Default,Debug)]
pub struct JobTab {
	fg: Option<Job>,
	order: Vec<usize>,
	new_updates: Vec<usize>,
	jobs: Vec<Option<Job>>
}

impl JobTab {
	pub fn new() -> Self {
		Self::default()
	}
	pub fn take_fg(&mut self) -> Option<Job> {
		self.fg.take()
	}
	fn next_open_pos(&self) -> usize {
		if let Some(position) = self.jobs.iter().position(|slot| slot.is_none()) {
			position
		} else {
			self.jobs.len()
		}
	}
	pub fn jobs(&self) -> &Vec<Option<Job>> {
		&self.jobs
	}
	pub fn jobs_mut(&mut self) -> &mut Vec<Option<Job>> {
		&mut self.jobs
	}
	pub fn curr_job(&self) -> Option<usize> {
		self.order.last().copied()
	}
	pub fn prev_job(&self) -> Option<usize> {
		self.order.last().copied()
	}
	fn prune_jobs(&mut self) {
		while let Some(job) = self.jobs.last() {
			if job.is_none() {
				self.jobs.pop();
			} else {
				break
			}
		}
	}
	pub fn insert_job(&mut self, mut job: Job, silent: bool) -> ShResult<usize> {
		self.prune_jobs();
		let tab_pos = if let Some(id) = job.tabid() { id } else { self.next_open_pos() };
		job.set_tabid(tab_pos);
		self.order.push(tab_pos);
		if !silent {
			write(borrow_fd(1),job.display(&self.order, JobCmdFlags::INIT).as_bytes())?;
		}
		if tab_pos == self.jobs.len() {
			self.jobs.push(Some(job))
		} else {
			self.jobs[tab_pos] = Some(job);
		}
		Ok(tab_pos)
	}
	pub fn order(&self) -> &[usize] {
		&self.order
	}
	pub fn query(&self, identifier: JobID) -> Option<&Job> {
		match identifier {
			// Match by process group ID
			JobID::Pgid(pgid) => {
				self.jobs.iter().find_map(|job| {
					job.as_ref().filter(|j| j.pgid() == pgid)
				})
			}
			// Match by process ID
			JobID::Pid(pid) => {
				self.jobs.iter().find_map(|job| {
					job.as_ref().filter(|j| j.children().iter().any(|child| child.pid() == pid))
				})
			}
			// Match by table ID (index in the job table)
			JobID::TableID(id) => {
				self.jobs.get(id).and_then(|job| job.as_ref())
			}
			// Match by command name (partial match)
			JobID::Command(cmd) => {
				self.jobs.iter().find_map(|job| {
					job.as_ref().filter(|j| {
						j.children().iter().any(|child| {
							child.cmd().as_ref().is_some_and(|c| c.contains(&cmd))
						})
					})
				})
			}
		}
	}
	pub fn query_mut(&mut self, identifier: JobID) -> Option<&mut Job> {
		match identifier {
			// Match by process group ID
			JobID::Pgid(pgid) => {
				self.jobs.iter_mut().find_map(|job| {
					job.as_mut().filter(|j| j.pgid() == pgid)
				})
			}
			// Match by process ID
			JobID::Pid(pid) => {
				self.jobs.iter_mut().find_map(|job| {
					job.as_mut().filter(|j| j.children().iter().any(|child| child.pid() == pid))
				})
			}
			// Match by table ID (index in the job table)
			JobID::TableID(id) => {
				self.jobs.get_mut(id).and_then(|job| job.as_mut())
			}
			// Match by command name (partial match)
			JobID::Command(cmd) => {
				self.jobs.iter_mut().find_map(|job| {
					job.as_mut().filter(|j| {
						j.children().iter().any(|child| {
							child.cmd().as_ref().is_some_and(|c| c.contains(&cmd))
						})
					})
				})
			}
		}
	}
	pub fn get_fg(&self) -> Option<&Job> {
		self.fg.as_ref()
	}
	pub fn get_fg_mut(&mut self) -> Option<&mut Job> {
		self.fg.as_mut()
	}
	pub fn new_fg(&mut self, job: Job) -> ShResult<Vec<WtStat>> {
		let pgid = job.pgid();
		self.fg = Some(job);
		attach_tty(pgid)?;
		let statuses = self.fg.as_mut().unwrap().wait_pgrp()?;
		attach_tty(getpgrp())?;
		Ok(statuses)
	}
	pub fn fg_to_bg(&mut self, stat: WtStat) -> ShResult<()> {
		if self.fg.is_none() {
			return Ok(())
		}
		take_term()?;
		let fg = std::mem::take(&mut self.fg);
		if let Some(mut job) = fg {
			job.set_stats(stat);
			self.insert_job(job, false)?;
		}
		Ok(())
	}
	pub fn bg_to_fg(&mut self, id: JobID) -> ShResult<()> {
		let job = self.remove_job(id);
		if let Some(job) = job {
			wait_fg(job)?;
		}
		Ok(())
	}
	pub fn remove_job(&mut self, id: JobID) -> Option<Job> {
		let tabid = self.query(id).map(|job| job.tabid().unwrap());
		if let Some(tabid) = tabid {
			self.jobs.get_mut(tabid).and_then(Option::take)
		} else {
			None
		}
	}
	pub fn print_jobs(&mut self, flags: JobCmdFlags) -> ShResult<()> {
		let jobs = if flags.contains(JobCmdFlags::NEW_ONLY) {
			&self.jobs
				.iter()
				.filter(|job| job.as_ref().is_some_and(|job| self.new_updates.contains(&job.tabid().unwrap())))
				.map(|job| job.as_ref())
				.collect::<Vec<Option<&Job>>>()
		} else {
			&self.jobs
				.iter()
				.map(|job| job.as_ref())
				.collect::<Vec<Option<&Job>>>()
		};
		let mut jobs_to_remove = vec![];
		for job in jobs.iter().flatten() {
			// Skip foreground job
			let id = job.tabid().unwrap();
			// Filter jobs based on flags
			if flags.contains(JobCmdFlags::RUNNING) && !matches!(job.get_stats().get(id).unwrap(), WtStat::StillAlive | WtStat::Continued(_)) {
				continue;
			}
			if flags.contains(JobCmdFlags::STOPPED) && !matches!(job.get_stats().get(id).unwrap(), WtStat::Stopped(_,_)) {
				continue;
			}
			// Print the job in the selected format
			write(borrow_fd(1), format!("{}\n",job.display(&self.order,flags)).as_bytes())?;
			if job.get_stats().iter().all(|stat| matches!(stat,WtStat::Exited(_, _))) {
				jobs_to_remove.push(JobID::TableID(id));
			}
		}
		for id in jobs_to_remove {
			self.remove_job(id);
		}
		Ok(())
	}
}

#[derive(Debug)]
pub struct JobBldr {
	table_id: Option<usize>,
	pgid: Option<Pid>,
	children: Vec<ChildProc>
}

impl Default for JobBldr {
	fn default() -> Self {
		Self::new()
	}
}

impl JobBldr {
	pub fn new() -> Self {
		Self { table_id: None, pgid: None, children: vec![] }
	}
	pub fn with_id(self, id: usize) -> Self {
		Self {
			table_id: Some(id),
			pgid: self.pgid,
			children: self.children
		}
	}
	pub fn with_pgid(self, pgid: Pid) -> Self {
		Self {
			table_id: self.table_id,
			pgid: Some(pgid),
			children: self.children
		}
	}
	pub fn set_pgid(&mut self, pgid: Pid) {
		self.pgid = Some(pgid);
	}
	pub fn pgid(&self) -> Option<Pid> {
		self.pgid
	}
	pub fn with_children(self, children: Vec<ChildProc>) -> Self {
		Self {
			table_id: self.table_id,
			pgid: self.pgid,
			children
		}
	}
	pub fn push_child(&mut self, child: ChildProc) {
		self.children.push(child);
	}
	pub fn build(self) -> Job {
		Job {
			table_id: self.table_id,
			pgid: self.pgid.unwrap_or(Pid::from_raw(0)),
			children: self.children
		}
	}
}

/// A wrapper around Vec<JobBldr> with some job-specific methods
#[derive(Default,Debug)]
pub struct JobStack(Vec<JobBldr>);

impl JobStack {
	pub fn new() -> Self {
		Self::default()
	}
	pub fn new_job(&mut self) {
		self.0.push(JobBldr::new())
	}
	pub fn curr_job_mut(&mut self) -> Option<&mut JobBldr> {
		self.0.last_mut()
	}
	pub fn finalize_job(&mut self) -> Option<Job> {
		self.0.pop().map(|bldr| bldr.build())
	}
}

#[derive(Debug,Clone)]
pub struct Job {
	table_id: Option<usize>,
	pgid: Pid,
	children: Vec<ChildProc>
}

impl Job {
	pub fn set_tabid(&mut self, id: usize) {
		self.table_id = Some(id)
	}
	pub fn running(&self) -> bool {
		!self.children.iter().all(|chld| chld.exited())
	}
	pub fn tabid(&self) -> Option<usize> {
		self.table_id
	}
	pub fn pgid(&self) -> Pid {
		self.pgid
	}
	pub fn get_cmds(&self) -> Vec<&str> {
		let mut cmds = vec![];
		for child in &self.children {
			cmds.push(child.cmd().unwrap_or_default())
		}
		cmds
	}
	pub fn set_stats(&mut self, stat: WtStat) {
		for child in self.children.iter_mut() {
			child.set_stat(stat);
		}
	}
	pub fn get_stats(&self) -> Vec<WtStat> {
		self.children
			.iter()
			.map(|chld| chld.stat())
			.collect::<Vec<WtStat>>()
	}
	pub fn get_pids(&self) -> Vec<Pid> {
		self.children
			.iter()
			.map(|chld| chld.pid())
			.collect::<Vec<Pid>>()
	}
	pub fn children(&self) -> &[ChildProc] {
		&self.children
	}
	pub fn children_mut(&mut self) -> &mut Vec<ChildProc> {
		&mut self.children
	}
	pub fn killpg(&mut self, sig: Signal) -> ShResult<()> {
		let stat = match sig {
			Signal::SIGTSTP => WtStat::Stopped(self.pgid, Signal::SIGTSTP),
			Signal::SIGCONT => WtStat::Continued(self.pgid),
			Signal::SIGTERM => WtStat::Signaled(self.pgid, Signal::SIGTERM, false),
			_ => unimplemented!("{}",sig)
		};
		self.set_stats(stat);
		Ok(killpg(self.pgid, sig)?)
	}
	pub fn wait_pgrp(&mut self) -> ShResult<Vec<WtStat>> {
		let mut stats = vec![];
		flog!(TRACE, "waiting on children");
		flog!(TRACE, self.children);
		for child in self.children.iter_mut() {
			if child.pid == Pid::this() {
				// TODO: figure out some way to get the exit code of builtins
				let code = state::get_status();
				stats.push(WtStat::Exited(child.pid, code));
				continue
			}
			let result = child.wait(Some(WtFlag::WSTOPPED));
			match result {
				Ok(stat) => {
					stats.push(stat);
				}
				Err(Errno::ECHILD) => break,
				Err(e) => return Err(e.into())
			}
		}
		Ok(stats)
	}
	pub fn update_by_id(&mut self, id: JobID, stat: WtStat) -> ShResult<()> {
		match id {
			JobID::Pid(pid) => {
				let query_result = self.children.iter_mut().find(|chld| chld.pid == pid);
				if let Some(child) = query_result {
					child.set_stat(stat);
				}
			}
			JobID::Command(cmd) => {
				let query_result = self.children
					.iter_mut()
					.find(|chld| chld
						.cmd()
						.is_some_and(|chld_cmd| chld_cmd.contains(&cmd))
					);
				if let Some(child) = query_result {
					child.set_stat(stat);
				}
			}
			JobID::TableID(tid) => {
				if self.table_id.is_some_and(|tblid| tblid == tid) {
					for child in self.children.iter_mut() {
						child.set_stat(stat);
					}
				}
			}
			JobID::Pgid(pgid) => {
				if pgid == self.pgid {
					for child in self.children.iter_mut() {
						child.set_stat(stat);
					}
				}
			}
		}
		Ok(())
	}
	pub fn display(&self, job_order: &[usize], flags: JobCmdFlags) -> String {
		let long = flags.contains(JobCmdFlags::LONG);
		let init = flags.contains(JobCmdFlags::INIT);
		let pids = flags.contains(JobCmdFlags::PIDS);

		let current = job_order.last();
		let prev = if job_order.len() > 2 {
			job_order.get(job_order.len() - 2)
		} else {
			None
		};

		let id = self.table_id.unwrap();
		let symbol = if current == self.table_id.as_ref() {
			"+"
		} else if prev == self.table_id.as_ref() {
			"-"
		} else {
			" "
		};
		let padding_count = symbol.len() + id.to_string().len() + 3;
		let padding = " ".repeat(padding_count);

		let mut output = format!("[{}]{}\t", id + 1, symbol);
		for (i, cmd) in self.get_cmds().iter().enumerate() {
			let pid = if pids || init {
				let mut pid = self.get_pids().get(i).unwrap().to_string();
				pid.push(' ');
				pid
			} else {
				"".to_string()
			};
			let job_stat = *self.get_stats().get(i).unwrap();
			let fmt_stat = DisplayWaitStatus(job_stat).to_string();

			let mut stat_line = if init {
				"".to_string()
			} else {
				fmt_stat.clone()
			};
			stat_line = format!("{}{} ",pid,stat_line);
			stat_line = format!("{} {}", stat_line, cmd);
			stat_line = match job_stat {
				WtStat::Stopped(..) | WtStat::Signaled(..) => stat_line.styled(Style::Magenta),
				WtStat::Exited(_, code) => {
					match code {
						0 => stat_line.styled(Style::Green),
						_ => stat_line.styled(Style::Red),
					}
				}
				_ => stat_line.styled(Style::Cyan)
			};
			if i != self.get_cmds().len() - 1 {
				stat_line = format!("{} |",stat_line);
			}

			let stat_final = if long {
				format!(
					"{}{} {}",
					if i != 0 { &padding } else { "" },
					self.get_pids().get(i).unwrap(),
					stat_line
				)
			} else {
				format!(
					"{}{}",
					if i != 0 { &padding } else { "" },
					stat_line
				)
			};
			output.push_str(&stat_final);
			output.push('\n');
		}
		output
	}
}

pub fn term_ctlr() -> Pid {
	tcgetpgrp(borrow_fd(0)).unwrap_or(getpgrp())
}

/// Calls attach_tty() on the shell's process group to retake control of the terminal
pub fn take_term() -> ShResult<()> {
	attach_tty(getpgrp())?;
	Ok(())
}

pub fn disable_reaping() -> ShResult<()> {
	flog!(TRACE, "Disabling reaping");
	unsafe { signal(Signal::SIGCHLD, SigHandler::Handler(crate::signal::ignore_sigchld)) }?;
	Ok(())
}

pub fn enable_reaping() -> ShResult<()> {
	flog!(TRACE, "Enabling reaping");
	unsafe { signal(Signal::SIGCHLD, SigHandler::Handler(crate::signal::handle_sigchld)) }.unwrap();
	Ok(())
}

/// Waits on the current foreground job and updates the shell's last status code
pub fn wait_fg(job: Job) -> ShResult<()> {
	if job.children().is_empty() {
		return Ok(()) // Nothing to do
	}
	flog!(TRACE, "Waiting on foreground job");
	let mut code = 0;
	attach_tty(job.pgid())?;
	disable_reaping()?;
	let statuses = write_jobs(|j| j.new_fg(job))?;
	for status in statuses {
		match status {
			WtStat::Exited(_, exit_code) => {
				code = exit_code;
			}
			WtStat::Stopped(_, sig) => {
				write_jobs(|j| j.fg_to_bg(status))?;
				code = SIG_EXIT_OFFSET + sig as i32;
			},
			WtStat::Signaled(_, sig, _) => {
				if sig == Signal::SIGTSTP {
					write_jobs(|j| j.fg_to_bg(status))?;
				}
				code = SIG_EXIT_OFFSET + sig as i32;
			},
			_ => { /* Do nothing */ }
		}
	}
	take_term()?;
	set_status(code);
	flog!(TRACE, "exit code: {}", code);
	enable_reaping()?;
	Ok(())
}

pub fn dispatch_job(job: Job, is_bg: bool) -> ShResult<()> {
	if is_bg {
		write_jobs(|j| {
			j.insert_job(job, false)
		})?;
	} else {
		wait_fg(job)?;
	}
	Ok(())
}

pub fn attach_tty(pgid: Pid) -> ShResult<()> {
	// If we aren't attached to a terminal, the pgid already controls it, or the process group does not exist
	// Then return ok
	if !isatty(0).unwrap_or(false) || pgid == term_ctlr() || killpg(pgid, None).is_err() {
		return Ok(())
	}
	flog!(TRACE, "Attaching tty to pgid: {}",pgid);

	if pgid == getpgrp() && term_ctlr() != getpgrp() {
		kill(term_ctlr(), Signal::SIGTTOU).ok();
	}

	let mut new_mask = SigSet::empty();
	let mut mask_bkup = SigSet::empty();

	new_mask.add(Signal::SIGTSTP);
	new_mask.add(Signal::SIGTTIN);
	new_mask.add(Signal::SIGTTOU);

	pthread_sigmask(SigmaskHow::SIG_BLOCK, Some(&new_mask), Some(&mut mask_bkup))?;

	let result = tcsetpgrp(borrow_fd(0), pgid);

	pthread_sigmask(SigmaskHow::SIG_SETMASK, Some(&mask_bkup), Some(&mut new_mask))?;

	match result {
		Ok(_) => Ok(()),
		Err(e) => {
			flog!(ERROR, "error while switching term control: {}",e);
			tcsetpgrp(borrow_fd(0), getpgrp())?;
			Ok(())
		}
	}
}
