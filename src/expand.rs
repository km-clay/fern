use std::collections::HashSet;
use std::iter::Peekable;
use std::str::{Chars, FromStr};

use glob::Pattern;
use regex::Regex;

use crate::state::{read_jobs, read_vars, write_jobs, write_meta, write_vars, LogTab};
use crate::procio::{IoBuf, IoFrame, IoMode, IoStack};
use crate::prelude::*;
use crate::parse::{Redir, RedirType};
use crate::parse::execute::exec_input;
use crate::parse::lex::{is_field_sep, is_hard_sep, LexFlags, LexStream, Tk, TkFlags, TkRule};
use crate::libsh::error::{ShErr, ShErrKind, ShResult};

/// Variable substitution marker
pub const VAR_SUB: char = '\u{fdd0}';
/// Double quote '"' marker
pub const DUB_QUOTE: char = '\u{fdd1}';
/// Single quote '\\'' marker
pub const SNG_QUOTE: char = '\u{fdd2}';
/// Tilde sub marker
pub const TILDE_SUB: char = '\u{fdd3}';
/// Subshell marker
pub const SUBSH: char = '\u{fdd4}';
/// Input process sub marker
pub const PROC_SUB_IN: char = '\u{fdd5}';
/// Output process sub marker
pub const PROC_SUB_OUT: char = '\u{fdd6}';

impl Tk {
	/// Create a new expanded token
	///
	/// params
	/// tokens: A vector of raw tokens lexed from the expansion result
	/// span: The span of the original token that is being expanded
	/// flags: some TkFlags
	pub fn expand(self) -> ShResult<Self> {
		let flags = self.flags;
		let span = self.span.clone();
		let exp = Expander::new(self).expand()?;
		let class = TkRule::Expanded { exp };
		Ok(Self { class, span, flags, })
	}
	/// Perform word splitting
	pub fn get_words(&self) -> Vec<String> {
		match &self.class {
			TkRule::Expanded { exp } => exp.clone(),
			_ => vec![self.to_string()]
		}
	}
}

pub struct Expander {
	raw: String,
}

impl Expander {
	pub fn new(raw: Tk) -> Self {
		let unescaped = unescape_str(raw.span.as_str());
		Self { raw: unescaped }
	}
	pub fn expand(&mut self) -> ShResult<Vec<String>> {
		let mut chars = self.raw.chars().peekable();
		self.raw = expand_raw(&mut chars)?;
		if let Ok(glob_exp) = expand_glob(&self.raw) {
			if !glob_exp.is_empty() {
				self.raw = glob_exp;
			}
		}
		Ok(self.split_words())
	}
	pub fn split_words(&mut self) -> Vec<String> {
		let mut words = vec![];
		let mut chars = self.raw.chars();
		let mut cur_word = String::new();

		'outer: while let Some(ch) = chars.next() {
			match ch {
				DUB_QUOTE |
				SNG_QUOTE |
				SUBSH => {
					while let Some(q_ch) = chars.next() {
						match q_ch {
							_ if q_ch == ch => continue 'outer, // Isn't rust cool
							_ => cur_word.push(q_ch)
						}
					}
				}
				_ if is_field_sep(ch) => {
					words.push(mem::take(&mut cur_word));
				}
				_ => cur_word.push(ch)
			}
		}
		if !cur_word.is_empty() {
			words.push(cur_word);
		}
		words
	}
}

pub fn expand_raw(chars: &mut Peekable<Chars<'_>>) -> ShResult<String> {
	let mut result = String::new();

	while let Some(ch) = chars.next() {
		match ch {
			TILDE_SUB => {
				let home = env::var("HOME").unwrap_or_default();
				result.push_str(&home);
			}
			PROC_SUB_OUT =>{
				let mut inner = String::new();
				while let Some(ch) = chars.next() {
					match ch {
						PROC_SUB_OUT => break,
						_ => inner.push(ch)
					}
				}
				let fd_path = expand_proc_sub(&inner, false)?;
				result.push_str(&fd_path);
			}
			PROC_SUB_IN =>{
				let mut inner = String::new();
				while let Some(ch) = chars.next() {
					match ch {
						PROC_SUB_IN => break,
						_ => inner.push(ch)
					}
				}
				let fd_path = expand_proc_sub(&inner, true)?;
				result.push_str(&fd_path);
			}
			VAR_SUB => {
				flog!(INFO, chars);
				let expanded = expand_var(chars)?;
				result.push_str(&expanded);
			}
			_ => result.push(ch)
		}
	}
	Ok(result)
}

pub fn expand_var(chars: &mut Peekable<Chars<'_>>) -> ShResult<String> {
	let mut var_name = String::new();
	let mut in_brace = false;
	while let Some(&ch) = chars.peek() {
		match ch {
			SUBSH if var_name.is_empty() => {
				chars.next(); // now safe to consume
				let mut subsh_body = String::new();
				while let Some(c) = chars.next() {
					if c == SUBSH { break }
					subsh_body.push(c);
				}
				let expanded = expand_cmd_sub(&subsh_body)?;
				return Ok(expanded);
			}
			'{' if var_name.is_empty() => {
				chars.next(); // consume the brace
				in_brace = true;
			}
			'}' if in_brace => {
				chars.next(); // consume the brace
				let val = perform_param_expansion(&var_name)?;
				return Ok(val);
			}
			ch if in_brace => {
				chars.next(); // safe to consume
				var_name.push(ch);
			}
			ch if is_hard_sep(ch) || !(ch.is_alphanumeric() || ch == '_' || ch == '-') => {
				let val = read_vars(|v| v.get_var(&var_name));
				flog!(INFO,var_name);
				flog!(INFO,val);
				flog!(INFO,ch);
				return Ok(val); 
			}
			_ => {
				chars.next(); 
				var_name.push(ch);
			}
		}
	}
	if !var_name.is_empty() {
		let var_val = read_vars(|v| v.get_var(&var_name));
		flog!(INFO,var_val);
		Ok(var_val)
	} else {
		Ok(String::new())
	}
}

pub fn expand_glob(raw: &str) -> ShResult<String> {
	let mut words = vec![];

	for entry in glob::glob(raw)
		.map_err(|_| ShErr::simple(ShErrKind::SyntaxErr, "Invalid glob pattern"))? {
		let entry = entry
			.map_err(|_| ShErr::simple(ShErrKind::SyntaxErr, "Invalid filename found in glob"))?;

		words.push(entry.to_str().unwrap().to_string())
	}
	Ok(words.join(" "))
}

pub fn is_a_number(raw: &str) -> bool {
	let trimmed = raw.trim();
	trimmed.parse::<i32>().is_ok() || trimmed.parse::<f64>().is_ok()
}

enum ArithTk {
	Num(f64),
	Op(ArithOp),
	LParen,
	RParen
}

impl ArithTk {
	pub fn tokenize(raw: &str) -> ShResult<Vec<Self>> {
		let mut tokens = Vec::new();
		let mut chars = raw.chars().peekable();

		while let Some(&ch) = chars.peek() {
			match ch {
				' ' | '\t' => { chars.next(); }
				'0'..='9' | '.' => {
					let mut num = String::new();
					while let Some(&digit) = chars.peek() {
						if digit.is_ascii_digit() || digit == '.' {
							num.push(digit);
							chars.next();
						} else {
							break;
						}
					}
					let Ok(num) = num.parse::<f64>() else {
						panic!()
					};
					tokens.push(Self::Num(num));
				}
				'+' | '-' | '*' | '/' | '%' => {
					let mut buf = String::new();
					buf.push(ch);
					tokens.push(Self::Op(buf.parse::<ArithOp>().unwrap()));
					chars.next();
				}
				'(' => { tokens.push(Self::LParen); chars.next(); }
				')' => { tokens.push(Self::RParen); chars.next(); }
				_ => return Err(ShErr::Simple { kind: ShErrKind::ParseErr, msg: "Invalid character in arithmetic substitution".into(), notes: vec![] })
			}
		}

		Ok(tokens)
	}

	fn to_rpn(tokens: Vec<ArithTk>) -> ShResult<Vec<ArithTk>> {
		let mut output = Vec::new();
		let mut ops = Vec::new();

		fn precedence(op: &ArithOp) -> usize {
			match op {
				ArithOp::Add | 
				ArithOp::Sub => 1,
				ArithOp::Mul |
				ArithOp::Div |
				ArithOp::Mod => 2,
			}
		}

		for token in tokens {
			match token {
				ArithTk::Num(_) => output.push(token),
				ArithTk::Op(op1) => {
					while let Some(ArithTk::Op(op2)) = ops.last() {
						if precedence(op2) >= precedence(&op1) {
							output.push(ops.pop().unwrap());
						} else {
							break;
						}
					}
					ops.push(ArithTk::Op(op1));
				}
				ArithTk::LParen => ops.push(ArithTk::LParen),
				ArithTk::RParen => {
					while let Some(top) = ops.pop() {
						if let ArithTk::LParen = top {
							break;
						} else {
							output.push(top);
						}
					}
				}
			}
		}

		while let Some(op) = ops.pop() {
			output.push(op);
		}

		Ok(output)
	}
	pub fn eval_rpn(tokens: Vec<ArithTk>) -> ShResult<f64> {
		let mut stack = Vec::new();

		for token in tokens {
			match token {
				ArithTk::Num(n) => stack.push(n),
				ArithTk::Op(op) => {
					let rhs = stack.pop().ok_or(ShErr::Simple {
						kind: ShErrKind::ParseErr,
						msg: "Missing right-hand operand".into(),
						notes: vec![],
					})?;
					let lhs = stack.pop().ok_or(ShErr::Simple {
						kind: ShErrKind::ParseErr,
						msg: "Missing left-hand operand".into(),
						notes: vec![],
					})?;
					let result = match op {
						ArithOp::Add => lhs + rhs,
						ArithOp::Sub => lhs - rhs,
						ArithOp::Mul => lhs * rhs,
						ArithOp::Div => lhs / rhs,
						ArithOp::Mod => lhs % rhs,
					};
					stack.push(result);
				}
				_ => return Err(ShErr::Simple {
					kind: ShErrKind::ParseErr,
					msg: "Unexpected token during evaluation".into(),
					notes: vec![],
				}),
			}
		}

		if stack.len() != 1 {
			return Err(ShErr::Simple {
				kind: ShErrKind::ParseErr,
				msg: "Invalid arithmetic expression".into(),
				notes: vec![],
			});
		}

		Ok(stack[0])
	}
}

enum ArithOp {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
}

impl FromStr for ArithOp {
	type Err = ShErr;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		assert!(s.len() == 1);
		match s.chars().next().unwrap() {
			'+' => Ok(Self::Add),
			'-' => Ok(Self::Sub),
			'*' => Ok(Self::Mul),
			'/' => Ok(Self::Div),
			'%' => Ok(Self::Mod),
			_ => Err(ShErr::Simple { kind: ShErrKind::ParseErr, msg: "Invalid arithmetic operator".into(), notes: vec![] })
		}
	}
}

pub fn expand_arithmetic(raw: &str) -> ShResult<String> {
	let body = raw
		.strip_prefix('(')
		.unwrap()
		.strip_suffix(')')
		.unwrap(); // Unwraps are safe here, we already checked for the parens
	let unescaped = unescape_math(body); 
	let expanded = expand_raw(&mut unescaped.chars().peekable())?;
	let tokens = ArithTk::tokenize(&expanded)?;
	let rpn = ArithTk::to_rpn(tokens)?;
	let result = ArithTk::eval_rpn(rpn)?;
	Ok(result.to_string())
}

pub fn expand_proc_sub(raw: &str, is_input: bool) -> ShResult<String> {
	// FIXME: Still a lot of issues here
	// Seems like debugging will be a massive effort
	let (rpipe, wpipe) = IoMode::get_pipes();
	let rpipe_raw = rpipe.src_fd();
	let wpipe_raw = wpipe.src_fd();

	let (proc_fd, register_fd, redir_type, path) = match is_input {
		false => (wpipe, rpipe, RedirType::Output, format!("/proc/self/fd/{}", rpipe_raw)),
		true => (rpipe, wpipe, RedirType::Input, format!("/proc/self/fd/{}", wpipe_raw)),
	};

	match unsafe { fork()? } {
		ForkResult::Child => {
			let redir = Redir::new(proc_fd, redir_type);
			let io_frame = IoFrame::from_redir(redir);
			let mut io_stack = IoStack::new();
			io_stack.push_frame(io_frame);

			if let Err(e) = exec_input(raw.to_string(), Some(io_stack)) {
				eprintln!("{e}");
				exit(1);
			}
			exit(0);
		}
		ForkResult::Parent { child } => {
			write_jobs(|j| j.register_fd(child, register_fd));
			let registered = read_jobs(|j| j.registered_fds().to_vec());
			flog!(DEBUG,registered);
			// Do not wait; process may run in background
			Ok(path)
		}
	}
} 

/// Get the command output of a given command input as a String
pub fn expand_cmd_sub(raw: &str) -> ShResult<String> {
	flog!(DEBUG, "in expand_cmd_sub");
	flog!(DEBUG, raw);
	if raw.starts_with('(') && raw.ends_with(')') {
		if let Ok(output) = expand_arithmetic(raw) {
			return Ok(output) // It's actually an arithmetic sub
		}
	}
	let (rpipe,wpipe) = IoMode::get_pipes();
	let cmd_sub_redir = Redir::new(wpipe, RedirType::Output);
	let cmd_sub_io_frame = IoFrame::from_redir(cmd_sub_redir);
	let mut io_stack = IoStack::new();
	let mut io_buf = IoBuf::new(rpipe);

	match unsafe { fork()? } {
		ForkResult::Child => {
			io_stack.push_frame(cmd_sub_io_frame);
			if let Err(e) = exec_input(raw.to_string(), Some(io_stack)) {
				eprintln!("{e}");
				exit(1);
			}
			exit(0);
		}
		ForkResult::Parent { child } => {
			std::mem::drop(cmd_sub_io_frame); // Closes the write pipe
			let status = waitpid(child, Some(WtFlag::WSTOPPED))?;
			match status {
				WtStat::Exited(_, _) => {
					flog!(DEBUG, "filling buffer");
					io_buf.fill_buffer()?;
					flog!(DEBUG, "done");
					Ok(io_buf.as_str()?.trim().to_string())
				}
				_ => Err(ShErr::simple(ShErrKind::InternalErr, "Command sub failed"))
			}
		}
	}
}

/// Processes strings into intermediate representations that are more readable by the program
///
/// Clean up a single layer of escape characters, and then replace control characters like '$' with a non-character unicode representation that is unmistakable by the rest of the code
pub fn unescape_str(raw: &str) -> String {
	let mut chars = raw.chars().peekable();
	let mut result = String::new();
	let mut first_char = true;


	while let Some(ch) = chars.next() {
		match ch {
			'~' if first_char => {
				result.push(TILDE_SUB)
			}
			'\\' => {
				if let Some(next_ch) = chars.next() {
					result.push(next_ch)
				}
			}
			'(' => {
				result.push(SUBSH);
				let mut paren_count = 1;
				while let Some(subsh_ch) = chars.next() {
					match subsh_ch {
						'\\' => {
							result.push(subsh_ch);
							if let Some(next_ch) = chars.next() {
								result.push(next_ch)
							}
						}
						'$' if chars.peek() != Some(&'(') => result.push(VAR_SUB),
						'(' => {
							paren_count += 1;
							result.push(subsh_ch)
						}
						')' => {
							paren_count -= 1;
							if paren_count == 0 {
								result.push(SUBSH);
								break
							} else {
								result.push(subsh_ch)
							}
						}
						_ => result.push(subsh_ch)
					}
				}
			}
			'"' => {
				result.push(DUB_QUOTE);
				while let Some(q_ch) = chars.next() {
					match q_ch {
						'\\' => {
							result.push(q_ch);
							if let Some(next_ch) = chars.next() {
								result.push(next_ch)
							}
						}
						'$' => {
							result.push(VAR_SUB);
							if chars.peek() == Some(&'(') {
								chars.next();
								let mut paren_count = 1;
								result.push(SUBSH);
								while let Some(subsh_ch) = chars.next() {
									match subsh_ch {
										'\\' => {
											result.push(subsh_ch);
											if let Some(next_ch) = chars.next() {
												result.push(next_ch)
											}
										}
										'(' => {
											result.push(subsh_ch);
											paren_count += 1;
										}
										')' => {
											paren_count -= 1;
											if paren_count <= 0 {
												result.push(SUBSH);
												break
											} else {
												result.push(subsh_ch);
											}
										}
										_ => result.push(subsh_ch),
									}
								}
							}
						}
						'"' => {
							result.push(DUB_QUOTE);
							break
						}
						_ => result.push(q_ch)
					}
				}
			}
			'\'' => {
				result.push(SNG_QUOTE);
				while let Some(q_ch) = chars.next() {
					match q_ch {
						'\'' => {
							result.push(SNG_QUOTE);
							break
						}
						_ => result.push(q_ch)
					}
				}
			}
			'<' if chars.peek() == Some(&'(') => {
				chars.next();
				let mut paren_count = 1;
				result.push(PROC_SUB_OUT);
				while let Some(subsh_ch) = chars.next() {
					match subsh_ch {
						'\\' => {
							result.push(subsh_ch);
							if let Some(next_ch) = chars.next() {
								result.push(next_ch)
							}
						}
						'(' => {
							result.push(subsh_ch);
							paren_count += 1;
						}
						')' => {
							paren_count -= 1;
							if paren_count <= 0 {
								result.push(PROC_SUB_OUT);
								break
							} else {
								result.push(subsh_ch);
							}
						}
						_ => result.push(subsh_ch),
					}
				}
			}
			'>' if chars.peek() == Some(&'(') => {
				chars.next();
				let mut paren_count = 1;
				result.push(PROC_SUB_IN);
				while let Some(subsh_ch) = chars.next() {
					match subsh_ch {
						'\\' => {
							result.push(subsh_ch);
							if let Some(next_ch) = chars.next() {
								result.push(next_ch)
							}
						}
						'(' => {
							result.push(subsh_ch);
							paren_count += 1;
						}
						')' => {
							paren_count -= 1;
							if paren_count <= 0 {
								result.push(PROC_SUB_IN);
								break
							} else {
								result.push(subsh_ch);
							}
						}
						_ => result.push(subsh_ch),
					}
				}
			}
			'$' => result.push(VAR_SUB),
			_ => result.push(ch)
		}
		first_char = false;
	}
	result
}

pub fn unescape_math(raw: &str) -> String {
	let mut chars = raw.chars().peekable();
	let mut result = String::new();

	while let Some(ch) = chars.next() {
		flog!(DEBUG,result);
		match ch {
			'\\' => {
				if let Some(next_ch) = chars.next() {
					result.push(next_ch)
				}
			}
			'$' => {
				result.push(VAR_SUB);
				if chars.peek() == Some(&'(') {
					result.push(SUBSH);
					chars.next();
					let mut paren_count = 1;
					while let Some(subsh_ch) = chars.next() {
						match subsh_ch {
							'\\' => {
								result.push(subsh_ch);
								if let Some(next_ch) = chars.next() {
									result.push(next_ch)
								}
							}
							'$' if chars.peek() != Some(&'(') => result.push(VAR_SUB),
							'(' => {
								paren_count += 1;
								result.push(subsh_ch)
							}
							')' => {
								paren_count -= 1;
								if paren_count == 0 {
									result.push(SUBSH);
									break
								} else {
									result.push(subsh_ch)
								}
							}
							_ => result.push(subsh_ch)
						}
					}
				}
			}
			_ => result.push(ch)
		}
	}
	flog!(INFO, result);
	result
}

#[derive(Debug)]
pub enum ParamExp {
	Len, // #var_name
	DefaultUnsetOrNull(String), // :-
	DefaultUnset(String), // -
	SetDefaultUnsetOrNull(String), // :=
	SetDefaultUnset(String), // =
	AltSetNotNull(String), // :+
	AltNotNull(String), // +
	ErrUnsetOrNull(String), // :?
	ErrUnset(String), // ?
	Substr(usize), // :pos
	SubstrLen(usize,usize), // :pos:len
	RemShortestPrefix(String), // #pattern
	RemLongestPrefix(String), // ##pattern
	RemShortestSuffix(String), // %pattern
	RemLongestSuffix(String), // %%pattern
	ReplaceFirstMatch(String,String), // /search/replace
	ReplaceAllMatches(String,String), // //search/replace
	ReplacePrefix(String,String), // #search/replace
	ReplaceSuffix(String,String), // %search/replace
	VarNamesWithPrefix(String), // !prefix@ || !prefix*
	ExpandInnerVar(String), // !var
}

impl FromStr for ParamExp {
	type Err = ShErr;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		use ParamExp::*;

		let parse_err = || Err(ShErr::Simple {
			kind: ShErrKind::SyntaxErr,
			msg: "Invalid parameter expansion".into(),
			notes: vec![],
		});

		// Handle indirect var expansion: ${!var}
		if let Some(var) = s.strip_prefix('!') {
			if var.ends_with('*') || var.ends_with('@') {
				return Ok(VarNamesWithPrefix(var.to_string()));
			}
			return Ok(ExpandInnerVar(var.to_string()));
		}

		// Pattern removals
		if let Some(rest) = s.strip_prefix("##") {
			return Ok(RemLongestPrefix(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix('#') {
			return Ok(RemShortestPrefix(rest.to_string()));
		}
		if let Some(rest) = s.strip_prefix("%%") {
			return Ok(RemLongestSuffix(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix('%') {
			return Ok(RemShortestSuffix(rest.to_string()));
		}

		// Replacements
		if let Some(rest) = s.strip_prefix("//") {
			let mut parts = rest.splitn(2, '/');
			let pattern = parts.next().unwrap_or("");
			let repl = parts.next().unwrap_or("");
			return Ok(ReplaceAllMatches(pattern.to_string(), repl.to_string()));
		}
		if let Some(rest) = s.strip_prefix('/') {
			if let Some(rest) = rest.strip_prefix('%') {
				let mut parts = rest.splitn(2, '/');
				let pattern = parts.next().unwrap_or("");
				let repl = parts.next().unwrap_or("");
				return Ok(ReplaceSuffix(pattern.to_string(), repl.to_string()));
			} else if let Some(rest) = rest.strip_prefix('#') {
				let mut parts = rest.splitn(2, '/');
				let pattern = parts.next().unwrap_or("");
				let repl = parts.next().unwrap_or("");
				return Ok(ReplacePrefix(pattern.to_string(), repl.to_string()));
			} else {
				let mut parts = rest.splitn(2, '/');
				let pattern = parts.next().unwrap_or("");
				let repl = parts.next().unwrap_or("");
				return Ok(ReplaceFirstMatch(pattern.to_string(), repl.to_string()));
			}
		}

		// Fallback / assignment / alt
		if let Some(rest) = s.strip_prefix(":-") {
			return Ok(DefaultUnsetOrNull(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix('-') {
			return Ok(DefaultUnset(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix(":+") {
			return Ok(AltSetNotNull(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix('+') {
			return Ok(AltNotNull(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix(":=") {
			return Ok(SetDefaultUnsetOrNull(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix('=') {
			return Ok(SetDefaultUnset(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix(":?") {
			return Ok(ErrUnsetOrNull(rest.to_string()));
		} else if let Some(rest) = s.strip_prefix('?') {
			return Ok(ErrUnset(rest.to_string()));
		}

		// Substring
		if let Some((pos, len)) = parse_pos_len(s) {
			return Ok(match len {
				Some(l) => SubstrLen(pos, l),
				None => Substr(pos),
			});
		}

		parse_err()
	}
}

pub fn parse_pos_len(s: &str) -> Option<(usize, Option<usize>)> {
	let raw = s.strip_prefix(':')?;
	if let Some((start,len)) = raw.split_once(':') {
		Some((
			start.parse::<usize>().ok()?,
			len.parse::<usize>().ok(),
		))
	} else {
		Some((
			raw.parse::<usize>().ok()?,
			None,
		))
	}
}

pub fn perform_param_expansion(raw: &str) -> ShResult<String> {
	let vars = read_vars(|v| v.clone());
	let mut chars = raw.chars();
	let mut var_name = String::new();
	let mut rest = String::new();
	if raw.starts_with('#') {
		return Ok(vars.get_var(raw.strip_prefix('#').unwrap()).len().to_string())
	}

	while let Some(ch) = chars.next() {
		match ch {
			'!' |
			'#' |
			'%' |
			':' |
			'-' |
			'+' |
			'=' |
			'/' |
			'?' => {
				rest.push(ch);
				rest.push_str(&chars.collect::<String>());
				break
			}
			_ => var_name.push(ch)
		}
	}

	flog!(DEBUG,rest);
	if let Ok(expansion) = rest.parse::<ParamExp>() {
		flog!(DEBUG,expansion);
		match expansion {
			ParamExp::Len => unreachable!(),
			ParamExp::DefaultUnsetOrNull(default) => {
				if !vars.var_exists(&var_name) || vars.get_var(&var_name).is_empty() {
					Ok(default)
				} else {
					Ok(vars.get_var(&var_name))
				}
			}
			ParamExp::DefaultUnset(default) => {
				if !vars.var_exists(&var_name) {
					Ok(default)
				} else {
					Ok(vars.get_var(&var_name))
				}
			}
			ParamExp::SetDefaultUnsetOrNull(default) => {
				if !vars.var_exists(&var_name) || vars.get_var(&var_name).is_empty() {
					write_vars(|v| v.set_var(&var_name, &default, false));
					Ok(default)
				} else {
					Ok(vars.get_var(&var_name))
				}
			}
			ParamExp::SetDefaultUnset(default) => {
				if !vars.var_exists(&var_name) {
					write_vars(|v| v.set_var(&var_name, &default, false));
					Ok(default)
				} else {
					Ok(vars.get_var(&var_name))
				}
			}
			ParamExp::AltSetNotNull(alt) => {
				if vars.var_exists(&var_name) && !vars.get_var(&var_name).is_empty() {
					Ok(alt)
				} else {
					Ok("".into())
				}
			}
			ParamExp::AltNotNull(alt) => {
				if vars.var_exists(&var_name) {
					Ok(alt)
				} else {
					Ok("".into())
				}
			}
			ParamExp::ErrUnsetOrNull(err) => {
				if !vars.var_exists(&var_name) || vars.get_var(&var_name).is_empty() {
					Err(
						ShErr::Simple { kind: ShErrKind::ExecFail, msg: err, notes: vec![] }
					)
				} else {
					Ok(vars.get_var(&var_name))
				}
			}
			ParamExp::ErrUnset(err) => {
				if !vars.var_exists(&var_name) {
					Err(
						ShErr::Simple { kind: ShErrKind::ExecFail, msg: err, notes: vec![] }
					)
				} else {
					Ok(vars.get_var(&var_name))
				}
			}
			ParamExp::Substr(pos) => {
				let value = vars.get_var(&var_name);
				if let Some(substr) = value.get(pos..) {
					Ok(substr.to_string())
				} else {
					Ok(value)
				}
			}
			ParamExp::SubstrLen(pos, len) => {
				let value = vars.get_var(&var_name);
				let end = pos.saturating_add(len);
				if let Some(substr) = value.get(pos..end) {
					Ok(substr.to_string())
				} else {
					Ok(value)
				}
			}
			ParamExp::RemShortestPrefix(prefix) => {
				let value = vars.get_var(&var_name);
				let pattern = Pattern::new(&prefix).unwrap();
				for i in 0..=value.len() {
					let sliced = &value[..i];
					if pattern.matches(sliced) {
						return Ok(value[i..].to_string())
					}
				}
				Ok(value)
			}
			ParamExp::RemLongestPrefix(prefix) => {
				let value = vars.get_var(&var_name);
				let pattern = Pattern::new(&prefix).unwrap();
				for i in (0..=value.len()).rev() {
					let sliced = &value[..i];
					if pattern.matches(sliced) {
						return Ok(value[i..].to_string());
					}
				}
				Ok(value) // no match
			}
			ParamExp::RemShortestSuffix(suffix) => {
				let value = vars.get_var(&var_name);
				let pattern = Pattern::new(&suffix).unwrap();
				for i in (0..=value.len()).rev() {
					let sliced = &value[i..];
					if pattern.matches(sliced) {
						return Ok(value[..i].to_string());
					}
				}
				Ok(value)
			}
			ParamExp::RemLongestSuffix(suffix) => {
				let value = vars.get_var(&var_name);
				let pattern = Pattern::new(&suffix).unwrap();
				for i in 0..=value.len() {
					let sliced = &value[i..];
					if pattern.matches(sliced) {
						return Ok(value[..i].to_string());
					}
				}
				Ok(value)
			}
			ParamExp::ReplaceFirstMatch(search, replace) => {
				let value = vars.get_var(&var_name);
				let regex = glob_to_regex(&search, false); // unanchored pattern

				if let Some(mat) = regex.find(&value) {
					let before = &value[..mat.start()];
					let after = &value[mat.end()..];
					let result = format!("{}{}{}", before, replace, after);
					Ok(result)
				} else {
					Ok(value)
				}
			}
			ParamExp::ReplaceAllMatches(search, replace) => {
				let value = vars.get_var(&var_name);
				let regex = glob_to_regex(&search, false);
				let mut result = String::new();
				let mut last_match_end = 0;

				for mat in regex.find_iter(&value) {
					result.push_str(&value[last_match_end..mat.start()]);
					result.push_str(&replace);
					last_match_end = mat.end();
				}

				// Append the rest of the string
				result.push_str(&value[last_match_end..]);
				Ok(result)
			}
			ParamExp::ReplacePrefix(search, replace) => {
				let value = vars.get_var(&var_name);
				let pattern = Pattern::new(&search).unwrap();
				for i in (0..=value.len()).rev() {
					let sliced = &value[..i];
					if pattern.matches(sliced) {
						return Ok(format!("{}{}",replace,&value[i..]))
					}
				}
				Ok(value)
			}
			ParamExp::ReplaceSuffix(search, replace) => {
				let value = vars.get_var(&var_name);
				let pattern = Pattern::new(&search).unwrap();
				for i in (0..=value.len()).rev() {
					let sliced = &value[i..];
					if pattern.matches(sliced) {
						return Ok(format!("{}{}",&value[..i],replace))
					}
				}
				Ok(value)
			}
			ParamExp::VarNamesWithPrefix(prefix) => {
				let mut match_vars = vec![];
				for var in vars.vars().keys() {
					if var.starts_with(&prefix) {
						match_vars.push(var.clone())
					}
				}
				Ok(match_vars.join(" "))
			}
			ParamExp::ExpandInnerVar(var_name) => {
				let value = vars.get_var(&var_name);
				Ok(vars.get_var(&value))
			}
		}
	} else {
		Ok(vars.get_var(&var_name))
	}
}

fn glob_to_regex(glob: &str, anchored: bool) -> Regex {
	let mut regex = String::new();
	if anchored {
		regex.push('^');
	}
	for ch in glob.chars() {
		match ch {
			'*' => regex.push_str(".*"),
			'?' => regex.push('.'),
			'.' | '+' | '(' | ')' | '|' | '^' | '$' | '[' | ']' | '{' | '}' | '\\' => {
				regex.push('\\');
				regex.push(ch);
			}
			_ => regex.push(ch),
		}
	}
	if anchored {
		regex.push('$');
	}
	flog!(DEBUG, regex);
	Regex::new(&regex).unwrap()
}

#[derive(Debug)]
pub enum PromptTk {
	AsciiOct(i32),
	Text(String),
	AnsiSeq(String),
	VisGrp,
	UserSeq,
	Runtime,
	Weekday,
	Dquote,
	Squote,
	Return,
	Newline,
	Pwd,
	PwdShort,
	Hostname,
	HostnameShort,
	ShellName,
	Username,
	PromptSymbol,
	ExitCode,
	SuccessSymbol,
	FailureSymbol,
	JobCount
}

pub fn format_cmd_runtime(dur: std::time::Duration) -> String {
	const ETERNITY: u128 = f32::INFINITY as u128;
	let mut micros     = dur.as_micros();
	let mut millis     = 0;
	let mut seconds    = 0;
	let mut minutes    = 0;
	let mut hours      = 0;
	let mut days       = 0;
	let mut weeks      = 0;
	let mut months     = 0;
	let mut years      = 0;
	let mut decades    = 0;
	let mut centuries  = 0;
	let mut millennia  = 0;
	let mut epochs     = 0;
	let mut aeons      = 0;
	let mut eternities = 0;

	if micros >= 1000 {
		millis = micros / 1000;
		micros %= 1000;
	}
	if millis >= 1000 {
		seconds = millis / 1000;
		millis %= 1000;
	}
	if seconds >= 60 {
		minutes = seconds / 60;
		seconds %= 60;
	}
	if minutes >= 60 {
		hours = minutes / 60;
		minutes %= 60;
	}
	if hours >= 24 {
		days = hours / 24;
		hours %= 24;
	}
	if days >= 7 {
		weeks = days / 7;
		days %= 7;
	}
	if weeks >= 4 {
		months = weeks / 4;
		weeks %= 4;
	}
	if months >= 12 {
		years = months / 12;
		weeks %= 12;
	}
	if years >= 10 {
		decades = years / 10;
		years %= 10;
	}
	if decades >= 10 {
		centuries = decades / 10;
		decades %= 10;
	}
	if centuries >= 10 {
		millennia = centuries / 10;
		centuries %= 10;
	}
	if millennia >= 1000 {
		epochs = millennia / 1000;
		millennia %= 1000;
	}
	if epochs >= 1000 {
		aeons = epochs / 1000;
		epochs %= aeons;
	}
	if aeons == ETERNITY {
		eternities = aeons / ETERNITY;
		aeons %= ETERNITY;
	}

	// Format the result
	let mut result = Vec::new();
	if eternities > 0 {
		let mut string = format!("{} eternit", eternities);
		if eternities > 1 {
			string.push_str("ies");
		} else {
			string.push('y');
		}
		result.push(string)
	}
	if aeons > 0 {
		let mut string = format!("{} aeon", aeons);
		if aeons > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if epochs > 0 {
		let mut string = format!("{} epoch", epochs);
		if epochs > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if millennia > 0 {
		let mut string = format!("{} millenni", millennia);
		if millennia > 1 {
			string.push_str("um")
		} else {
			string.push('a')
		}
		result.push(string)
	}
	if centuries > 0 {
		let mut string = format!("{} centur", centuries);
		if centuries > 1 {
			string.push_str("ies")
		} else {
			string.push('y')
		}
		result.push(string)
	}
	if decades > 0 {
		let mut string = format!("{} decade", decades);
		if decades > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if years > 0 {
		let mut string = format!("{} year", years);
		if years > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if months > 0 {
		let mut string = format!("{} month", months);
		if months > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if weeks > 0 {
		let mut string = format!("{} week", weeks);
		if weeks > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if days > 0 {
		let mut string = format!("{} day", days);
		if days > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if hours > 0 {
		let string = format!("{}h", hours);
		result.push(string);
	}
	if minutes > 0 {
		let string = format!("{}m", minutes);
		result.push(string);
	}
	if seconds > 0 {
		let string = format!("{}s", seconds);
		result.push(string);
	}
	if millis > 0 {
		let string = format!("{}ms",millis);
		result.push(string);
	}
	if result.is_empty() && micros > 0 {
		let string = format!("{}µs",micros);
		result.push(string);
	}

	result.join(" ")
}

fn tokenize_prompt(raw: &str) -> Vec<PromptTk> {
	let mut chars = raw.chars().peekable();
	let mut tk_text = String::new();
	let mut tokens = vec![];

	while let Some(ch) = chars.next() {
		match ch {
			'\\' => {
				// Push any accumulated text as a token
				if !tk_text.is_empty() {
					tokens.push(PromptTk::Text(std::mem::take(&mut tk_text)));
				}

				// Handle the escape sequence
				if let Some(ch) = chars.next() {
					match ch {
						'w' => tokens.push(PromptTk::Pwd),
						'W' => tokens.push(PromptTk::PwdShort),
						'h' => tokens.push(PromptTk::Hostname),
						'H' => tokens.push(PromptTk::HostnameShort),
						's' => tokens.push(PromptTk::ShellName),
						'u' => tokens.push(PromptTk::Username),
						'$' => tokens.push(PromptTk::PromptSymbol),
						'n' => tokens.push(PromptTk::Text("\n".into())),
						'r' => tokens.push(PromptTk::Text("\r".into())),
						'T' => tokens.push(PromptTk::Runtime),
						'\\' => tokens.push(PromptTk::Text("\\".into())),
						'"' => tokens.push(PromptTk::Text("\"".into())),
						'\'' => tokens.push(PromptTk::Text("'".into())),
						'e' => {
							if chars.next() == Some('[') {
								let mut params = String::new();

								// Collect parameters and final character
								while let Some(ch) = chars.next() {
									match ch {
										'0'..='9' | ';' | '?' | ':' => params.push(ch), // Valid parameter characters
										'A'..='Z' | 'a'..='z' => { // Final character (letter)
											params.push(ch);
											break;
										}
										_ => {
											// Invalid character in ANSI sequence
											tokens.push(PromptTk::Text(format!("\x1b[{params}")));
											break;
										}
									}
								}

								tokens.push(PromptTk::AnsiSeq(format!("\x1b[{params}")));
							} else {
								// Handle case where 'e' is not followed by '['
								tokens.push(PromptTk::Text("\\e".into()));
							}
						}
						'0'..='7' => {
							// Handle octal escape
							let mut octal_str = String::new();
							octal_str.push(ch);

							// Collect up to 2 more octal digits
							for _ in 0..2 {
								if let Some(&next_ch) = chars.peek() {
									if ('0'..='7').contains(&next_ch) {
										octal_str.push(chars.next().unwrap());
									} else {
										break;
									}
								} else {
									break;
								}
							}

							// Parse the octal string into an integer
							if let Ok(octal) = i32::from_str_radix(&octal_str, 8) {
								tokens.push(PromptTk::AsciiOct(octal));
							} else {
								// Fallback: treat as raw text
								tokens.push(PromptTk::Text(format!("\\{octal_str}")));
							}
						}
						_ => {
							// Unknown escape sequence: treat as raw text
							tokens.push(PromptTk::Text(format!("\\{ch}")));
						}
					}
				} else {
					// Handle trailing backslash
					tokens.push(PromptTk::Text("\\".into()));
				}
			}
			_ => {
				// Accumulate non-escape characters
				tk_text.push(ch);
			}
		}
	}

	// Push any remaining text as a token
	if !tk_text.is_empty() {
		tokens.push(PromptTk::Text(tk_text));
	}

	tokens
}

pub fn expand_prompt(raw: &str) -> ShResult<String> {
	let mut tokens = tokenize_prompt(raw).into_iter();
	let mut result = String::new();

	while let Some(token) = tokens.next() {
		match token {
			PromptTk::AsciiOct(_) => todo!(),
			PromptTk::Text(txt) => result.push_str(&txt),
			PromptTk::AnsiSeq(params) => result.push_str(&params),
			PromptTk::Runtime => {
				if let Some(runtime) = write_meta(|m| m.stop_timer()) {
					let runtime_fmt = format_cmd_runtime(runtime);
					result.push_str(&runtime_fmt);
				}
			}
			PromptTk::Pwd => {
				let mut pwd = std::env::var("PWD").unwrap();
				let home = std::env::var("HOME").unwrap();
				if pwd.starts_with(&home) {
					pwd = pwd.replacen(&home, "~", 1);
				}
				result.push_str(&pwd);
			}
			PromptTk::PwdShort => {
				let mut path = std::env::var("PWD").unwrap();
				let home = std::env::var("HOME").unwrap();
				if path.starts_with(&home) {
					path = path.replacen(&home, "~", 1);
				}
				let pathbuf = PathBuf::from(&path);
				let mut segments = pathbuf.iter().count();
				let mut path_iter = pathbuf.iter();
				while segments > 4 {
					path_iter.next();
					segments -= 1;
				}
				let path_rebuilt: PathBuf = path_iter.collect();
				let mut path_rebuilt = path_rebuilt.to_str().unwrap().to_string();
				if path_rebuilt.starts_with(&home) {
					path_rebuilt = path_rebuilt.replacen(&home, "~", 1);
				}
				result.push_str(&path_rebuilt);
			}
			PromptTk::Hostname => {
				let hostname = std::env::var("HOST").unwrap();
				result.push_str(&hostname);
			}
			PromptTk::HostnameShort => todo!(),
			PromptTk::ShellName => result.push_str("fern"),
			PromptTk::Username => {
				let username = std::env::var("USER").unwrap();
				result.push_str(&username);
			}
			PromptTk::PromptSymbol => {
				let uid = std::env::var("UID").unwrap();
				let symbol = if &uid == "0" {
					'#'
				} else {
					'$'
				};
				result.push(symbol);
			}
			PromptTk::ExitCode => todo!(),
			PromptTk::SuccessSymbol => todo!(),
			PromptTk::FailureSymbol => todo!(),
			PromptTk::JobCount => todo!(),
			_ => unimplemented!()
		}
	}

	Ok(result)
}

/// Expand aliases in the given input string
///
/// Recursively calls itself until all aliases are expanded
pub fn expand_aliases(input: String, mut already_expanded: HashSet<String>, log_tab: &LogTab) -> String {
	let mut result = input.clone();
	let tokens: Vec<_> = LexStream::new(Arc::new(input), LexFlags::empty()).collect();
	let mut expanded_this_iter: Vec<String> = vec![];

	for token_result in tokens.into_iter().rev() {
		let Ok(tk) = token_result else { continue };

		if !tk.flags.contains(TkFlags::IS_CMD) { continue }
		if tk.flags.contains(TkFlags::KEYWORD) { continue }

		let raw_tk = tk.span.as_str().to_string();

		if already_expanded.contains(&raw_tk) { continue }

		if let Some(alias) = log_tab.get_alias(&raw_tk) {
			result.replace_range(tk.span.range(), &alias);
			expanded_this_iter.push(raw_tk);
		}
	}

	if expanded_this_iter.is_empty() {
		result
	} else {
		already_expanded.extend(expanded_this_iter);
		expand_aliases(result, already_expanded, log_tab)
	}
}
