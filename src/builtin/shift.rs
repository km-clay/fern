use crate::{jobs::JobBldr, libsh::error::{ShErr, ShErrKind, ShResult}, parse::{NdRule, Node}, state::{self, write_vars}};

use super::setup_builtin;

pub fn shift(node: Node, job: &mut JobBldr) -> ShResult<()> {
	let NdRule::Command { assignments: _, argv } = node.class else {
		unreachable!()
	};

	let (argv,_) = setup_builtin(argv, job, None)?;
	let mut argv = argv.into_iter();

	if let Some((arg,span)) = argv.next() {
		let Ok(count) = arg.parse::<usize>() else {
			return Err(
				ShErr::full(
					ShErrKind::ExecFail,
					"Expected a number in shift args",
					span.into()
				)
			)
		};
		for _ in 0..count {
			write_vars(|v| v.fpop_arg());
		}
	}

	state::set_status(0);
	Ok(())
}
