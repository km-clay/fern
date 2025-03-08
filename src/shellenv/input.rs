use crate::prelude::*;

#[derive(Clone,Debug,PartialEq)]
pub struct InputMan {
	input: Option<String>,
	saved_input: Option<String>,
	spans: Vec<Rc<RefCell<Span>>>,
	saved_spans: Vec<Span>
}

impl InputMan {
	pub fn new() -> Self {
		Self { input: None, saved_input: None, spans: vec![], saved_spans: vec![] }
	}
	pub fn clear(&mut self) {
		*self = Self::new();
	}
	pub fn new_input(&mut self, input: &str) {
		self.input = Some(input.to_string())
	}
	pub fn get_input(&self) -> Option<&String> {
		self.input.as_ref()
	}
	pub fn get_input_mut(&mut self) -> Option<&mut String> {
		self.input.as_mut()
	}
	pub fn save_state(&mut self) {
		self.saved_input = self.input.clone();
		self.saved_spans.clear();
		for span in &self.spans {
			self.saved_spans.push(span.borrow().clone());
		}
	}
	pub fn load_state(&mut self) {
		if self.saved_input.is_some() {
			self.input = self.saved_input.take();

			for (span, saved_span) in self.spans.iter_mut().zip(self.saved_spans.iter()) {
				*span.borrow_mut() = saved_span.clone();
			}

			self.saved_spans.clear();
		}
	}
	pub fn new_span(&mut self, start: usize, end: usize) -> Rc<RefCell<Span>> {
		if let Some(_input) = &self.input {
			let span = Rc::new(RefCell::new(Span::new(start, end)));
			self.spans.push(span.clone());
			span
		} else {
			Rc::new(RefCell::new(Span::new(0,0)))
		}
	}
	pub fn remove_span(&mut self, span: Rc<RefCell<Span>>) {
		if let Some(idx) = self.spans.iter().position(|iter_span| *iter_span == span) {
			self.spans.remove(idx);
		}
	}
	pub fn spans_mut(&mut self) -> &mut Vec<Rc<RefCell<Span>>> {
		&mut self.spans
	}
	pub fn clamp(&self, span: Rc<RefCell<Span>>) {
		let mut span = span.borrow_mut();
		if let Some(input) = &self.input {
			span.clamp_start(input.len());
			span.clamp_end(input.len());
		}
	}
	pub fn clamp_all(&self) {
		for span in &self.spans {
			self.clamp(span.clone());
		}
	}
	pub fn get_slice(&self, span: Rc<RefCell<Span>>) -> Option<&str> {
		let span = span.borrow();
		let mut start = span.start();
		let end = span.end();
		if start > end {
			start = end;
		}

		self.input.as_ref().map(|s| &s[start..end])
	}
}
