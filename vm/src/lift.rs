use std::collections::BTreeMap;
use std::fmt::Write;

use crate::dis::{Insn, Operand, Value};
use crate::ops::Layout;
use crate::template::Node;

pub struct Lifter<'a> {
    layouts: &'a BTreeMap<u8, Layout>,
    trampoline: Option<&'a (crate::stack::Affine, crate::stack::Affine)>,
    stack: Vec<Node>,
    temps: usize,
}

impl<'a> Lifter<'a> {
    pub fn new(
        layouts: &'a BTreeMap<u8, Layout>,
        trampoline: Option<&'a (crate::stack::Affine, crate::stack::Affine)>,
    ) -> Self {
        Lifter { layouts, trampoline, stack: Vec::new(), temps: 0 }
    }

    pub fn reset(&mut self) {
        self.stack.clear();
    }

    pub fn load(&mut self, state: &[Node]) {
        self.stack = state.to_vec();
    }

    pub fn save(&self) -> Vec<Node> {
        self.stack.clone()
    }

    pub fn step(&mut self, i: &Insn, text: Option<&str>, out: &mut String) {
        let mut line = String::new();
        self.run(i, text, &mut line);
        let depth = self.stack.len();
        let _ = writeln!(out, "{:>8} {:>4} {:>3}  {line}", i.at, depth, i.op);
    }

    fn run(&mut self, i: &Insn, text: Option<&str>, line: &mut String) {
        let Some(layout) = self.layouts.get(&i.op) else {
            let _ = write!(line, "<no handler> {}", operands(i));
            return;
        };
        let operands_n = i.numbers();

        if let Some((is_new, at)) = layout.invoke {
            let argc = operands_n.get(at).copied().unwrap_or(0).max(0);
            let Some((new, call)) = self.trampoline else {
                let _ = write!(line, "call/{argc} <trampoline unknown>");
                self.stack.clear();
                return;
            };
            let Some(net) = (if is_new { new } else { call }).eval(&[argc]) else {
                let _ = write!(line, "call/{argc} <effect unknown>");
                self.stack.clear();
                return;
            };
            let pops = (1 - net).max(0) as usize;
            let callee = self.stack.pop().unwrap_or(Node::Unknown);
            let take = self.stack.len().saturating_sub(pops);
            let mut taken: Vec<Node> = self.stack.split_off(take);
            let keep = taken.len().saturating_sub(argc.max(0) as usize);
            let args: Vec<Node> = taken.split_off(keep);
            let made = if is_new {
                Node::New(Box::new(callee), args)
            } else {
                Node::Call(Box::new(callee), args)
            };
            let id = self.temps;
            self.temps += 1;
            let _ = write!(line, "t{id} = {made}");
            self.stack.push(Node::Temp(id));
            return;
        }

        if layout.effects.contains(&crate::ops::Effect::Callable) {
            let callee = self.stack.pop().unwrap_or(Node::Unknown);
            self.stack.pop();
            let _ = write!(line, "bind {callee}");
            self.stack.push(callee);
            return;
        }

        if layout.ret {
            let value = match i.operands.first().and_then(|o| o.int()) {
                Some(n) if !layout.steps.is_empty() => format!("l[{n}]"),
                _ => self.stack.pop().unwrap_or(Node::Unknown).to_string(),
            };
            self.stack.clear();
            let _ = write!(line, "return {value}");
            return;
        }

        if layout.closure.is_some() {
            let captured: Vec<String> = i
                .operands
                .iter()
                .skip(2)
                .map(|o| match o {
                    Operand::List(v) => {
                        let names: Vec<String> =
                            v.iter().map(|x| format!("l[{x}]")).collect();
                        names.join(", ")
                    }
                    other => other.to_string(),
                })
                .collect();
            let argc = i.operands.first().and_then(|o| o.int()).unwrap_or(0);
            self.stack.push(Node::Text(format!("closure/{argc}")));
            let _ = write!(line, "push closure/{argc} [{}]", captured.join(", "));
            return;
        }

        let Some(t) = &layout.template else {
            let effect = layout
                .delta
                .as_ref()
                .or(layout.branch.as_ref().map(|(_, fallthrough)| fallthrough))
                .and_then(|d| d.eval(&operands_n));
            self.plain(i, effect, line);
            return;
        };

        let Some(sp) = t.sp else {
            let d = layout
                .delta
                .as_ref()
                .or(layout.branch.as_ref().map(|(_, fallthrough)| fallthrough))
                .and_then(|d| d.eval(&operands_n));
            self.plain(i, d, line);
            return;
        };

        let base = self.stack.len() as i64;
        let mut values: BTreeMap<i64, Node> = BTreeMap::new();
        for (k, v) in &t.stack {
            values.insert(*k, self.subst(v, base, i, text));
        }
        let writes: Vec<(Node, Node)> = t
            .writes
            .iter()
            .map(|(target, value)| {
                (self.subst(target, base, i, text), self.subst(value, base, i, text))
            })
            .collect();

        let end = base + sp;
        if end < 0 {
            self.stack.clear();
            let _ = write!(line, "op{} {} [underflow]", i.op, operands(i));
            return;
        }
        self.stack.resize(end.max(0) as usize, Node::Unknown);

        let mut parts: Vec<String> = Vec::new();
        for (target, value) in &writes {
            parts.push(format!("{target} = {value}"));
        }
        for (k, v) in values {
            let at = base + k;
            if (0..end).contains(&at) {
                parts.push(format!("s{at} = {v}"));
                self.stack[at as usize] = v;
            }
        }
        if parts.is_empty() {
            let _ = write!(line, "op{} {} [sp{sp:+}]", i.op, operands(i));
        } else {
            let _ = write!(line, "{}", parts.join(", "));
        }
        self.jump(i, line);
    }

    fn plain(&mut self, i: &Insn, effect: Option<i64>, line: &mut String) {
        let branch = self.layouts.get(&i.op).and_then(|l| l.branch.as_ref()).is_some();
        let Some(d) = effect else {
            self.stack.clear();
            let _ = write!(line, "op{} {} [sp ?]", i.op, operands(i));
            self.jump(i, line);
            return;
        };
        let top = self.stack.last().cloned().unwrap_or(Node::Unknown);
        let end = self.stack.len() as i64 + d;
        self.stack.resize(end.max(0) as usize, Node::Unknown);
        match (i.target, branch, d) {
            (Some(t), true, _) => {
                let _ = write!(line, "if ({top}) skip -> {t}");
            }
            (Some(t), false, -1) => {
                let _ = write!(line, "if (!{top}) goto {t}");
            }
            (Some(t), false, 0) => {
                let _ = write!(line, "goto {t}");
            }
            (Some(t), false, _) => {
                let _ = write!(line, "goto {t} [sp{d:+}]");
            }
            (None, _, d) if d < 0 => {
                let _ = write!(line, "drop {}", -d);
            }
            (None, _, d) if d > 0 => {
                let _ = write!(line, "s{} .. s{} = {} values", end - d, end - 1, d);
            }
            _ => {
                let _ = write!(line, "op{} {}", i.op, operands(i));
            }
        }
    }

    fn jump(&self, i: &Insn, line: &mut String) {
        if let Some(t) = i.target {
            let _ = write!(line, "   -> {t}");
        }
    }

    fn subst(&self, n: &Node, base: i64, i: &Insn, text: Option<&str>) -> Node {
        match n {
            Node::Slot(k) => {
                let at = base + k;
                if at < 0 || at as usize >= self.stack.len() {
                    return Node::Unknown;
                }
                self.stack[at as usize].clone()
            }
            Node::Operand(at) => match i.operands.get(*at) {
                Some(Operand::Int(v)) => Node::Num(*v as f64),
                Some(Operand::Const(Value::Num(v))) => Node::Num(*v),
                Some(Operand::Const(v)) => Node::Text(v.to_string()),
                _ => Node::Unknown,
            },
            Node::Str => text.map_or(Node::Unknown, |t| Node::Text(t.to_string())),
            Node::Global(x) => Node::Global(Box::new(self.subst(x, base, i, text))),
            Node::Local(x) => Node::Local(Box::new(self.subst(x, base, i, text))),
            Node::Bin(op, a, b) => Node::Bin(
                op.clone(),
                Box::new(self.subst(a, base, i, text)),
                Box::new(self.subst(b, base, i, text)),
            ),
            Node::Un(op, a) => Node::Un(op.clone(), Box::new(self.subst(a, base, i, text))),
            Node::Index(a, b) => Node::Index(
                Box::new(self.subst(a, base, i, text)),
                Box::new(self.subst(b, base, i, text)),
            ),
            Node::New(c, args) => Node::New(
                Box::new(self.subst(c, base, i, text)),
                args.iter().map(|a| self.subst(a, base, i, text)).collect(),
            ),
            Node::Call(c, args) => Node::Call(
                Box::new(self.subst(c, base, i, text)),
                args.iter().map(|a| self.subst(a, base, i, text)).collect(),
            ),
            other => other.clone(),
        }
    }
}

fn operands(i: &Insn) -> String {
    let list: Vec<String> = i
        .operands
        .iter()
        .map(|o| match o {
            Operand::List(v) => format!("[{} items]", v.len()),
            other => other.to_string(),
        })
        .collect();
    list.join(", ")
}
