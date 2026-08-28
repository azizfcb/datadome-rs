use oxc_ast::ast::*;
use oxc_semantic::{Scoping, SymbolId};
use oxc_syntax::operator::{AssignmentOperator, UpdateOperator};
use std::collections::BTreeMap;

use crate::api::{Api, Helper};

#[derive(Clone, Debug)]
pub enum Node {
    Slot(i64),
    Operand(usize),
    Str,
    Window,
    Global(Box<Node>),
    Local(Box<Node>),
    Acc,
    Result,
    Cell(u32),
    Num(f64),
    Text(String),
    Bin(String, Box<Node>, Box<Node>),
    Un(String, Box<Node>),
    Index(Box<Node>, Box<Node>),
    New(Box<Node>, Vec<Node>),
    Call(Box<Node>, Vec<Node>),
    Temp(usize),
    Unknown,
}

#[derive(Debug)]
pub struct Template {
    pub sp: Option<i64>,
    pub stack: BTreeMap<i64, Node>,
    pub writes: Vec<(Node, Node)>,
}

pub fn of(
    api: &Api,
    program: &Program,
    scoping: &Scoping,
    operand: &BTreeMap<SymbolId, usize>,
) -> Option<Template> {
    let mut m = Machine {
        api,
        scoping,
        operand,
        sp: 0,
        stack: BTreeMap::new(),
        env: BTreeMap::new(),
        writes: Vec::new(),
        ok: true,
        dynamic: false,
        ctx: context(program),
    };
    for s in &program.body {
        m.statement(s);
    }
    m.ok.then(|| Template {
        sp: (!m.dynamic).then_some(m.sp),
        stack: if m.dynamic { BTreeMap::new() } else { m.stack },
        writes: m.writes,
    })
}

#[derive(Clone, Debug)]
enum V {
    Ctx,
    Role(u32),
    Addr(i64),
    Frame,
    FrameAddr(Node),
    VarAddr(Node),
    Node(Node),
}

struct Machine<'a> {
    api: &'a Api,
    scoping: &'a Scoping,
    operand: &'a BTreeMap<SymbolId, usize>,
    sp: i64,
    stack: BTreeMap<i64, Node>,
    env: BTreeMap<SymbolId, V>,
    writes: Vec<(Node, Node)>,
    ok: bool,
    dynamic: bool,
    ctx: Option<SymbolId>,
}

impl<'a> Machine<'a> {
    fn statement(&mut self, s: &Statement) {
        match s {
            Statement::ExpressionStatement(e) => {
                self.eval(&e.expression);
            }
            Statement::VariableDeclaration(d) => {
                for v in &d.declarations {
                    let value = v.init.as_ref().map(|i| self.eval(i));
                    if let (BindingPattern::BindingIdentifier(id), Some(value)) = (&v.id, value) {
                        self.env.insert(id.symbol_id(), value);
                    }
                }
            }
            Statement::EmptyStatement(_) => {}
            Statement::ForStatement(_)
            | Statement::ForInStatement(_)
            | Statement::ForOfStatement(_)
            | Statement::IfStatement(_)
            | Statement::BlockStatement(_)
            | Statement::TryStatement(_)
            | Statement::ReturnStatement(_) => self.dynamic = true,
            _ => self.ok = false,
        }
    }

    fn eval(&mut self, e: &Expression) -> V {
        match e {
            Expression::NumericLiteral(n) => V::Node(Node::Num(n.value)),
            Expression::StringLiteral(s) => V::Node(Node::Text(s.value.to_string())),
            Expression::Identifier(i) => {
                if i.name == "window" {
                    return V::Node(Node::Window);
                }
                let Some(symbol) = i.reference_id.get().and_then(|r| {
                    self.scoping.get_reference(r).symbol_id()
                }) else {
                    return V::Node(Node::Unknown);
                };
                if Some(symbol) == self.ctx {
                    return V::Ctx;
                }
                if let Some(at) = self.operand.get(&symbol) {
                    return V::Node(Node::Operand(*at));
                }
                self.env.get(&symbol).cloned().unwrap_or(V::Node(Node::Unknown))
            }
            Expression::SequenceExpression(s) => {
                let mut last = V::Node(Node::Unknown);
                for x in &s.expressions {
                    last = self.eval(x);
                }
                last
            }
            Expression::UpdateExpression(u) => self.update(u),
            Expression::AssignmentExpression(a) => self.assign(a),
            Expression::ComputedMemberExpression(m) => self.read(m),
            Expression::StaticMemberExpression(m) => {
                let object = match self.eval(&m.object) {
                    V::Role(k) => self.cell(k),
                    other => node(other),
                };
                V::Node(Node::Index(
                    Box::new(object),
                    Box::new(Node::Text(m.property.name.to_string())),
                ))
            }
            Expression::BinaryExpression(b) => {
                let left = self.eval(&b.left);
                let right = self.eval(&b.right);
                self.binary(b.operator.as_str(), left, right)
            }
            Expression::UnaryExpression(u) => {
                let v = self.value(&u.argument);
                V::Node(Node::Un(u.operator.as_str().to_string(), Box::new(v)))
            }
            Expression::CallExpression(c) => self.call(c),
            Expression::NewExpression(n) => {
                let callee = self.value(&n.callee);
                let args = n.arguments.iter().filter_map(|a| a.as_expression()).collect::<Vec<_>>();
                let args = args.into_iter().map(|a| self.value(a)).collect();
                V::Node(Node::New(Box::new(callee), args))
            }
            Expression::LogicalExpression(l) => {
                self.eval(&l.left);
                self.eval(&l.right);
                V::Node(Node::Unknown)
            }
            Expression::ConditionalExpression(c) => {
                self.eval(&c.test);
                self.eval(&c.consequent);
                self.eval(&c.alternate);
                self.dynamic = true;
                V::Node(Node::Unknown)
            }
            Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => {
                V::Node(Node::Unknown)
            }
            Expression::ArrayExpression(_) | Expression::ObjectExpression(_) => {
                V::Node(Node::Unknown)
            }
            Expression::BooleanLiteral(b) => V::Node(Node::Num(b.value as u8 as f64)),
            Expression::NullLiteral(_) => V::Node(Node::Text("null".to_string())),
            _ => {
                self.dynamic = true;
                V::Node(Node::Unknown)
            }
        }
    }

    fn value(&mut self, e: &Expression) -> Node {
        match self.eval(e) {
            V::Node(n) => n,
            _ => Node::Unknown,
        }
    }

    fn binary(&mut self, op: &str, left: V, right: V) -> V {
        if op == "+" || op == "-" {
            let sign = if op == "+" { 1 } else { -1 };
            if let (V::Addr(k), V::Node(Node::Num(n))) = (&left, &right) {
                return V::Addr(k + sign * *n as i64);
            }
            if let (V::Role(slot), V::Node(x)) = (&left, &right)
                && Some(*slot) == self.vars_slot()
            {
                return V::VarAddr(x.clone());
            }
            if let (V::Frame, V::Node(x)) = (&left, &right) {
                return V::FrameAddr(x.clone());
            }
        }
        let (l, r) = (node(left), node(right));
        V::Node(Node::Bin(op.to_string(), Box::new(l), Box::new(r)))
    }

    fn vars_slot(&self) -> Option<u32> {
        let base = self.api.globals_base?;
        self.api.cells.iter().find(|(_, v)| **v == base).map(|(k, _)| *k)
    }

    fn slot(&mut self, e: &Expression) -> Option<u32> {
        let V::Role(k) = self.eval(e) else { return None };
        Some(k)
    }

    fn read(&mut self, m: &ComputedMemberExpression) -> V {
        let object = self.eval(&m.object);
        if matches!(object, V::Ctx) {
            if let Expression::NumericLiteral(k) = &m.expression {
                return V::Role(k.value as u32);
            }
            return V::Node(Node::Unknown);
        }
        if !matches!(&object, V::Role(k) if Some(*k) == self.api.image) {
            let index = self.value(&m.expression);
            return V::Node(Node::Index(Box::new(node(object)), Box::new(index)));
        }
        match self.eval(&m.expression) {
            V::Role(k) if Some(k) == self.api.sp_slot => V::Addr(self.sp),
            V::Role(k) if Some(k) == self.api.acc_slot => V::Node(Node::Acc),
            V::Role(k) if Some(k) == self.api.bp_slot => V::Frame,
            V::Addr(k) => V::Node(self.at(k)),
            V::VarAddr(x) => V::Node(Node::Global(Box::new(x))),
            V::FrameAddr(x) => V::Node(Node::Local(Box::new(x))),
            _ => V::Node(Node::Unknown),
        }
    }

    fn at(&self, k: i64) -> Node {
        if self.dynamic {
            return Node::Unknown;
        }
        self.stack.get(&k).cloned().unwrap_or(Node::Slot(k))
    }

    fn update(&mut self, u: &UpdateExpression) -> V {
        let step = if u.operator == UpdateOperator::Increment { 1 } else { -1 };
        let SimpleAssignmentTarget::ComputedMemberExpression(m) = &u.argument else {
            let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &u.argument else {
                return V::Node(Node::Unknown);
            };
            let Some(symbol) =
                id.reference_id.get().and_then(|r| self.scoping.get_reference(r).symbol_id())
            else {
                return V::Node(Node::Unknown);
            };
            let Some(V::Addr(before)) = self.env.get(&symbol).cloned() else {
                return V::Node(Node::Unknown);
            };
            self.env.insert(symbol, V::Addr(before + step));
            return V::Addr(if u.prefix { before + step } else { before });
        };
        let object = self.eval(&m.object);
        let is_sp = matches!(&object, V::Role(k) if Some(*k) == self.api.image)
            && self.slot(&m.expression) == self.api.sp_slot;
        if !is_sp {
            return V::Node(Node::Unknown);
        }
        let before = self.sp;
        self.sp += step;
        V::Addr(if u.prefix { self.sp } else { before })
    }

    fn assign(&mut self, a: &AssignmentExpression) -> V {
        if let AssignmentTarget::StaticMemberExpression(m) = &a.left {
            let object = match self.eval(&m.object) {
                V::Role(k) => self.cell(k),
                other => node(other),
            };
            let value = self.value(&a.right);
            let target = Node::Index(
                Box::new(object),
                Box::new(Node::Text(m.property.name.to_string())),
            );
            self.writes.push((target, value.clone()));
            return V::Node(value);
        }
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else {
            let value = self.eval(&a.right);
            if let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left
                && let Some(symbol) =
                    id.reference_id.get().and_then(|r| self.scoping.get_reference(r).symbol_id())
            {
                self.env.insert(symbol, value.clone());
            }
            return value;
        };

        let object = self.eval(&m.object);
        let target = if matches!(&object, V::Role(k) if Some(*k) == self.api.image) {
            self.eval(&m.expression)
        } else {
            let key = self.value(&m.expression);
            V::Node(Node::Index(Box::new(node(object)), Box::new(key)))
        };

        let value = match a.operator {
            AssignmentOperator::Assign => self.value(&a.right),
            _ => {
                let old = self.load(&target);
                let right = self.value(&a.right);
                let op = a.operator.as_str().trim_end_matches('=').to_string();
                Node::Bin(op, Box::new(old), Box::new(right))
            }
        };

        match target {
            V::Addr(k) => {
                self.stack.insert(k, value.clone());
            }
            V::Role(k) if Some(k) == self.api.sp_slot => match self.eval(&a.right) {
                V::Addr(to) if a.operator == AssignmentOperator::Assign => self.sp = to,
                _ => self.dynamic = true,
            },
            V::Role(k) if Some(k) == self.api.acc_slot => {
                self.writes.push((Node::Acc, value.clone()));
            }
            V::VarAddr(x) => self.writes.push((Node::Global(Box::new(x)), value.clone())),
            V::FrameAddr(x) => self.writes.push((Node::Local(Box::new(x)), value.clone())),
            V::Node(n) => self.writes.push((n, value.clone())),
            V::Role(k) if Some(k) == self.api.ip_slot => self.dynamic = true,
            V::Role(k) => self.writes.push((self.cell(k), value.clone())),
            _ => self.dynamic = true,
        }
        V::Node(value)
    }

    fn cell(&self, k: u32) -> Node {
        if Some(k) == self.api.acc_slot {
            Node::Acc
        } else if Some(k) == self.api.result {
            Node::Result
        } else {
            Node::Cell(k)
        }
    }

    fn load(&mut self, target: &V) -> Node {
        match target {
            V::Addr(k) => self.at(*k),
            V::VarAddr(x) => Node::Global(Box::new(x.clone())),
            V::FrameAddr(x) => Node::Local(Box::new(x.clone())),
            V::Node(n) => n.clone(),
            _ => Node::Unknown,
        }
    }

    fn call(&mut self, c: &CallExpression) -> V {
        let args: Vec<Node> = c
            .arguments
            .iter()
            .filter_map(|a| a.as_expression())
            .map(|a| self.value(a))
            .collect();

        let Some(slot) = self.slot(&c.callee) else {
            let callee = self.value(&c.callee);
            return V::Node(Node::Call(Box::new(callee), args));
        };

        if self.api.readers.contains_key(&slot) {
            return V::Node(Node::Unknown);
        }
        if Some(slot) == self.api.strings {
            return V::Node(Node::Str);
        }
        match self.api.roles.get(&slot) {
            Some(Helper::PushGlobal) => {
                let value = Node::Global(Box::new(args.first().cloned().unwrap_or(Node::Unknown)));
                self.push(value);
            }
            Some(Helper::StoreGlobal) => {
                let key = args.first().cloned().unwrap_or(Node::Unknown);
                let top = self.at(self.sp - 1);
                self.writes.push((Node::Global(Box::new(key)), top));
            }
            Some(Helper::PopToAcc) => {
                let top = self.at(self.sp - 1);
                self.sp -= 1;
                self.writes.push((Node::Acc, top));
            }
            Some(Helper::MemberGet) => {
                let key = self.at(self.sp - 1);
                let object = self.at(self.sp - 2);
                self.sp -= 1;
                self.stack.insert(self.sp - 1, Node::Index(Box::new(object), Box::new(key)));
            }
            Some(Helper::MemberSet) => {
                let key = self.at(self.sp - 1);
                let object = self.at(self.sp - 2);
                let value = self.at(self.sp - 3);
                self.sp -= 2;
                self.writes.push((Node::Index(Box::new(object), Box::new(key)), value));
            }
            Some(Helper::Nop) | Some(Helper::Dispatch) => {}
            None => self.ok = false,
        }
        V::Node(Node::Unknown)
    }

    fn push(&mut self, value: Node) {
        self.stack.insert(self.sp, value);
        self.sp += 1;
    }
}

fn context(program: &Program) -> Option<SymbolId> {
    for s in &program.body {
        let Statement::VariableDeclaration(d) = s else { continue };
        for v in &d.declarations {
            let (
                BindingPattern::BindingIdentifier(id),
                Some(Expression::ComputedMemberExpression(m)),
            ) = (&v.id, &v.init)
            else {
                continue;
            };
            if matches!(&m.object, Expression::Identifier(o) if o.name == "arguments") {
                return Some(id.symbol_id());
            }
        }
    }
    None
}

fn node(v: V) -> Node {
    match v {
        V::Node(n) => n,
        _ => Node::Unknown,
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Node::Slot(k) => write!(f, "s{k}"),
            Node::Operand(i) => write!(f, "op{i}"),
            Node::Str => f.write_str("str"),
            Node::Window => f.write_str("window"),
            Node::Global(x) => write!(f, "g[{x}]"),
            Node::Local(x) => write!(f, "l[{x}]"),
            Node::Acc => f.write_str("acc"),
            Node::Result => f.write_str("result"),
            Node::Cell(k) => write!(f, "cell{k}"),
            Node::Num(n) => write!(f, "{n}"),
            Node::Text(t) => write!(f, "{t:?}"),
            Node::Bin(op, a, b) => write!(f, "({a} {op} {b})"),
            Node::Un(op, a) => write!(f, "{op}{a}"),
            Node::Index(a, b) => write!(f, "{a}[{b}]"),
            Node::New(c, args) => write!(f, "new {c}({})", list(args)),
            Node::Call(c, args) => write!(f, "{c}({})", list(args)),
            Node::Temp(i) => write!(f, "t{i}"),
            Node::Unknown => f.write_str("?"),
        }
    }
}

fn list(args: &[Node]) -> String {
    args.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut parts = Vec::new();
        for (k, v) in &self.stack {
            if self.sp.is_none_or(|sp| *k < sp) {
                parts.push(format!("s{k} = {v}"));
            }
        }
        for (target, value) in &self.writes {
            parts.push(format!("{target} := {value}"));
        }
        match self.sp {
            Some(sp) => write!(f, "sp{sp:+}  {}", parts.join("; ")),
            None => write!(f, "sp?  {}", parts.join("; ")),
        }
    }
}
