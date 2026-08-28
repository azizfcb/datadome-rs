use oxc_ast::ast::*;
use oxc_semantic::{Scoping, SymbolId};
use oxc_syntax::operator::{AssignmentOperator, BinaryOperator, UpdateOperator};
use std::collections::BTreeMap;

use crate::ops::Effect;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Affine {
    pub c: i64,
    pub terms: Vec<(i64, usize)>,
}

impl Affine {
    pub fn constant(c: i64) -> Self {
        Affine { c, terms: Vec::new() }
    }

    pub fn add(&mut self, other: &Affine, scale: i64) {
        self.c += other.c * scale;
        for (k, at) in &other.terms {
            match self.terms.iter_mut().find(|(_, a)| a == at) {
                Some(t) => t.0 += k * scale,
                None => self.terms.push((k * scale, *at)),
            }
        }
        self.terms.retain(|(k, _)| *k != 0);
    }

    pub fn eval(&self, operands: &[i64]) -> Option<i64> {
        let mut v = self.c;
        for (k, at) in &self.terms {
            v += k * operands.get(*at).copied()?;
        }
        Some(v)
    }
}

pub struct Sp {
    pub cell: Option<i64>,
    pub slot: Option<u32>,
}

impl Sp {
    fn is(&self, e: &Expression) -> bool {
        let Expression::ComputedMemberExpression(m) = e else { return false };
        self.is_member(m)
    }

    fn is_member(&self, m: &ComputedMemberExpression) -> bool {
        match &m.expression {
            Expression::NumericLiteral(n) => Some(n.value as i64) == self.cell,
            Expression::ComputedMemberExpression(inner) => {
                matches!(&inner.expression, Expression::NumericLiteral(n)
                    if Some(n.value as u32) == self.slot)
            }
            _ => false,
        }
    }
}

pub struct Walk<'a> {
    pub sp: &'a Sp,
    pub scoping: &'a Scoping,
    pub helpers: &'a BTreeMap<u32, Affine>,
    pub operand: &'a BTreeMap<SymbolId, usize>,
    now: Affine,
    vars: BTreeMap<SymbolId, Affine>,
    ok: bool,
    prefer: Option<bool>,
}

pub fn delta<'a>(
    body: &[Statement],
    sp: &'a Sp,
    scoping: &'a Scoping,
    helpers: &'a BTreeMap<u32, Affine>,
    operand: &'a BTreeMap<SymbolId, usize>,
) -> Option<Affine> {
    walk(body, sp, scoping, helpers, operand, None)
}

pub fn arms<'a>(
    body: &[Statement],
    sp: &'a Sp,
    scoping: &'a Scoping,
    helpers: &'a BTreeMap<u32, Affine>,
    operand: &'a BTreeMap<SymbolId, usize>,
) -> Option<(Affine, Affine)> {
    let first = walk(body, sp, scoping, helpers, operand, Some(true))?;
    let second = walk(body, sp, scoping, helpers, operand, Some(false))?;
    Some((first, second))
}

fn walk<'a>(
    body: &[Statement],
    sp: &'a Sp,
    scoping: &'a Scoping,
    helpers: &'a BTreeMap<u32, Affine>,
    operand: &'a BTreeMap<SymbolId, usize>,
    prefer: Option<bool>,
) -> Option<Affine> {
    let mut w = Walk {
        sp,
        scoping,
        helpers,
        operand,
        now: Affine::default(),
        vars: BTreeMap::new(),
        ok: true,
        prefer,
    };
    w.statements(body);
    w.ok.then_some(w.now)
}

impl<'a> Walk<'a> {
    fn statements(&mut self, list: &[Statement]) {
        for s in list {
            self.statement(s);
        }
    }

    fn statement(&mut self, s: &Statement) {
        match s {
            Statement::ExpressionStatement(e) => {
                self.expression(&e.expression);
            }
            Statement::VariableDeclaration(d) => self.declaration(d),
            Statement::ReturnStatement(r) => {
                if let Some(a) = &r.argument {
                    self.expression(a);
                }
            }
            Statement::BlockStatement(b) => self.statements(&b.body),
            Statement::TryStatement(t) => {
                self.statements(&t.block.body);
                if let Some(c) = &t.handler {
                    let after = self.now.clone();
                    self.statements(&c.body.body);
                    if self.now != after {
                        self.ok = false;
                    }
                    self.now = after;
                }
                if let Some(f) = &t.finalizer {
                    self.statements(&f.body);
                }
            }
            Statement::EmptyStatement(_) => {}
            Statement::ForStatement(f) => self.repeat(f),
            Statement::ForInStatement(f) => self.bounded(&f.right, &f.body),
            Statement::ForOfStatement(f) => self.bounded(&f.right, &f.body),
            Statement::IfStatement(i) => {
                self.expression(&i.test);
                match self.prefer {
                    Some(true) => self.statement(&i.consequent),
                    Some(false) => {
                        if let Some(alt) = &i.alternate {
                            self.statement(alt);
                        }
                    }
                    None => {
                        let before = self.now.clone();
                        self.statement(&i.consequent);
                        let taken = std::mem::replace(&mut self.now, before);
                        if let Some(alt) = &i.alternate {
                            self.statement(alt);
                        }
                        if self.now != taken {
                            self.ok = false;
                        }
                        self.now = taken;
                    }
                }
            }
            _ => self.ok = false,
        }
    }

    fn declaration(&mut self, d: &VariableDeclaration) {
        for v in &d.declarations {
            let value = v.init.as_ref().map(|i| self.expression(i));
            if let (BindingPattern::BindingIdentifier(id), Some(Some(a))) = (&v.id, value) {
                self.vars.insert(id.symbol_id(), a);
            }
        }
    }

    fn bounded(&mut self, subject: &Expression, body: &Statement) {
        self.expression(subject);
        let before = std::mem::take(&mut self.now);
        self.statement(body);
        if self.now != Affine::default() {
            self.ok = false;
        }
        self.now = before;
    }

    fn repeat(&mut self, f: &ForStatement) {
        if let Some(init) = &f.init {
            match init {
                ForStatementInit::VariableDeclaration(d) => self.declaration(d),
                _ => {
                    self.expression(init.to_expression());
                }
            }
        }
        let bound = f.test.as_ref().and_then(|t| self.bound(t));
        let before = std::mem::take(&mut self.now);
        self.statement(&f.body);
        let body = std::mem::replace(&mut self.now, before);
        if body == Affine::default() {
            return;
        }
        match bound {
            Some(at) if body.terms.is_empty() => {
                self.now.terms.push((body.c, at));
                self.now.terms.retain(|(k, _)| *k != 0);
            }
            _ => self.ok = false,
        }
    }

    fn bound(&self, test: &Expression) -> Option<usize> {
        let Expression::BinaryExpression(b) = test else { return None };
        let Expression::Identifier(i) = &b.right else { return None };
        let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
        self.operand.get(&symbol).copied()
    }

    fn expression(&mut self, e: &Expression) -> Option<Affine> {
        match e {
            Expression::UpdateExpression(u) => self.update(u),
            Expression::AssignmentExpression(a) => self.assign(a),
            Expression::SequenceExpression(s) => {
                let mut last = None;
                for x in &s.expressions {
                    last = self.expression(x);
                }
                last
            }
            Expression::BinaryExpression(b) => {
                let left = self.expression(&b.left);
                let right = self.expression(&b.right);
                let (mut l, r) = (left?, right?);
                match b.operator {
                    BinaryOperator::Addition => l.add(&r, 1),
                    BinaryOperator::Subtraction => l.add(&r, -1),
                    _ => return None,
                }
                Some(l)
            }
            Expression::ConditionalExpression(c) => {
                self.expression(&c.test);
                match self.prefer {
                    Some(true) => {
                        self.expression(&c.consequent);
                    }
                    Some(false) => {
                        self.expression(&c.alternate);
                    }
                    None => {
                        let before = self.now.clone();
                        self.expression(&c.consequent);
                        let taken = std::mem::replace(&mut self.now, before);
                        self.expression(&c.alternate);
                        if self.now != taken {
                            self.ok = false;
                        }
                        self.now = taken;
                    }
                }
                None
            }
            Expression::CallExpression(c) => {
                for a in &c.arguments {
                    if let Some(x) = a.as_expression() {
                        self.expression(x);
                    }
                }
                if let Some(slot) = slot(&c.callee)
                    && let Some(d) = self.helpers.get(&slot)
                {
                    let d = d.clone();
                    self.now.add(&d, 1);
                }
                None
            }
            Expression::NewExpression(n) => {
                for a in &n.arguments {
                    if let Some(x) = a.as_expression() {
                        self.expression(x);
                    }
                }
                None
            }
            Expression::ComputedMemberExpression(m) => {
                if self.sp.is(e) {
                    return Some(self.now.clone());
                }
                self.expression(&m.object);
                self.expression(&m.expression);
                None
            }
            Expression::StaticMemberExpression(m) => {
                self.expression(&m.object);
                None
            }
            Expression::UnaryExpression(u) => {
                self.expression(&u.argument);
                None
            }
            Expression::LogicalExpression(l) => {
                self.expression(&l.left);
                self.expression(&l.right);
                None
            }
            Expression::Identifier(i) => {
                let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
                if let Some(at) = self.operand.get(&symbol) {
                    return Some(Affine { c: 0, terms: vec![(1, *at)] });
                }
                self.vars.get(&symbol).cloned()
            }
            Expression::NumericLiteral(n) => Some(Affine::constant(n.value as i64)),
            Expression::ArrayExpression(a) => {
                for el in &a.elements {
                    if let Some(x) = el.as_expression() {
                        self.expression(x);
                    }
                }
                None
            }
            Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => None,
            _ => None,
        }
    }

    fn update(&mut self, u: &UpdateExpression) -> Option<Affine> {
        let step = if u.operator == UpdateOperator::Increment { 1 } else { -1 };
        let SimpleAssignmentTarget::ComputedMemberExpression(m) = &u.argument else {
            let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &u.argument else {
                return None;
            };
            let symbol = self.scoping.get_reference(id.reference_id()).symbol_id()?;
            let before = self.vars.get(&symbol)?.clone();
            let mut after = before.clone();
            after.add(&Affine::constant(step), 1);
            self.vars.insert(symbol, after.clone());
            return Some(if u.prefix { after } else { before });
        };
        if !self.sp.is_member(m) {
            return None;
        }
        let before = self.now.clone();
        self.now.add(&Affine::constant(step), 1);
        Some(if u.prefix { self.now.clone() } else { before })
    }

    fn assign(&mut self, a: &AssignmentExpression) -> Option<Affine> {
        let value = self.expression(&a.right);
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else {
            if let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left
                && let Some(symbol) = self.scoping.get_reference(id.reference_id()).symbol_id()
                && let Some(v) = value.clone()
            {
                self.vars.insert(symbol, v);
            }
            return value;
        };
        if !self.sp.is_member(m) {
            self.expression(&m.object);
            self.expression(&m.expression);
            return value;
        }
        let v = value?;
        match a.operator {
            AssignmentOperator::Assign => self.now = v,
            AssignmentOperator::Addition => self.now.add(&v, 1),
            AssignmentOperator::Subtraction => self.now.add(&v, -1),
            _ => self.ok = false,
        }
        Some(self.now.clone())
    }
}

fn slot(e: &Expression) -> Option<u32> {
    let Expression::ComputedMemberExpression(m) = e else { return None };
    let Expression::NumericLiteral(n) = &m.expression else { return None };
    Some(n.value as u32)
}

#[derive(Clone, Debug)]
pub enum Val {
    Bytes(Vec<u8>),
    Num(f64),
    Str(String),
    Array(Vec<Val>),
    Window,
    Prop(String),
    Callable(String),
    Unknown,
}

impl Val {
    fn byte(&self) -> Option<u8> {
        match self {
            Val::Num(n) if (0.0..256.0).contains(n) => Some(*n as u8),
            _ => None,
        }
    }
}

pub fn globals(
    insns: &[crate::dis::Insn],
    layouts: &BTreeMap<u8, crate::ops::Layout>,
    trampoline: Option<&(Affine, Affine)>,
    text: &dyn Fn(&crate::dis::Insn) -> Option<String>,
) -> (Vec<Val>, usize) {
    let mut stack: Vec<Val> = Vec::new();
    for (n, i) in insns.iter().enumerate() {
        let Some(layout) = layouts.get(&i.op) else { return (stack, n) };
        let (Some(d), operands) = (&layout.delta, i.numbers()) else { return (stack, n) };
        let Some(step) = d.eval(&operands) else { return (stack, n) };
        if i.target.is_some() {
            return (stack, n);
        }

        if let Some((is_new, at)) = layout.invoke {
            let (Some((new, call)), Some(argc)) = (trampoline, operands.get(at)) else {
                return (stack, n);
            };
            let Some(inner) = (if is_new { new } else { call }).eval(&[*argc]) else {
                return (stack, n);
            };
            let _ = step + inner;
            let Some(made) = construct(&mut stack, (*argc).max(0) as usize, is_new) else {
                return (stack, n);
            };
            stack.push(made);
            continue;
        }

        let run = d
            .terms
            .iter()
            .find(|(k, _)| *k < 0)
            .and_then(|(_, at)| operands.get(*at).copied())
            .unwrap_or(0);
        let many = i.operands.iter().find_map(|o| o.list()).map_or(0, |l| l.len() as i64);
        let known: i64 = layout.effects.iter().map(|e| e.delta(run, many)).sum();

        for _ in 0..(known - step).max(0) {
            if stack.pop().is_none() {
                return (stack, n);
            }
        }
        for _ in 0..(step - known).max(0) {
            stack.push(Val::Unknown);
        }
        for effect in &layout.effects {
            if apply(*effect, run, &mut stack, i, &operands, text).is_none() {
                return (stack, n);
            }
        }
    }
    (stack, insns.len())
}

impl Effect {
    fn delta(&self, run: i64, many: i64) -> i64 {
        match self {
            Effect::Member | Effect::Callable => -1,
            Effect::Slice => 1 - run,
            Effect::Many => many,
            _ => 1,
        }
    }
}

fn apply(
    effect: Effect,
    run: i64,
    stack: &mut Vec<Val>,
    i: &crate::dis::Insn,
    operands: &[i64],
    text: &dyn Fn(&crate::dis::Insn) -> Option<String>,
) -> Option<()> {
    match effect {
        Effect::Window => stack.push(Val::Window),
        Effect::Const(at) => stack.push(Val::Num(*operands.get(at)? as f64)),
        Effect::Str => stack.push(text(i).map_or(Val::Unknown, Val::Str)),
        Effect::Bytes => {
            stack.push(i.operands.iter().find_map(|o| o.bytes()).map_or(Val::Unknown, Val::Bytes))
        }
        Effect::Global(at) => {
            let slot = *operands.get(at)? as usize;
            stack.push(stack.get(slot).cloned().unwrap_or(Val::Unknown));
        }
        Effect::Many => {
            let list = i.operands.iter().find_map(|o| o.list())?;
            stack.extend(list.into_iter().map(Val::Num));
        }
        Effect::Slice => {
            let take = stack.len().checked_sub(run.max(0) as usize)?;
            let mut items: Vec<Val> = stack.split_off(take);
            items.reverse();
            stack.push(Val::Array(items));
        }
        Effect::Member => {
            let (key, obj) = (stack.pop()?, stack.pop()?);
            stack.push(match (obj, key) {
                (Val::Window, Val::Str(k)) => Val::Prop(k),
                _ => Val::Unknown,
            });
        }
        Effect::Callable => {
            let (callee, _this) = (stack.pop()?, stack.pop()?);
            stack.push(match callee {
                Val::Prop(k) => Val::Callable(k),
                _ => Val::Unknown,
            });
        }
    }
    Some(())
}

fn construct(stack: &mut Vec<Val>, argc: usize, is_new: bool) -> Option<Val> {
    let callee = stack.pop()?;
    let at = stack.len().checked_sub(argc)?;
    let args: Vec<Val> = stack.split_off(at);
    if !is_new {
        return Some(Val::Unknown);
    }
    let (Val::Callable(name), [Val::Array(items)]) = (&callee, args.as_slice()) else {
        return Some(Val::Unknown);
    };
    if !name.ends_with("Array") {
        return Some(Val::Unknown);
    }
    match items.iter().map(Val::byte).collect::<Option<Vec<u8>>>() {
        Some(bytes) => Some(Val::Bytes(bytes)),
        None => Some(Val::Unknown),
    }
}
