use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_parser::{ParseOptions, Parser};
use oxc_span::{GetSpan, SourceType};

use crate::api::Api;
use crate::run::{Value, number};

pub struct Reader {
    pub code: Vec<i32>,
    pub base: usize,
}

pub struct State {
    pub memory: Vec<Value>,
    pub result: Rc<RefCell<BTreeMap<String, Value>>>,
    pub reader: Reader,
    pub api: Api,
    pub tags: BTreeMap<u8, crate::konst::Tag>,
    pub locals: Vec<(String, Value)>,
    pub source: String,
    pub returned: Option<Value>,
    pub halted: bool,
    pub steps: usize,
    pub note: Option<String>,
    pub wanted: Vec<String>,
    pub op: u8,
    pub watch: usize,
    pub applies: usize,
    pub spill: BTreeMap<i64, Value>,
    pub newing: bool,
    pub in_new: bool,
    pub clock: f64,
    pub elapsed: f64,
    pub seed: u64,
    pub host: crate::host::Host,
}

impl State {
    pub fn miss(&mut self, note: &str) {
        if !self.wanted.iter().any(|found| found == note) && self.wanted.len() < 60 {
            self.wanted.push(note.to_string());
        }
    }

    pub fn cell(&self, at: usize) -> Value {
        self.memory.get(at).cloned().unwrap_or(Value::Undefined)
    }

    pub fn put(&mut self, at: usize, value: Value) {
        if at >= self.memory.len() {
            self.memory.resize(at + 1, Value::Undefined);
        }
        if self.watch != 0 && at == self.watch {
            let text = value.text();
            eprintln!(
                "watch {at} <- {} {} at step {} ip {} op {}",
                value.kind(),
                &text[..text.len().min(40)],
                self.steps,
                self.ip(),
                self.op
            );
        }
        self.memory[at] = value;
    }

    fn slot(&self, at: usize) -> usize {
        self.cell(at).number() as usize
    }

    pub fn ip(&self) -> usize {
        let cell = self.api.cells.get(&self.api.ip_slot.unwrap_or(0)).copied().unwrap_or(0);
        self.slot(cell as usize)
    }

    pub fn set_ip(&mut self, at: usize) {
        let cell = self.api.cells.get(&self.api.ip_slot.unwrap_or(0)).copied().unwrap_or(0);
        self.put(cell as usize, Value::Num(at as f64));
    }

    fn byte(&mut self) -> i32 {
        let at = self.ip();
        let found = self.reader.code.get(at.wrapping_sub(self.reader.base)).copied().unwrap_or(0);
        self.set_ip(at + 1);
        found
    }

    pub fn read(&mut self, width: usize) -> f64 {
        let mut found = 0i64;
        for _ in 0..width {
            found = (found << 8) | self.byte() as i64;
        }
        found as f64
    }

    pub fn base(&self) -> usize {
        self.api.globals_base.unwrap_or(0) as usize
    }

    pub fn stack(&self) -> usize {
        let cell = self.api.cells.get(&self.api.sp_slot.unwrap_or(0)).copied().unwrap_or(0);
        self.cell(cell as usize).number() as usize
    }

    pub fn set_stack(&mut self, at: usize) {
        let cell = self.api.cells.get(&self.api.sp_slot.unwrap_or(0)).copied().unwrap_or(0);
        self.put(cell as usize, Value::Num(at as f64));
    }

    pub fn acc(&self) -> usize {
        self.api.cells.get(&self.api.acc_slot.unwrap_or(0)).copied().unwrap_or(0) as usize
    }

    pub fn roll(&mut self) -> f64 {
        self.seed ^= self.seed << 13;
        self.seed ^= self.seed >> 7;
        self.seed ^= self.seed << 17;
        (self.seed >> 11) as f64 / (1u64 << 53) as f64
    }

    fn local(&self, name: &str) -> Option<Value> {
        self.locals.iter().rev().find(|(found, _)| found == name).map(|(_, value)| value.clone())
    }

    fn bind(&mut self, name: &str, value: Value) {
        self.locals.push((name.to_string(), value));
    }

    fn set(&mut self, name: &str, value: Value) {
        for entry in self.locals.iter_mut().rev() {
            if entry.0 == name {
                entry.1 = value;
                return;
            }
        }
        self.locals.push((name.to_string(), value));
    }
}

pub struct Handlers {
    sources: BTreeMap<u8, String>,
    cache: RefCell<BTreeMap<String, &'static Program<'static>>>,
}

impl Handlers {
    pub fn new(sources: BTreeMap<u8, String>) -> Self {
        Handlers { sources, cache: RefCell::new(BTreeMap::new()) }
    }

    fn program(&self, source: &str) -> Option<&'static Program<'static>> {
        if let Some(found) = self.cache.borrow().get(source) {
            return Some(found);
        }
        let allocator: &'static Allocator = Box::leak(Box::new(Allocator::default()));
        let text: &'static str = Box::leak(source.to_string().into_boxed_str());
        let parsed = Parser::new(allocator, text, SourceType::mjs())
            .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
            .parse();
        if parsed.panicked {
            return None;
        }
        let leaked: &'static Program<'static> = Box::leak(Box::new(parsed.program));
        self.cache.borrow_mut().insert(source.to_string(), leaked);
        Some(leaked)
    }

    pub fn install(&mut self, op: u8, source: String) {
        self.sources.insert(op, source);
    }

    pub fn has(&self, op: u8) -> bool {
        self.sources.contains_key(&op)
    }

    pub fn run(&self, op: u8, state: &mut State) {
        let Some(source) = self.sources.get(&op) else {
            state.note = Some(format!("no handler {op}"));
            state.halted = true;
            return;
        };
        let wrapped = format!("function h(){{ {source} }}");
        let Some(parsed) = self.program(&wrapped) else {
            state.note = Some(format!("parse {op}"));
            state.halted = true;
            return;
        };
        let previous = std::mem::replace(&mut state.source, wrapped.clone());
        let earlier = std::mem::replace(&mut state.op, op);
        let depth = state.locals.len();
        state.bind(
            "arguments",
            Value::List(Rc::new(RefCell::new(vec![Value::Host("api")]))),
        );
        for statement in &parsed.body {
            if let Statement::FunctionDeclaration(found) = statement {
                if let Some(body) = &found.body {
                    for inner in &body.statements {
                        self.statement(inner, state);
                        if state.halted {
                            break;
                        }
                    }
                }
            }
        }
        state.locals.truncate(depth);
        state.source = previous;
        state.op = earlier;
    }

    pub fn apply(&self, closure: &crate::run::Closure, arguments: &[Value], state: &mut State) -> Value {
        let Some(parsed) = self.program(&closure.source) else { return Value::Undefined };
        let previous = std::mem::replace(&mut state.source, closure.source.clone());
        let outer = std::mem::replace(&mut state.in_new, std::mem::take(&mut state.newing));
        let depth = state.locals.len();
        for (name, value) in &closure.env {
            state.bind(name, value.clone());
        }
        for (at, name) in closure.params.iter().enumerate() {
            state.bind(name, arguments.get(at).cloned().unwrap_or(Value::Undefined));
        }
        state.returned = None;
        for statement in &parsed.body {
            if let Statement::FunctionDeclaration(found) = statement {
                if let Some(body) = &found.body {
                    for inner in &body.statements {
                        self.statement(inner, state);
                        if state.halted || state.returned.is_some() {
                            break;
                        }
                    }
                }
            }
        }
        state.locals.truncate(depth);
        state.source = previous;
        state.in_new = outer;
        state.returned.take().unwrap_or(Value::Undefined)
    }

    fn statement(&self, node: &Statement, state: &mut State) {
        match node {
            Statement::ExpressionStatement(found) => {
                self.value(&found.expression, state);
            }
            Statement::VariableDeclaration(found) => {
                for entry in &found.declarations {
                    let BindingPattern::BindingIdentifier(name) = &entry.id else { continue };
                    let value = match &entry.init {
                        Some(found) => self.value(found, state),
                        None => Value::Undefined,
                    };
                    state.bind(name.name.as_str(), value);
                }
            }
            Statement::ForStatement(found) => {
                let depth = state.locals.len();
                let mut scoped = false;
                if let Some(init) = &found.init {
                    match init {
                        ForStatementInit::VariableDeclaration(declaration) => {
                            scoped = declaration.kind != VariableDeclarationKind::Var;
                            for entry in &declaration.declarations {
                                let BindingPattern::BindingIdentifier(name) = &entry.id else {
                                    continue;
                                };
                                let value = match &entry.init {
                                    Some(found) => self.value(found, state),
                                    None => Value::Undefined,
                                };
                                state.bind(name.name.as_str(), value);
                            }
                        }
                        other => {
                            if let Some(expression) = other.as_expression() {
                                self.value(expression, state);
                            }
                        }
                    }
                }
                let mut rounds = 0;
                loop {
                    rounds += 1;
                    if rounds > 1_000_000 {
                        state.note = Some("loop".to_string());
                        state.halted = true;
                        break;
                    }
                    if let Some(test) = &found.test {
                        if !self.value(test, state).truthy() {
                            break;
                        }
                    }
                    self.statement(&found.body, state);
                    if state.halted {
                        break;
                    }
                    if let Some(update) = &found.update {
                        self.value(update, state);
                    }
                }
                if scoped {
                    state.locals.truncate(depth);
                }
            }
            Statement::BlockStatement(found) => {
                let depth = state.locals.len();
                for inner in &found.body {
                    self.statement(inner, state);
                    if state.halted || state.returned.is_some() {
                        break;
                    }
                }
                state.locals.truncate(depth);
            }
            Statement::IfStatement(found) => {
                if self.value(&found.test, state).truthy() {
                    self.statement(&found.consequent, state);
                } else if let Some(other) = &found.alternate {
                    self.statement(other, state);
                }
            }
            Statement::TryStatement(found) => {
                for inner in &found.block.body {
                    self.statement(inner, state);
                    if state.halted {
                        break;
                    }
                }
            }
            Statement::ReturnStatement(found) => {
                let value = match &found.argument {
                    Some(inner) => self.value(inner, state),
                    None => Value::Undefined,
                };
                state.returned = Some(value);
            }
            Statement::EmptyStatement(_) => {}
            other => {
                state.note = Some(format!("statement {}", label(other)));
                state.halted = true;
            }
        }
    }

    fn value(&self, node: &Expression, state: &mut State) -> Value {
        match node {
            Expression::NumericLiteral(found) => Value::Num(found.value),
            Expression::StringLiteral(found) => Value::Text(Rc::new(found.value.to_string())),
            Expression::BooleanLiteral(found) => Value::Bool(found.value),
            Expression::NullLiteral(_) => Value::Null,
            Expression::Identifier(found) => match found.name.as_str() {
                "undefined" => Value::Undefined,
                name => match state.local(name) {
                    Some(value) => value,
                    None => crate::run::host(name),
                },
            },
            Expression::SequenceExpression(found) => {
                let mut last = Value::Undefined;
                for inner in &found.expressions {
                    last = self.value(inner, state);
                }
                last
            }
            Expression::ParenthesizedExpression(found) => self.value(&found.expression, state),
            Expression::UnaryExpression(found) => {
                let inner = self.value(&found.argument, state);
                match found.operator {
                    UnaryOperator::LogicalNot => Value::Bool(!inner.truthy()),
                    UnaryOperator::UnaryNegation => Value::Num(-inner.number()),
                    UnaryOperator::UnaryPlus => Value::Num(inner.number()),
                    UnaryOperator::BitwiseNot => Value::Num(!(inner.number() as i64 as i32) as f64),
                    _ => Value::Undefined,
                }
            }
            Expression::LogicalExpression(found) => {
                let left = self.value(&found.left, state);
                match found.operator {
                    LogicalOperator::And => {
                        if left.truthy() { self.value(&found.right, state) } else { left }
                    }
                    LogicalOperator::Or => {
                        if left.truthy() { left } else { self.value(&found.right, state) }
                    }
                    LogicalOperator::Coalesce => match left {
                        Value::Undefined | Value::Null => self.value(&found.right, state),
                        other => other,
                    },
                }
            }
            Expression::ConditionalExpression(found) => {
                if self.value(&found.test, state).truthy() {
                    self.value(&found.consequent, state)
                } else {
                    self.value(&found.alternate, state)
                }
            }
            Expression::BinaryExpression(found) => {
                let left = self.value(&found.left, state);
                let right = self.value(&found.right, state);
                arithmetic(found.operator, &left, &right)
            }
            Expression::UpdateExpression(found) => {
                let place = self.place(&found.argument, state);
                let old = self.load(&place, state).number();
                let new = match found.operator {
                    UpdateOperator::Increment => old + 1.0,
                    UpdateOperator::Decrement => old - 1.0,
                };
                self.store(&place, Value::Num(new), state);
                Value::Num(if found.prefix { new } else { old })
            }
            Expression::AssignmentExpression(found) => {
                let place = match &found.left {
                    AssignmentTarget::AssignmentTargetIdentifier(name) => {
                        Place::Name(name.name.to_string())
                    }
                    AssignmentTarget::ComputedMemberExpression(member) => {
                        let base = self.value(&member.object, state);
                        let key = self.value(&member.expression, state);
                        Place::Cell(base, key)
                    }
                    AssignmentTarget::StaticMemberExpression(member) => {
                        let base = self.value(&member.object, state);
                        let key = Value::Text(Rc::new(member.property.name.to_string()));
                        Place::Cell(base, key)
                    }
                    _ => return Value::Undefined,
                };
                let value = match found.operator {
                    AssignmentOperator::Assign => self.value(&found.right, state),
                    other => {
                        let old = self.load(&place, state);
                        let right = self.value(&found.right, state);
                        let op = match other {
                            AssignmentOperator::Addition => BinaryOperator::Addition,
                            AssignmentOperator::Subtraction => BinaryOperator::Subtraction,
                            AssignmentOperator::Multiplication => BinaryOperator::Multiplication,
                            AssignmentOperator::Division => BinaryOperator::Division,
                            AssignmentOperator::Remainder => BinaryOperator::Remainder,
                            AssignmentOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
                            AssignmentOperator::BitwiseOR => BinaryOperator::BitwiseOR,
                            AssignmentOperator::BitwiseXOR => BinaryOperator::BitwiseXOR,
                            AssignmentOperator::ShiftLeft => BinaryOperator::ShiftLeft,
                            AssignmentOperator::ShiftRight => BinaryOperator::ShiftRight,
                            AssignmentOperator::ShiftRightZeroFill => {
                                BinaryOperator::ShiftRightZeroFill
                            }
                            _ => return Value::Undefined,
                        };
                        arithmetic(op, &old, &right)
                    }
                };
                self.store(&place, value.clone(), state);
                value
            }
            Expression::ComputedMemberExpression(found) => {
                let base = self.value(&found.object, state);
                let key = self.value(&found.expression, state);
                if matches!(base, Value::Undefined | Value::Null) {
                    let span = found.object.span();
                    let text = state
                        .source
                        .get(span.start as usize..span.end as usize)
                        .unwrap_or("?")
                        .to_string();
                    state.miss(&format!("ip {} op {} :: {text}[{}]", state.ip(), state.op, key.text()));
                }
                self.load(&Place::Cell(base, key), state)
            }
            Expression::StaticMemberExpression(found) => {
                let base = self.value(&found.object, state);
                if matches!(base, Value::Undefined | Value::Null) {
                    let span = found.object.span();
                    let text = state
                        .source
                        .get(span.start as usize..span.end as usize)
                        .unwrap_or("?")
                        .to_string();
                    state.miss(&format!("ip {} op {} :: {text}.{}", state.ip(), state.op, found.property.name));
                }
                let key = Value::Text(Rc::new(found.property.name.to_string()));
                self.load(&Place::Cell(base, key), state)
            }
            Expression::ThisExpression(_) => {
                if state.in_new { Value::Host("newtarget") } else { Value::Undefined }
            }
            Expression::ArrayExpression(found) => {
                let mut items = Vec::new();
                for element in &found.elements {
                    match element.as_expression() {
                        Some(inner) => items.push(self.value(inner, state)),
                        None => items.push(Value::Undefined),
                    }
                }
                Value::List(Rc::new(RefCell::new(items)))
            }
            Expression::ObjectExpression(found) => {
                let mut entries = BTreeMap::new();
                for property in &found.properties {
                    let ObjectPropertyKind::ObjectProperty(property) = property else { continue };
                    let Some(name) = property.key.static_name() else { continue };
                    let value = self.value(&property.value, state);
                    entries.insert(name.to_string(), value);
                }
                Value::Map(Rc::new(RefCell::new(entries)))
            }
            Expression::FunctionExpression(found) => {
                let span = match &found.body {
                    Some(body) => body.span,
                    None => found.span,
                };
                let text = state
                    .source
                    .get(span.start as usize..span.end as usize)
                    .unwrap_or("{}")
                    .to_string();
                let params = found
                    .params
                    .items
                    .iter()
                    .filter_map(|item| match &item.pattern {
                        BindingPattern::BindingIdentifier(name) => Some(name.name.to_string()),
                        _ => None,
                    })
                    .collect();
                let env = state.locals.clone();
                Value::Closure(Rc::new(crate::run::Closure {
                    source: format!("function h() {text}"),
                    params,
                    env,
                }))
            }
            Expression::NewExpression(found) => {
                let mut arguments = Vec::new();
                for entry in &found.arguments {
                    if let Some(expression) = entry.as_expression() {
                        arguments.push(self.value(expression, state));
                    }
                }
                let callee = self.value(&found.callee, state);
                let name = match (&callee, &found.callee) {
                    (Value::Host(found), _) => found.to_string(),
                    (_, Expression::Identifier(found)) => found.name.to_string(),
                    _ => String::new(),
                };
                if let Value::Closure(found) = &callee {
                    state.newing = true;
                    return self.apply(found, &arguments, state);
                }
                if name == "Date" {
                    let time = match arguments.first() {
                        Some(found) if !matches!(found, Value::Undefined) => found.number(),
                        _ => {
                            state.clock += 1.0;
                            state.clock
                        }
                    };
                    return crate::dom::date(time);
                }
                if let Value::Bound(inner) = &callee {
                    let mut list = inner.1.clone();
                    list.extend(arguments);
                    let label = match &inner.0 {
                        Value::Host(found) => found.to_string(),
                        Value::Method(_, found) => found.to_string(),
                        _ => String::new(),
                    };
                    return crate::run::construct(&label, list);
                }
                crate::run::construct(&name, arguments)
            }
            Expression::CallExpression(found) => {
                let mut arguments = Vec::new();
                for entry in &found.arguments {
                    if let Some(expression) = entry.as_expression() {
                        arguments.push(self.value(expression, state));
                    }
                }
                let callee = match &found.callee {
                    Expression::ComputedMemberExpression(member) => {
                        let base = self.value(&member.object, state);
                        let key = self.value(&member.expression, state);
                        (base, key)
                    }
                    Expression::StaticMemberExpression(member) => {
                        let base = self.value(&member.object, state);
                        (base, Value::Text(Rc::new(member.property.name.to_string())))
                    }
                    Expression::Identifier(name) => (
                        Value::Undefined,
                        match state.local(name.name.as_str()) {
                            Some(found) => found,
                            None => crate::run::host(name.name.as_str()),
                        },
                    ),
                    _ => (Value::Undefined, Value::Undefined),
                };
                self.invoke(callee, arguments, state)
            }
            other => {
                state.note = Some(format!("expression {}", kind(other)));
                state.halted = true;
                Value::Undefined
            }
        }
    }

    fn place(&self, node: &SimpleAssignmentTarget, state: &mut State) -> Place {
        match node {
            SimpleAssignmentTarget::AssignmentTargetIdentifier(name) => {
                Place::Name(name.name.to_string())
            }
            SimpleAssignmentTarget::ComputedMemberExpression(member) => {
                let base = self.value(&member.object, state);
                let key = self.value(&member.expression, state);
                Place::Cell(base, key)
            }
            _ => Place::Name(String::new()),
        }
    }

    fn load(&self, place: &Place, state: &mut State) -> Value {
        match place {
            Place::Name(name) => state.local(name).unwrap_or(Value::Undefined),
            Place::Cell(base, key) => crate::run::member(base, key, state),
        }
    }

    fn store(&self, place: &Place, value: Value, state: &mut State) {
        match place {
            Place::Name(name) => state.set(name, value),
            Place::Cell(base, key) => crate::run::assign(base, key, value, state),
        }
    }

    fn invoke(&self, callee: (Value, Value), arguments: Vec<Value>, state: &mut State) -> Value {
        let target = match &callee.0 {
            Value::Undefined => callee.1.clone(),
            other => crate::run::member(other, &callee.1, state),
        };
        if let Value::Closure(found) = &target {
            state.applies += 1;
            return self.apply(found, &arguments, state);
        }
        let name = callee.1.text();
        if matches!(callee.0, Value::Host("bindfn")) && name == "apply" {
            let mut list: Vec<Value> = match arguments.get(1) {
                Some(Value::List(items)) => items.borrow().clone(),
                _ => Vec::new(),
            };
            if !list.is_empty() {
                list.remove(0);
            }
            let target = arguments.first().cloned().unwrap_or(Value::Undefined);
            return Value::Bound(Rc::new((target, list)));
        }
        if matches!(name.as_str(), "apply" | "call") {
            let list: Vec<Value> = if name == "apply" {
                match arguments.get(1) {
                    Some(Value::List(items)) => items.borrow().clone(),
                    _ => Vec::new(),
                }
            } else {
                arguments.iter().skip(1).cloned().collect()
            };
            match &callee.0 {
                Value::Closure(found) => return self.apply(found, &list, state),
                Value::Method(host, method) => {
                    return crate::run::native(host, method, list, state);
                }
                Value::Prop(owner, method) => {
                    return crate::run::method(owner, method, &list, state);
                }
                Value::Bound(inner) => {
                    let mut all = inner.1.clone();
                    all.extend(list);
                    return self.invoke((Value::Undefined, inner.0.clone()), all, state);
                }
                Value::Host(host) => {
                    return crate::run::native(host, "", list, state);
                }
                _ => {}
            }
        }
        if let Value::Closure(found) = &callee.0 {
            if callee.1.text() == "apply" {
                let list = match arguments.get(1) {
                    Some(Value::List(items)) => items.borrow().clone(),
                    _ => Vec::new(),
                };
                return self.apply(found, &list, state);
            }
            if callee.1.text() == "call" {
                let rest: Vec<Value> = arguments.iter().skip(1).cloned().collect();
                return self.apply(found, &rest, state);
            }
        }
        crate::run::call(callee.0, callee.1, arguments, state)
    }
}

enum Place {
    Name(String),
    Cell(Value, Value),
}

fn arithmetic(op: BinaryOperator, left: &Value, right: &Value) -> Value {
    match op {
        BinaryOperator::Addition => match (left, right) {
            (Value::Text(_), _) | (_, Value::Text(_)) => {
                Value::Text(Rc::new(format!("{}{}", left.text(), right.text())))
            }
            _ => Value::Num(left.number() + right.number()),
        },
        BinaryOperator::Subtraction => Value::Num(left.number() - right.number()),
        BinaryOperator::Multiplication => Value::Num(left.number() * right.number()),
        BinaryOperator::Division => Value::Num(left.number() / right.number()),
        BinaryOperator::Remainder => Value::Num(left.number() % right.number()),
        BinaryOperator::BitwiseAnd => {
            Value::Num(((left.number() as i64 as i32) & (right.number() as i64 as i32)) as f64)
        }
        BinaryOperator::BitwiseOR => {
            Value::Num(((left.number() as i64 as i32) | (right.number() as i64 as i32)) as f64)
        }
        BinaryOperator::BitwiseXOR => {
            Value::Num(((left.number() as i64 as i32) ^ (right.number() as i64 as i32)) as f64)
        }
        BinaryOperator::ShiftLeft => Value::Num(
            (left.number() as i64 as i32).wrapping_shl(right.number() as u32 & 31) as f64,
        ),
        BinaryOperator::ShiftRight => Value::Num(
            (left.number() as i64 as i32).wrapping_shr(right.number() as u32 & 31) as f64,
        ),
        BinaryOperator::ShiftRightZeroFill => Value::Num(
            (left.number() as i64 as u32).wrapping_shr(right.number() as u32 & 31) as f64,
        ),
        BinaryOperator::LessThan => Value::Bool(left.number() < right.number()),
        BinaryOperator::GreaterThan => Value::Bool(left.number() > right.number()),
        BinaryOperator::LessEqualThan => Value::Bool(left.number() <= right.number()),
        BinaryOperator::GreaterEqualThan => Value::Bool(left.number() >= right.number()),
        BinaryOperator::Equality => Value::Bool(crate::run::same(left, right)),
        BinaryOperator::Inequality => Value::Bool(!crate::run::same(left, right)),
        BinaryOperator::StrictEquality => Value::Bool(crate::run::strict(left, right)),
        BinaryOperator::StrictInequality => Value::Bool(!crate::run::strict(left, right)),
        BinaryOperator::Instanceof => Value::Bool(matches!(left, Value::Host("newtarget"))),
        _ => Value::Undefined,
    }
}

fn kind(node: &Expression) -> &'static str {
    match node {
        Expression::FunctionExpression(_) => "function",
        Expression::ArrowFunctionExpression(_) => "arrow",
        Expression::ThisExpression(_) => "this",
        Expression::ArrayExpression(_) => "array",
        Expression::ObjectExpression(_) => "object",
        Expression::TemplateLiteral(_) => "template",
        Expression::AwaitExpression(_) => "await",
        _ => "other",
    }
}

fn label(node: &Statement) -> &'static str {
    match node {
        Statement::TryStatement(_) => "try",
        Statement::ReturnStatement(_) => "return",
        Statement::WhileStatement(_) => "while",
        Statement::SwitchStatement(_) => "switch",
        Statement::ThrowStatement(_) => "throw",
        Statement::FunctionDeclaration(_) => "function",
        _ => "other",
    }
}

pub fn text(value: f64) -> String {
    number(value)
}
