use oxc_allocator::{Allocator, Vec as ArenaVec};
use oxc_ast::builder::AstBuilder;
use oxc_ast_visit::{VisitMut, walk_mut};
use oxc_codegen::Codegen;
use oxc_span::SPAN;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_parser::{ParseOptions, Parser};
use oxc_semantic::{Scoping, SemanticBuilder, SymbolId};
use oxc_span::SourceType;
use oxc_syntax::operator::AssignmentOperator;
use oxc_syntax::scope::ScopeFlags;
use std::collections::{BTreeMap, BTreeSet as Set};

use crate::api::{Api, Width};
use crate::stack::Affine;

#[derive(Debug)]
pub enum Step {
    Read(Width),
    Repeat(usize, Vec<Step>),
}

pub fn trampoline(api: &Api, src: &str) -> Option<(Affine, Affine)> {
    let alloc = Allocator::default();
    let ret = Parser::new(&alloc, src, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    let scoping = SemanticBuilder::new().build(&ret.program).semantic.into_scoping();

    let mut find = Trampoline { api, scoping: &scoping, out: None };
    find.visit_program(&ret.program);
    find.out
}

struct Trampoline<'a> {
    api: &'a Api,
    scoping: &'a Scoping,
    out: Option<(Affine, Affine)>,
}

impl<'a, 'b> Visit<'b> for Trampoline<'a> {
    fn visit_function(&mut self, f: &Function<'b>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        let (Some(body), [FormalParameter { pattern: BindingPattern::BindingIdentifier(p), .. }]) =
            (f.body.as_deref(), f.params.items.as_slice())
        else {
            return;
        };
        let sp = crate::stack::Sp { cell: None, slot: self.api.sp_slot };
        let argc = BTreeMap::from([(p.symbol_id(), 0usize)]);
        for s in &body.statements {
            let Statement::IfStatement(i) = s else { continue };
            let Expression::BinaryExpression(test) = &i.test else { continue };
            let (BinaryOperator::Instanceof, Some(alt)) = (test.operator, &i.alternate) else {
                continue;
            };
            let path = |s| {
                crate::stack::delta(
                    std::slice::from_ref(s),
                    &sp,
                    self.scoping,
                    &self.api.helpers,
                    &argc,
                )
            };
            if let (Some(a), Some(b)) = (path(&i.consequent), path(alt)) {
                self.out = Some((a, b));
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Effect {
    Window,
    Const(usize),
    Many,
    Bytes,
    Str,
    Global(usize),
    Slice,
    Member,
    Callable,
}

pub struct Layout {
    pub steps: Vec<Step>,
    pub string: Option<(usize, usize)>,
    pub jump: Option<(usize, bool)>,
    pub closure: Option<f64>,
    pub delta: Option<crate::stack::Affine>,
    pub invoke: Option<(bool, usize)>,
    pub effects: Vec<Effect>,
    pub branch: Option<(Affine, Affine)>,
    pub template: Option<crate::template::Template>,
    pub ret: bool,
}

pub fn layout(api: &Api, src: &str) -> Layout {
    let alloc = Allocator::default();
    let ret = Parser::new(&alloc, src, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    let scoping = SemanticBuilder::new().build(&ret.program).semantic.into_scoping();

    let mut reads =
        Reads { api, scoping: &scoping, out: Vec::new(), from: BTreeMap::new(), n: 0, string: None, jump: None, closure: None, invoke: None, effects: Vec::new(), loops: 0, news: Set::default(), slices: Set::default(), closures: Set::default() };
    reads.visit_program(&ret.program);
    let sp = crate::stack::Sp { cell: None, slot: api.sp_slot };
    let delta =
        crate::stack::delta(&ret.program.body, &sp, &scoping, &api.helpers, &reads.from);
    let branch = (delta.is_none() && reads.jump.is_some())
        .then(|| {
            let both = crate::stack::arms(
                &ret.program.body,
                &sp,
                &scoping,
                &api.helpers,
                &reads.from,
            )?;
            let first = jumps(&ret.program, api.ip_slot, true)?;
            Some(if first { both } else { (both.1, both.0) })
        })
        .flatten();

    let template = crate::template::of(api, &ret.program, &scoping, &reads.from);

    Layout {
        steps: reads.out,
        string: reads.string,
        jump: reads.jump,
        closure: reads.closure,
        delta,
        invoke: reads.invoke,
        effects: reads.effects,
        branch,
        template,
        ret: restores(&ret.program, api.bp_slot),
    }
}

fn restores(program: &Program, bp_slot: Option<u32>) -> bool {
    struct Look {
        bp_slot: Option<u32>,
        found: bool,
    }
    impl<'a> Visit<'a> for Look {
        fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'a>) {
            walk::walk_assignment_expression(self, a);
            let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
            let Expression::ComputedMemberExpression(cell) = &m.expression else { return };
            let Expression::NumericLiteral(slot) = &cell.expression else { return };
            if Some(slot.value as u32) == self.bp_slot {
                self.found = true;
            }
        }
    }
    let mut look = Look { bp_slot, found: false };
    look.visit_program(program);
    look.found
}

struct Reads<'a> {
    api: &'a Api,
    scoping: &'a Scoping,
    out: Vec<Step>,
    from: BTreeMap<SymbolId, usize>,
    n: usize,
    string: Option<(usize, usize)>,
    jump: Option<(usize, bool)>,
    closure: Option<f64>,
    invoke: Option<(bool, usize)>,
    effects: Vec<Effect>,
    loops: usize,
    news: Set<SymbolId>,
    slices: Set<SymbolId>,
    closures: Set<SymbolId>,
}

impl<'a, 'b> Visit<'b> for Reads<'a> {
    fn visit_function(&mut self, _: &Function<'b>, _: ScopeFlags) {}
    fn visit_arrow_function_expression(&mut self, _: &ArrowFunctionExpression<'b>) {}

    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'b>) {
        walk::walk_assignment_expression(self, a);
        self.produces(a);
        let back = match a.operator {
            AssignmentOperator::Addition => false,
            AssignmentOperator::Subtraction => true,
            _ => return,
        };
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        let Expression::ComputedMemberExpression(cell) = &m.expression else { return };
        let Expression::NumericLiteral(slot) = &cell.expression else { return };
        if Some(slot.value as u32) != self.api.ip_slot {
            return;
        }
        let Expression::Identifier(i) = &a.right else { return };
        if let Some(symbol) = self.scoping.get_reference(i.reference_id()).symbol_id() {
            self.jump = self.from.get(&symbol).map(|at| (*at, back));
        }
    }

    fn visit_new_expression(&mut self, n: &NewExpression<'b>) {
        walk::walk_new_expression(self, n);
        if let Some(at) = self.argc(&n.callee, &n.arguments) {
            self.invoke = Some((true, at));
        }
    }

    fn visit_binary_expression(&mut self, b: &BinaryExpression<'b>) {
        walk::walk_binary_expression(self, b);
        if b.operator != BinaryOperator::Addition {
            return;
        }
        let Expression::ComputedMemberExpression(m) = &b.left else { return };
        let Expression::ComputedMemberExpression(cell) = &m.expression else { return };
        let Expression::NumericLiteral(slot) = &cell.expression else { return };
        if Some(slot.value as u32) == self.api.ip_slot {
            self.closure = Some(0.0);
        }
    }

    fn visit_call_expression(&mut self, c: &CallExpression<'b>) {
        walk::walk_call_expression(self, c);
        if let Some(w) = self.reader(c) {
            self.out.push(Step::Read(w));
            self.n += 1;
        } else if self.slot(c) == self.api.push_global
            && let Some(at) = c.arguments.first().and_then(|a| self.operand(a))
        {
            self.effects.push(Effect::Global(at));
        } else if let Some(at) = self.argc(&c.callee, &c.arguments) {
            self.invoke = Some((false, at));
        } else if self.slot(c) == self.api.strings
            && let [_, index, key] = c.arguments.as_slice()
        {
            self.string = self.operand(index).zip(self.operand(key));
        }
    }

    fn visit_variable_declarator(&mut self, d: &VariableDeclarator<'b>) {
        walk::walk_variable_declarator(self, d);
        if let (BindingPattern::BindingIdentifier(id), Some(init)) = (&d.id, &d.init) {
            match init {
                Expression::NewExpression(_) => {
                    self.news.insert(id.symbol_id());
                }
                Expression::CallExpression(c)
                    if matches!(&c.callee, Expression::StaticMemberExpression(_)) =>
                {
                    self.slices.insert(id.symbol_id());
                }
                Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => {
                    self.closures.insert(id.symbol_id());
                }
                _ => {}
            }
            if let Expression::CallExpression(c) = init
                && self.reader(c).is_some()
            {
                self.from.insert(id.symbol_id(), self.n - 1);
            }
        }
    }

    fn visit_for_statement(&mut self, f: &ForStatement<'b>) {
        self.loops += 1;
        if let Some(init) = &f.init {
            match init {
                ForStatementInit::VariableDeclaration(d) => self.visit_variable_declaration(d),
                _ => self.visit_expression(init.to_expression()),
            }
        }
        let bound = f.test.as_ref().and_then(|t| self.bound(t));
        let outer = std::mem::take(&mut self.out);
        let n = self.n;
        self.visit_statement(&f.body);
        let body = std::mem::replace(&mut self.out, outer);
        if body.is_empty() {
            self.n = n;
            return;
        }
        match bound {
            Some(at) => self.out.push(Step::Repeat(at, body)),
            None => self.out.extend(body),
        }
        self.loops -= 1;
    }
}

impl<'a> Reads<'a> {
    fn reader(&self, c: &CallExpression) -> Option<Width> {
        let Expression::ComputedMemberExpression(m) = &c.callee else { return None };
        let Expression::NumericLiteral(slot) = &m.expression else { return None };
        c.arguments.is_empty().then(|| self.api.readers.get(&(slot.value as u32)).copied())?
    }

    fn produces(&mut self, a: &AssignmentExpression) {
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        let effect = match &m.expression {
            Expression::UpdateExpression(_) => match &a.right {
                Expression::Identifier(i) if i.name == "window" => Effect::Window,
                Expression::Identifier(i) => {
                    let Some(s) = self.scoping.get_reference(i.reference_id()).symbol_id() else {
                        return;
                    };
                    if self.news.contains(&s) {
                        Effect::Bytes
                    } else if self.closures.contains(&s) {
                        Effect::Callable
                    } else if self.slices.contains(&s) {
                        Effect::Slice
                    } else if let Some(at) = self.from.get(&s).copied() {
                        if self.loops > 0 { Effect::Many } else { Effect::Const(at) }
                    } else {
                        return;
                    }
                }
                Expression::CallExpression(c) => match &c.callee {
                    Expression::ComputedMemberExpression(_) if self.reader(c).is_some() => {
                        if self.loops > 0 { Effect::Many } else { Effect::Const(self.n - 1) }
                    }
                    Expression::ComputedMemberExpression(_)
                        if self.slot(c) == self.api.strings =>
                    {
                        Effect::Str
                    }
                    Expression::StaticMemberExpression(_) => Effect::Slice,
                    _ => return,
                },
                Expression::FunctionExpression(_) => Effect::Callable,
                _ => return,
            },
            Expression::BinaryExpression(_) => match &a.right {
                Expression::ComputedMemberExpression(_) => Effect::Member,
                _ => return,
            },
            _ => return,
        };
        self.effects.push(effect);
    }

    fn argc(&self, callee: &Expression, args: &ArenaVec<Argument>) -> Option<usize> {
        let Expression::Identifier(f) = callee else { return None };
        let symbol = self.scoping.get_reference(f.reference_id()).symbol_id()?;
        if self.from.contains_key(&symbol) {
            return None;
        }
        let [arg] = args.as_slice() else { return None };
        self.operand(arg)
    }

    fn slot(&self, c: &CallExpression) -> Option<u32> {
        let Expression::ComputedMemberExpression(m) = &c.callee else { return None };
        let Expression::NumericLiteral(slot) = &m.expression else { return None };
        Some(slot.value as u32)
    }

    fn operand(&self, a: &Argument) -> Option<usize> {
        let Expression::Identifier(i) = a.as_expression()? else { return None };
        let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
        self.from.get(&symbol).copied()
    }

    fn bound(&self, test: &Expression) -> Option<usize> {
        let Expression::BinaryExpression(b) = test else { return None };
        let Expression::Identifier(i) = &b.right else { return None };
        let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
        self.from.get(&symbol).copied()
    }
}

fn jumps(program: &Program, ip_slot: Option<u32>, _first: bool) -> Option<bool> {
    struct Arm {
        ip_slot: Option<u32>,
        found: bool,
    }
    impl<'a> Visit<'a> for Arm {
        fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'a>) {
            walk::walk_assignment_expression(self, a);
            let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
            let Expression::ComputedMemberExpression(cell) = &m.expression else { return };
            if let Expression::NumericLiteral(n) = &cell.expression
                && Some(n.value as u32) == self.ip_slot
            {
                self.found = true;
            }
        }
    }

    struct Choice {
        ip_slot: Option<u32>,
        first: Option<bool>,
    }
    impl<'a> Visit<'a> for Choice {
        fn visit_conditional_expression(&mut self, c: &ConditionalExpression<'a>) {
            walk::walk_conditional_expression(self, c);
            let mut arm = Arm { ip_slot: self.ip_slot, found: false };
            arm.visit_expression(&c.consequent);
            self.first.get_or_insert(arm.found);
        }

        fn visit_if_statement(&mut self, i: &IfStatement<'a>) {
            walk::walk_if_statement(self, i);
            let mut arm = Arm { ip_slot: self.ip_slot, found: false };
            arm.visit_statement(&i.consequent);
            self.first.get_or_insert(arm.found);
        }
    }

    let mut choice = Choice { ip_slot, first: None };
    choice.visit_program(program);
    choice.first
}

pub fn render(api: &Api, src: &str) -> String {
    let alloc = Allocator::default();
    let mut ret = Parser::new(&alloc, src, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    let scoping = SemanticBuilder::new().build(&ret.program).semantic.into_scoping();

    let Some(ctx) = context(&ret.program) else { return src.to_string() };
    let mut names = BTreeMap::new();
    let name = |slot: Option<u32>, as_: &str, names: &mut BTreeMap<u32, String>| {
        if let Some(s) = slot {
            names.insert(s, as_.to_string());
        }
    };
    name(api.image, "M", &mut names);
    name(api.sp_slot, "SP", &mut names);
    name(api.ip_slot, "IP", &mut names);
    name(api.strings, "STR", &mut names);
    name(api.push_global, "PUSHG", &mut names);
    name(api.bp_slot, "BP", &mut names);
    name(api.acc_slot, "ACC", &mut names);
    name(api.result, "RESULT", &mut names);
    name(api.image, "M", &mut names);
    for (slot, cell) in &api.cells {
        if Some(*cell) == api.globals_base {
            names.insert(*slot, "VARS".to_string());
        }
    }

    let mut rewrite = Rewrite {
        ast: AstBuilder::new(&alloc),
        alloc: &alloc,
        scoping: &scoping,
        api,
        ctx,
        names,
        reads: 0,
    };
    rewrite.visit_program(&mut ret.program);
    Codegen::default().build(&ret.program).code
}

fn context(program: &Program) -> Option<SymbolId> {
    for s in &program.body {
        let Statement::VariableDeclaration(d) = s else { continue };
        for v in &d.declarations {
            let (BindingPattern::BindingIdentifier(id), Some(Expression::ComputedMemberExpression(m))) =
                (&v.id, &v.init)
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

struct Rewrite<'a, 'c> {
    ast: AstBuilder<'a>,
    alloc: &'a Allocator,
    scoping: &'c Scoping,
    api: &'c Api,
    ctx: SymbolId,
    names: BTreeMap<u32, String>,
    reads: usize,
}

impl<'a, 'c> VisitMut<'a> for Rewrite<'a, 'c> {
    fn visit_expression(&mut self, e: &mut Expression<'a>) {
        if let Expression::CallExpression(c) = e
            && let Some(slot) = self.slot(&c.callee)
            && c.arguments.is_empty()
            && self.api.readers.contains_key(&slot)
        {
            let n = self.reads;
            self.reads += 1;
            *e = self.ident(&format!("op{n}"));
            return;
        }
        walk_mut::walk_expression(self, e);
        if let Some(slot) = self.slot(e) {
            let name = self.names.get(&slot).cloned().unwrap_or_else(|| format!("s{slot}"));
            *e = self.ident(&name);
        }
    }
}

impl<'a, 'c> Rewrite<'a, 'c> {
    fn slot(&self, e: &Expression) -> Option<u32> {
        let Expression::ComputedMemberExpression(m) = e else { return None };
        let Expression::Identifier(o) = &m.object else { return None };
        let reference = o.reference_id.get()?;
        if self.scoping.get_reference(reference).symbol_id()? != self.ctx {
            return None;
        }
        let Expression::NumericLiteral(n) = &m.expression else { return None };
        Some(n.value as u32)
    }

    fn ident(&self, name: &str) -> Expression<'a> {
        Expression::new_identifier(SPAN, self.alloc.alloc_str(name), &self.ast)
    }
}
