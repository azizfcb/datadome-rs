use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_semantic::{Scoping, SymbolId};
use oxc_syntax::operator::UnaryOperator;
use oxc_syntax::scope::ScopeFlags;
use std::collections::BTreeMap;

use crate::stack::{Affine, Sp};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Width {
    U8,
    U16,
    U24,
    Const,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Helper {
    PushGlobal,
    StoreGlobal,
    PopToAcc,
    MemberGet,
    MemberSet,
    Nop,
    Dispatch,
}

#[derive(Debug)]
pub struct Api {
    pub readers: BTreeMap<u32, Width>,
    pub strings: Option<u32>,
    pub ip_slot: Option<u32>,
    pub sp_slot: Option<u32>,
    pub helpers: BTreeMap<u32, Affine>,
    pub roles: BTreeMap<u32, Helper>,
    pub push_global: Option<u32>,
    pub image: Option<u32>,
    pub result: Option<u32>,
    pub cells: BTreeMap<u32, i64>,
    pub bp_slot: Option<u32>,
    pub globals_base: Option<i64>,
    pub acc_slot: Option<u32>,
    pub handler_base: i64,
    pub define_op: u8,
    pub halt: Option<i64>,
}

struct SpCell {
    found: Option<i64>,
}

impl<'a> Visit<'a> for SpCell {
    fn visit_computed_member_expression(&mut self, m: &ComputedMemberExpression<'a>) {
        walk::walk_computed_member_expression(self, m);
        let Expression::UpdateExpression(u) = &m.expression else { return };
        let SimpleAssignmentTarget::ComputedMemberExpression(cell) = &u.argument else { return };
        if let Expression::NumericLiteral(n) = &cell.expression {
            self.found.get_or_insert(n.value as i64);
        }
    }
}

struct Starts {
    out: Vec<(i64, i64)>,
    acc: Option<i64>,
}

impl<'a> Visit<'a> for Starts {
    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'a>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        let Expression::NumericLiteral(cell) = &m.expression else { return };
        match &a.right {
            Expression::NumericLiteral(v) => self.out.push((cell.value as i64, v.value as i64)),
            Expression::UnaryExpression(u) if u.operator == UnaryOperator::Void => {
                self.acc = Some(cell.value as i64)
            }
            _ => {}
        }
    }
}

pub fn api(program: &Program, scoping: &Scoping, lo: i64) -> Option<Api> {
    let mut arity = Arity { out: BTreeMap::new() };
    arity.visit_program(program);

    let mut find = Setup { scoping, lo, arity: &arity.out, api: None };
    find.visit_program(program);
    let mut found = find.api?;
    let mut driver = Halt { found: None };
    driver.visit_program(program);
    found.halt = driver.found;
    Some(found)
}

struct Halt {
    found: Option<i64>,
}

impl<'a> Visit<'a> for Halt {
    fn visit_for_statement(&mut self, node: &ForStatement<'a>) {
        walk::walk_for_statement(self, node);
        if self.found.is_some() {
            return;
        }
        let Some(Expression::LogicalExpression(test)) = &node.test else { return };
        let Expression::UnaryExpression(negated) = &test.left else { return };
        if negated.operator != UnaryOperator::LogicalNot {
            return;
        }
        let Expression::ComputedMemberExpression(member) = &negated.argument else { return };
        let Expression::NumericLiteral(cell) = &member.expression else { return };
        self.found = Some(cell.value as i64);
    }
}

struct Arity {
    out: BTreeMap<SymbolId, usize>,
}

impl<'a> Visit<'a> for Arity {
    fn visit_function(&mut self, f: &Function<'a>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        if let Some(id) = &f.id {
            self.out.insert(id.symbol_id(), f.params.items.len());
        }
    }
}

struct Setup<'a> {
    scoping: &'a Scoping,
    lo: i64,
    arity: &'a BTreeMap<SymbolId, usize>,
    api: Option<Api>,
}

impl<'a, 'b> Visit<'b> for Setup<'a> {
    fn visit_function(&mut self, f: &Function<'b>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        if self.api.is_some() {
            return;
        }
        let Some(body) = f.body.as_deref() else { return };
        let mut compiles = Compiles { found: false };
        compiles.visit_function_body(body);
        if !compiles.found {
            return;
        }

        let mut fns = BTreeMap::new();
        for s in &body.statements {
            if let Statement::FunctionDeclaration(d) = s
                && let Some(id) = &d.id
            {
                fns.insert(id.symbol_id(), &**d);
            }
        }

        let mut sp = SpCell { found: None };
        sp.visit_function_body(body);
        let Some(sp_cell) = sp.found else { return };

        let mut starts = Starts { out: Vec::new(), acc: None };
        starts.visit_function_body(body);
        let vars_base = starts.out.iter().find(|(c, _)| *c == sp_cell).map(|(_, v)| *v);
        let bp_cell = vars_base
            .and_then(|b| starts.out.iter().find(|(c, v)| *v == b && *c != sp_cell))
            .map(|(c, _)| *c);

        let mut slots = Slots {
            scoping: self.scoping,
            fns: &fns,
            arity: self.arity,
            sp: Sp { cell: Some(sp_cell), slot: None },
            helpers: BTreeMap::new(),
            roles: BTreeMap::new(),
            push_global: None,
            image: None,
            result: None,
            params: f
                .params
                .items
                .iter()
                .filter_map(|p| match &p.pattern {
                    BindingPattern::BindingIdentifier(b) => Some(b.symbol_id()),
                    _ => None,
                })
                .collect(),
            lo: self.lo,
            readers: BTreeMap::new(),
            strings: None,
            ip_cell: None,
            cells: BTreeMap::new(),
            define_slot: None,
            handler_base: None,
        };
        slots.visit_function_body(body);
        if let (Some(define), Some(base)) = (slots.define_slot, slots.handler_base) {
            let ip_slot = slots.ip_slot();
            let sp_slot = slots.slot_of(sp_cell);
            let bp_slot = bp_cell.and_then(|c| slots.slot_of(c));
            let acc_slot = starts.acc.and_then(|c| slots.slot_of(c));
            self.api = Some(Api {
                readers: slots.readers,
                strings: slots.strings,
                ip_slot,
                sp_slot,
                roles: slots.roles,
                helpers: slots.helpers,
                push_global: slots.push_global,
                image: slots.image,
                result: slots.result,
                bp_slot,
                globals_base: vars_base,
                acc_slot,
                cells: slots.cells,
                handler_base: base,
                define_op: (define - base) as u8,
                halt: None,
            });
        }
    }
}

struct Compiles {
    found: bool,
}

impl<'a> Visit<'a> for Compiles {
    fn visit_new_expression(&mut self, n: &NewExpression<'a>) {
        walk::walk_new_expression(self, n);
        self.found |= matches!(&n.callee, Expression::Identifier(i) if i.name == "Function");
    }
}

struct Slots<'a, 'f, 'b> {
    scoping: &'a Scoping,
    fns: &'f BTreeMap<SymbolId, &'f Function<'b>>,
    arity: &'a BTreeMap<SymbolId, usize>,
    sp: Sp,
    helpers: BTreeMap<u32, Affine>,
    roles: BTreeMap<u32, Helper>,
    push_global: Option<u32>,
    image: Option<u32>,
    result: Option<u32>,
    params: Vec<SymbolId>,
    lo: i64,
    readers: BTreeMap<u32, Width>,
    strings: Option<u32>,
    ip_cell: Option<i64>,
    cells: BTreeMap<u32, i64>,
    define_slot: Option<i64>,
    handler_base: Option<i64>,
}

impl<'a, 'f, 'b> Visit<'b> for Slots<'a, 'f, 'b> {
    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'b>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };

        if let Expression::NumericLiteral(slot) = &m.expression {
            let mut compiles = Compiles { found: false };
            compiles.visit_expression(&a.right);
            if compiles.found {
                self.define_slot = Some(slot.value as i64);
                let mut base = Base { found: None };
                base.visit_expression(&a.right);
                self.handler_base = base.found;
                return;
            }
            if let Some((w, cell)) = self.width(&a.right) {
                self.readers.insert(slot.value as u32, w);
                self.ip_cell.get_or_insert(cell);
            } else if self.decodes_strings(&a.right) {
                self.strings = Some(slot.value as u32);
            } else if let Expression::Identifier(i) = &a.right
                && self
                    .scoping
                    .get_reference(i.reference_id())
                    .symbol_id()
                    .is_some_and(|s| self.params.first() == Some(&s))
            {
                self.image = Some(slot.value as u32);
            } else if let Expression::Identifier(i) = &a.right
                && self
                    .scoping
                    .get_reference(i.reference_id())
                    .symbol_id()
                    .is_some_and(|s| self.params.get(1) == Some(&s))
            {
                self.result = Some(slot.value as u32);
            } else if let Expression::NumericLiteral(cell) = &a.right {
                self.cells.insert(slot.value as u32, cell.value as i64);
            } else if let Some(d) = self.moves(&a.right) {
                let slot = slot.value as u32;
                if let Some(role) = self.role(&a.right, &d) {
                    if role == Helper::PushGlobal {
                        self.push_global = Some(slot);
                    }
                    self.roles.insert(slot, role);
                }
                self.helpers.insert(slot, d);
            }
        }
    }
}

impl<'a, 'f, 'b> Slots<'a, 'f, 'b> {
    fn ip_slot(&self) -> Option<u32> {
        self.slot_of(self.ip_cell?)
    }

    fn slot_of(&self, cell: i64) -> Option<u32> {
        self.cells.iter().find(|(_, v)| **v == cell).map(|(k, _)| *k)
    }

    fn role(&self, e: &Expression, d: &Affine) -> Option<Helper> {
        let arity = match e {
            Expression::FunctionExpression(f) => f.params.items.len(),
            Expression::Identifier(i) => {
                let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
                *self.arity.get(&symbol)?
            }
            _ => return None,
        };
        Some(match (d.c, arity) {
            (1, 1) if self.reads_global(e) => Helper::PushGlobal,
            (0, 1) if self.writes_global(e) => Helper::StoreGlobal,
            (0, 1) => Helper::Nop,
            (0, 0) => Helper::Dispatch,
            (-1, 0) if self.writes_cell(e) => Helper::PopToAcc,
            (-1, 0) => Helper::MemberGet,
            (-2, 0) => Helper::MemberSet,
            _ => return None,
        })
    }

    fn writes_global(&self, e: &Expression) -> bool {
        let Some(body) = self.body(e) else { return false };
        let Expression::FunctionExpression(f) = e else { return false };
        let Some(FormalParameter { pattern: BindingPattern::BindingIdentifier(p), .. }) =
            f.params.items.first()
        else {
            return false;
        };
        let mut find = WritesIndexed { param: p.name.as_str(), found: false };
        find.visit_function_body(body);
        find.found
    }

    fn writes_cell(&self, e: &Expression) -> bool {
        let Some(body) = self.body(e) else { return false };
        let mut find = WritesCell { found: false };
        find.visit_function_body(body);
        find.found
    }

    fn reads_global(&self, e: &Expression) -> bool {
        let Some(Expression::FunctionExpression(f)) = Some(e) else { return false };
        let (Some(body), [FormalParameter { pattern: BindingPattern::BindingIdentifier(p), .. }]) =
            (f.body.as_deref(), f.params.items.as_slice())
        else {
            return false;
        };
        let mut find = Indexed { param: p.name.as_str(), found: false };
        find.visit_function_body(body);
        find.found
    }

    fn moves(&self, e: &Expression) -> Option<Affine> {
        let body = self.body(e)?;
        crate::stack::delta(&body.statements, &self.sp, self.scoping, &BTreeMap::new(), &BTreeMap::new())
    }

    fn body<'e>(&self, e: &'e Expression<'b>) -> Option<&'e FunctionBody<'b>>
    where
        'b: 'e,
        'f: 'e,
    {
        match e {
            Expression::FunctionExpression(f) => f.body.as_deref(),
            Expression::Identifier(i) => {
                let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
                self.fns.get(&symbol)?.body.as_deref()
            }
            _ => None,
        }
    }

    fn decodes_strings(&self, e: &Expression) -> bool {
        let Expression::Identifier(i) = e else { return false };
        let Some(symbol) = self.scoping.get_reference(i.reference_id()).symbol_id() else {
            return false;
        };
        self.arity.get(&symbol) == Some(&3)
    }

    fn width(&self, e: &Expression) -> Option<(Width, i64)> {
        let body = match e {
            Expression::FunctionExpression(f) => f.body.as_deref()?,
            Expression::Identifier(i) => {
                let symbol = self.scoping.get_reference(i.reference_id()).symbol_id()?;
                self.fns.get(&symbol)?.body.as_deref()?
            }
            _ => return None,
        };
        let mut reads =
            Reads { lo: self.lo, count: 0, returns: false, nested: false, cell: None };
        reads.visit_function_body(body);
        if !reads.returns || reads.count == 0 {
            return None;
        }
        let cell = reads.cell?;
        Some(match (reads.nested, reads.count) {
            (true, _) => (Width::Const, cell),
            (_, 1) => (Width::U8, cell),
            (_, 2) => (Width::U16, cell),
            (_, 3) => (Width::U24, cell),
            _ => return None,
        })
    }
}

struct Indexed<'a> {
    param: &'a str,
    found: bool,
}

impl<'a, 'b> Visit<'b> for Indexed<'a> {
    fn visit_computed_member_expression(&mut self, m: &ComputedMemberExpression<'b>) {
        walk::walk_computed_member_expression(self, m);
        if let Expression::BinaryExpression(b) = &m.expression
            && b.operator == BinaryOperator::Addition
            && matches!(&b.left, Expression::NumericLiteral(_))
            && matches!(&b.right, Expression::Identifier(i) if i.name == self.param)
        {
            self.found = true;
        }
    }
}

struct Base {
    found: Option<i64>,
}

impl<'a> Visit<'a> for Base {
    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'a>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        if let Expression::BinaryExpression(b) = &m.expression
            && b.operator == BinaryOperator::Addition
            && let Expression::NumericLiteral(n) = &b.left
        {
            self.found = Some(n.value as i64);
        }
    }
}

fn offsets_code(e: &Expression, lo: i64) -> bool {
    match e {
        Expression::NumericLiteral(n) => n.value as i64 == lo,
        Expression::BinaryExpression(b) => {
            offsets_code(&b.left, lo) || offsets_code(&b.right, lo)
        }
        _ => false,
    }
}

struct Reads {
    lo: i64,
    count: usize,
    returns: bool,
    nested: bool,
    cell: Option<i64>,
}

impl<'a> Visit<'a> for Reads {
    fn visit_return_statement(&mut self, r: &ReturnStatement<'a>) {
        walk::walk_return_statement(self, r);
        self.returns = true;
    }

    fn visit_function(&mut self, f: &Function<'a>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        self.nested = true;
    }

    fn visit_arrow_function_expression(&mut self, a: &ArrowFunctionExpression<'a>) {
        walk::walk_arrow_function_expression(self, a);
        self.nested = true;
    }

    fn visit_computed_member_expression(&mut self, m: &ComputedMemberExpression<'a>) {
        walk::walk_computed_member_expression(self, m);
        if offsets_code(&m.expression, self.lo) {
            self.count += 1;
        } else if let Expression::NumericLiteral(n) = &m.expression {
            self.cell.get_or_insert(n.value as i64);
        }
    }
}

struct WritesIndexed<'a> {
    param: &'a str,
    found: bool,
}

impl<'a, 'b> Visit<'b> for WritesIndexed<'a> {
    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'b>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        if let Expression::BinaryExpression(b) = &m.expression
            && matches!(&b.left, Expression::NumericLiteral(_))
            && matches!(&b.right, Expression::Identifier(i) if i.name == self.param)
        {
            self.found = true;
        }
    }
}

struct WritesCell {
    found: bool,
}

impl<'a> Visit<'a> for WritesCell {
    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'a>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        if matches!(&m.expression, Expression::NumericLiteral(_)) {
            self.found = true;
        }
    }
}
