use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_semantic::{ReferenceFlags, SemanticBuilder, SymbolFlags};
use oxc_syntax::node::NodeId;
use oxc_syntax::operator::UnaryOperator;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::core::JsValue;

pub fn inline_const<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let plan: HashMap<NodeId, JsValue> = {
        let ret = SemanticBuilder::new().build(program);
        if !ret.errors.is_empty() { return 0; }
        let semantic = ret.semantic;
        let scoping = semantic.scoping();

        let mut consts: HashMap<oxc_syntax::symbol::SymbolId, JsValue> = HashMap::default();
        for symbol_id in 0..scoping.symbols_len() {
            let sym = oxc_syntax::symbol::SymbolId::from_usize(symbol_id);
            let flags = scoping.symbol_flags(sym);
            if !flags.intersects(SymbolFlags::Variable | SymbolFlags::FunctionScopedVariable | SymbolFlags::BlockScopedVariable) {
                continue;
            }
            let refs = scoping.get_resolved_references(sym).collect::<Vec<_>>();
            let mut writes = 0usize;
            let mut reads = 0usize;
            for r in &refs {
                if r.flags().contains(ReferenceFlags::Write) { writes += 1; }
                if r.flags().contains(ReferenceFlags::Read) { reads += 1; }
            }
            if writes > 0 || reads == 0 { continue; }
            let decl_node = semantic.symbol_declaration(sym);
            let init_value = literal_init_for(decl_node, scoping.symbol_name(sym));
            let Some(value) = init_value else { continue };
            if !should_inline(&value, reads) { continue; }
            consts.insert(sym, value);
        }
        if consts.is_empty() { return 0; }

        let mut plan: HashMap<NodeId, JsValue> = HashMap::default();
        for (&sym, val) in &consts {
            for r in scoping.get_resolved_references(sym) {
                if !r.flags().contains(ReferenceFlags::Read) { continue; }
                plan.insert(r.node_id(), val.clone());
            }
        }
        plan
    };

    if plan.is_empty() { return 0; }
    let mut v = Inliner { alloc, plan, count: 0 };
    v.visit_program(program);
    v.count
}

fn literal_init_for(node: &oxc_semantic::AstNode, _name: &str) -> Option<JsValue> {
    let kind = node.kind();
    if let Some(decl) = kind.as_variable_declarator() {
        if let Some(init) = &decl.init {
            return literal_value(init);
        }
    }
    None
}

fn literal_value(e: &Expression) -> Option<JsValue> {
    match e {
        Expression::NumericLiteral(n) => Some(JsValue::Num(n.value)),
        Expression::StringLiteral(s) => Some(JsValue::Str(s.value.as_str().to_string())),
        Expression::BooleanLiteral(b) => Some(JsValue::Bool(b.value)),
        Expression::NullLiteral(_) => Some(JsValue::Null),
        Expression::Identifier(id) if id.name.as_str() == "undefined" => Some(JsValue::Undefined),
        Expression::UnaryExpression(u) if u.operator == UnaryOperator::UnaryNegation => {
            if let Expression::NumericLiteral(n) = &u.argument {
                return Some(JsValue::Num(-n.value));
            }
            None
        }
        _ => None,
    }
}

fn should_inline(value: &JsValue, ref_count: usize) -> bool {
    match value {
        JsValue::Num(_) | JsValue::Bool(_) | JsValue::Null | JsValue::Undefined => true,
        JsValue::Str(s) => ref_count <= 1 || s.len() <= 24,
        _ => false,
    }
}

struct Inliner<'a> {
    alloc: &'a Allocator,
    plan: HashMap<NodeId, JsValue>,
    count: usize,
}

impl<'a> VisitMut<'a> for Inliner<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        if let Expression::Identifier(id) = expr {
            let nid = id.node_id.get();
            if let Some(v) = self.plan.get(&nid).cloned() {
                if let Some(new_expr) = v.to_expr(self.alloc) {
                    *expr = new_expr;
                    self.count += 1;
                }
            }
        }
    }
}

pub fn unused_vars<'a>(program: &mut Program<'a>, _alloc: &'a Allocator) -> usize {
    let plan: HashSet<NodeId> = {
        let ret = SemanticBuilder::new().build(program);
        if !ret.errors.is_empty() { return 0; }
        let semantic = ret.semantic;
        let scoping = semantic.scoping();

        let mut to_remove: HashSet<NodeId> = HashSet::default();
        for symbol_id in 0..scoping.symbols_len() {
            let sym = oxc_syntax::symbol::SymbolId::from_usize(symbol_id);
            let flags = scoping.symbol_flags(sym);
            if !flags.intersects(SymbolFlags::Variable | SymbolFlags::FunctionScopedVariable | SymbolFlags::BlockScopedVariable) {
                continue;
            }
            let refs = scoping.get_resolved_references(sym).collect::<Vec<_>>();
            if !refs.is_empty() { continue; }
            let decl_node = semantic.symbol_declaration(sym);
            if let Some(decl) = decl_node.kind().as_variable_declarator() {
                if let Some(init) = &decl.init {
                    if !is_simple_init(init) { continue; }
                } else if !is_pure_uninit() { continue; }
                if let BindingPattern::BindingIdentifier(bi) = &decl.id {
                    to_remove.insert(bi.node_id.get());
                }
            }
        }
        to_remove
    };
    if plan.is_empty() { return 0; }

    let removed = strip_program(program, &plan);
    removed
}

fn is_simple_init(e: &Expression) -> bool {
    matches!(e,
        Expression::NumericLiteral(_) | Expression::StringLiteral(_)
        | Expression::BooleanLiteral(_) | Expression::NullLiteral(_)
        | Expression::Identifier(_) | Expression::ArrayExpression(_)
        | Expression::ObjectExpression(_)
    ) || matches!(e, Expression::UnaryExpression(u) if matches!(&u.argument, Expression::NumericLiteral(_) | Expression::BooleanLiteral(_)))
}

fn is_pure_uninit() -> bool { true }

fn strip_program<'a>(program: &mut Program<'a>, drop: &HashSet<NodeId>) -> usize {
    let mut count = 0usize;
    strip_stmts(&mut program.body, drop, &mut count);
    count
}

fn strip_stmts<'a>(stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>, drop: &HashSet<NodeId>, count: &mut usize) {
    for s in stmts.iter_mut() {
        strip_stmt(s, drop, count);
    }
    let mut i = 0;
    while i < stmts.len() {
        if let Statement::VariableDeclaration(vd) = &mut stmts[i] {
            vd.declarations.retain(|d| {
                if let BindingPattern::BindingIdentifier(bi) = &d.id {
                    if drop.contains(&bi.node_id.get()) { *count += 1; return false; }
                }
                true
            });
            if vd.declarations.is_empty() { stmts.remove(i); continue; }
        }
        i += 1;
    }
}

fn strip_stmt<'a>(s: &mut Statement<'a>, drop: &HashSet<NodeId>, count: &mut usize) {
    match s {
        Statement::BlockStatement(b) => strip_stmts(&mut b.body, drop, count),
        Statement::IfStatement(s) => {
            strip_stmt(&mut s.consequent, drop, count);
            if let Some(a) = &mut s.alternate { strip_stmt(a, drop, count); }
        }
        Statement::ForStatement(f) => strip_stmt(&mut f.body, drop, count),
        Statement::WhileStatement(w) => strip_stmt(&mut w.body, drop, count),
        Statement::DoWhileStatement(w) => strip_stmt(&mut w.body, drop, count),
        Statement::SwitchStatement(s) => for c in s.cases.iter_mut() {
            strip_stmts(&mut c.consequent, drop, count);
        },
        Statement::TryStatement(t) => {
            strip_stmts(&mut t.block.body, drop, count);
            if let Some(h) = &mut t.handler { strip_stmts(&mut h.body.body, drop, count); }
            if let Some(f) = &mut t.finalizer { strip_stmts(&mut f.body, drop, count); }
        }
        Statement::LabeledStatement(s) => strip_stmt(&mut s.body, drop, count),
        Statement::FunctionDeclaration(fd) => {
            if let Some(b) = &mut fd.body { strip_stmts(&mut b.statements, drop, count); }
        }
        _ => {}
    }
}
