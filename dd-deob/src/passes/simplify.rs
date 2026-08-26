use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::SPAN;
use oxc_syntax::number::NumberBase;
use oxc_syntax::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use rustc_hash::FxHashMap as HashMap;

use crate::core::{JsValue, MExpr, eval_expr, eval_mba, is_pure, literal_truthy, lower};

pub fn fold_expressions<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let env: HashMap<&str, JsValue> = HashMap::default();
    let mut v = ExprFold { alloc, env, count: 0 };
    v.visit_program(program);
    v.count
}

struct ExprFold<'a> {
    alloc: &'a Allocator,
    env: HashMap<&'a str, JsValue>,
    count: usize,
}

impl<'a> VisitMut<'a> for ExprFold<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        if matches!(expr,
            Expression::BinaryExpression(_)
            | Expression::UnaryExpression(_)
            | Expression::LogicalExpression(_)
            | Expression::ConditionalExpression(_)
        ) {
            if let Some(v) = eval_expr(expr, &self.env) {
                if let Some(new_expr) = v.to_expr(self.alloc) {
                    *expr = new_expr;
                    self.count += 1;
                    return;
                }
            }
        }
        if let Expression::ConditionalExpression(c) = expr {
            if let Some(t) = literal_truthy(&c.test) {
                if is_pure(&c.test) {
                    let ast = AstBuilder::new(self.alloc);
                    let chosen = if t {
                        std::mem::replace(&mut c.consequent, ast.void_0(SPAN))
                    } else {
                        std::mem::replace(&mut c.alternate, ast.void_0(SPAN))
                    };
                    *expr = chosen;
                    self.count += 1;
                }
            }
        }
        if let Expression::LogicalExpression(l) = expr {
            if let Some(t) = literal_truthy(&l.left) {
                let ast = AstBuilder::new(self.alloc);
                match (l.operator, t, is_pure(&l.left)) {
                    (LogicalOperator::And, true, _) => {
                        *expr = std::mem::replace(&mut l.right, ast.void_0(SPAN));
                        self.count += 1;
                    }
                    (LogicalOperator::And, false, true) => {
                        *expr = std::mem::replace(&mut l.left, ast.void_0(SPAN));
                        self.count += 1;
                    }
                    (LogicalOperator::Or, false, _) => {
                        *expr = std::mem::replace(&mut l.right, ast.void_0(SPAN));
                        self.count += 1;
                    }
                    (LogicalOperator::Or, true, true) => {
                        *expr = std::mem::replace(&mut l.left, ast.void_0(SPAN));
                        self.count += 1;
                    }
                    _ => {}
                }
            }
        }
    }
}

pub fn fold_if_statements<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut v = IfFold { alloc, count: 0 };
    v.visit_program(program);
    v.count
}

struct IfFold<'a> { alloc: &'a Allocator, count: usize }

impl<'a> VisitMut<'a> for IfFold<'a> {
    fn visit_statements(&mut self, stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        for s in stmts.iter_mut() {
            oxc_ast_visit::walk_mut::walk_statement(self, s);
        }
        let ast = AstBuilder::new(self.alloc);
        let mut i = 0;
        while i < stmts.len() {
            if !matches!(&stmts[i], Statement::IfStatement(_)) { i += 1; continue; }
            let take = std::mem::replace(&mut stmts[i], ast.statement_empty(SPAN));
            let Statement::IfStatement(boxed_if) = take else { unreachable!() };
            let if_stmt = boxed_if.unbox();
            let test_truthy = if is_pure(&if_stmt.test) { literal_truthy(&if_stmt.test) } else { None };
            match test_truthy {
                Some(true) => { self.replace_with(stmts, i, if_stmt.consequent); self.count += 1; }
                Some(false) => {
                    if let Some(alt) = if_stmt.alternate {
                        self.replace_with(stmts, i, alt);
                        self.count += 1;
                    } else { stmts.remove(i); self.count += 1; }
                }
                None => {
                    let alloc_if = oxc_allocator::Box::new_in(if_stmt, self.alloc);
                    stmts[i] = Statement::IfStatement(alloc_if);
                    i += 1;
                }
            }
        }
        stmts.retain(|s| !matches!(s, Statement::EmptyStatement(_)));
    }
}

impl<'a> IfFold<'a> {
    fn replace_with(&self, stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>, i: usize, new_s: Statement<'a>) {
        if let Statement::BlockStatement(b) = new_s {
            let block = b.unbox();
            stmts.remove(i);
            let mut idx = i;
            for s in block.body { stmts.insert(idx, s); idx += 1; }
        } else {
            stmts[i] = new_s;
        }
    }
}

// ============================================================================
// opaque — invariant fold + linear-fit MBA simplification
// ============================================================================

const PROBES: &[i32] = &[0, 1, -1, 2, -2, i32::MAX, i32::MIN, 42, -7, 0xdead_beefu32 as i32];
const EXTRA_TRIALS: usize = 12;

pub fn opaque<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut v = OpaqueFolder { alloc, count: 0, rng_state: 0x12345678 };
    v.visit_program(program);
    v.count
}

struct OpaqueFolder<'a> {
    alloc: &'a Allocator,
    count: usize,
    rng_state: u64,
}

enum FoldResult {
    Const(JsValue),
    Linear { names: Vec<String>, coeffs: LinearCoeffs },
}

#[derive(Debug, Clone, Copy)]
struct LinearCoeffs { a: i64, b: i64, c: i64 }

impl<'a> OpaqueFolder<'a> {
    fn next_i32(&mut self) -> i32 {
        self.rng_state = self.rng_state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (self.rng_state >> 32) as i32
    }

    fn try_fold(&mut self, expr: &Expression) -> Option<FoldResult> {
        if !matches!(expr, Expression::BinaryExpression(_)) { return None; }
        let mut bitwise = 0;
        count_bitwise(expr, &mut bitwise);
        if bitwise < 1 { return None; }

        let mut frees = Vec::new();
        if !collect_free_idents(expr, &mut frees) { return None; }
        if frees.is_empty() || frees.len() > 4 { return None; }

        let names: Vec<String> = frees.iter().cloned().collect();
        let lowered = lower(expr, &names)?;

        let mut probes_iter: Vec<i32> = PROBES.iter().copied().collect();
        for _ in 0..EXTRA_TRIALS { probes_iter.push(self.next_i32()); }

        let mut canonical: Option<JsValue> = None;
        let mut all_consistent = true;
        for trial in 0..probes_iter.len() {
            let mut args: Vec<JsValue> = Vec::with_capacity(names.len());
            for i in 0..names.len() {
                let p = probes_iter[(trial + i * 31) % probes_iter.len()];
                args.push(JsValue::Num(p as f64));
            }
            let val = eval_mba(&lowered, &args)?;
            if !is_finite_val(&val) { return None; }
            match &canonical {
                None => canonical = Some(val),
                Some(prev) => if !same_val(prev, &val) { all_consistent = false; break; },
            }
        }
        if all_consistent { return canonical.map(FoldResult::Const); }

        if names.len() > 2 || bitwise < 2 { return None; }
        let coeffs = fit_linear_int(&lowered, &names)?;
        Some(FoldResult::Linear { names, coeffs })
    }
}

fn fit_linear_int(body: &MExpr, names: &[String]) -> Option<LinearCoeffs> {
    let zero = vec![JsValue::Num(0.0); names.len()];
    let c = to_int_safe(&eval_mba(body, &zero)?)?;

    let mut x_args = vec![JsValue::Num(0.0); names.len()];
    x_args[0] = JsValue::Num(1.0);
    let a = to_int_safe(&eval_mba(body, &x_args)?)? - c;
    if a.abs() > 12 { return None; }

    let b = if names.len() == 2 {
        let mut y_args = vec![JsValue::Num(0.0); names.len()];
        y_args[1] = JsValue::Num(1.0);
        let b_raw = to_int_safe(&eval_mba(body, &y_args)?)? - c;
        if b_raw.abs() > 12 { return None; }
        b_raw
    } else { 0 };

    let probes: &[i32] = &[0, 1, -1, 2, -2, 7, -7, 100, -100, 12345, -54321, 17, -29, 0x10000, -0x10000];
    if names.len() == 1 {
        for &x in probes {
            let actual = to_int_safe(&eval_mba(body, &[JsValue::Num(x as f64)])?)?;
            let expected = (a as i32).wrapping_mul(x).wrapping_add(c as i32) as i64;
            if (actual as i32) != (expected as i32) { return None; }
        }
    } else {
        for &x in probes {
            for &y in probes {
                let actual = to_int_safe(&eval_mba(body, &[JsValue::Num(x as f64), JsValue::Num(y as f64)])?)?;
                let expected = (a as i32).wrapping_mul(x)
                    .wrapping_add((b as i32).wrapping_mul(y))
                    .wrapping_add(c as i32) as i64;
                if (actual as i32) != (expected as i32) { return None; }
            }
        }
    }
    Some(LinearCoeffs { a, b, c })
}

fn to_int_safe(v: &JsValue) -> Option<i64> {
    match v {
        JsValue::Num(n) if n.is_finite() && n.fract() == 0.0 && n.abs() <= i32::MAX as f64 + 1.0 => Some(*n as i64),
        _ => None,
    }
}

fn build_linear<'a>(ast: &AstBuilder<'a>, c: LinearCoeffs, names: &[String]) -> Expression<'a> {
    let mut terms: Vec<Expression<'a>> = Vec::new();
    let emit_term = |coef: i64, ident: &str| -> Option<Expression<'a>> {
        if coef == 0 { return None; }
        let name = ast.str(ident);
        if coef == 1 { return Some(ast.expression_identifier(SPAN, name)); }
        if coef == -1 {
            let id = ast.expression_identifier(SPAN, name);
            return Some(ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, id));
        }
        let coef_lit = if coef < 0 {
            let inner = ast.expression_numeric_literal(SPAN, -coef as f64, None, NumberBase::Decimal);
            ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, inner)
        } else {
            ast.expression_numeric_literal(SPAN, coef as f64, None, NumberBase::Decimal)
        };
        let id = ast.expression_identifier(SPAN, name);
        Some(ast.expression_binary(SPAN, coef_lit, BinaryOperator::Multiplication, id))
    };
    if let Some(t) = emit_term(c.a, &names[0]) { terms.push(t); }
    if names.len() == 2 {
        if let Some(t) = emit_term(c.b, &names[1]) { terms.push(t); }
    }

    let mut expr: Option<Expression<'a>> = None;
    for term in terms {
        expr = Some(match expr {
            None => term,
            Some(e) => ast.expression_binary(SPAN, e, BinaryOperator::Addition, term),
        });
    }
    if c.c == 0 && expr.is_some() { return expr.unwrap(); }
    let const_lit = if c.c < 0 {
        let inner = ast.expression_numeric_literal(SPAN, -c.c as f64, None, NumberBase::Decimal);
        ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, inner)
    } else {
        ast.expression_numeric_literal(SPAN, c.c as f64, None, NumberBase::Decimal)
    };
    match expr {
        None => const_lit,
        Some(e) => {
            if c.c < 0 {
                let pos = ast.expression_numeric_literal(SPAN, -c.c as f64, None, NumberBase::Decimal);
                ast.expression_binary(SPAN, e, BinaryOperator::Subtraction, pos)
            } else {
                ast.expression_binary(SPAN, e, BinaryOperator::Addition, const_lit)
            }
        }
    }
}

fn is_finite_val(v: &JsValue) -> bool {
    match v {
        JsValue::Num(n) => n.is_finite(),
        JsValue::Bool(_) => true,
        _ => false,
    }
}

fn same_val(a: &JsValue, b: &JsValue) -> bool {
    match (a, b) {
        (JsValue::Num(x), JsValue::Num(y)) => x == y || (x.is_nan() && y.is_nan()),
        (JsValue::Bool(x), JsValue::Bool(y)) => x == y,
        _ => false,
    }
}

fn count_bitwise(e: &Expression, n: &mut u32) {
    match e {
        Expression::BinaryExpression(b) => {
            if matches!(b.operator,
                BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseOR | BinaryOperator::BitwiseXOR
                | BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight | BinaryOperator::ShiftRightZeroFill) {
                *n += 1;
            }
            count_bitwise(&b.left, n);
            count_bitwise(&b.right, n);
        }
        Expression::UnaryExpression(u) if u.operator == UnaryOperator::BitwiseNot => {
            *n += 1;
            count_bitwise(&u.argument, n);
        }
        Expression::UnaryExpression(u) => count_bitwise(&u.argument, n),
        _ => {}
    }
}

fn collect_free_idents(e: &Expression, out: &mut Vec<String>) -> bool {
    match e {
        Expression::NumericLiteral(_) | Expression::BooleanLiteral(_) | Expression::NullLiteral(_) => true,
        Expression::Identifier(id) => {
            let n = id.name.as_str();
            if n == "undefined" || n == "NaN" || n == "Infinity" { return true; }
            if !out.iter().any(|x| x == n) { out.push(n.to_string()); }
            true
        }
        Expression::UnaryExpression(u) => match u.operator {
            UnaryOperator::UnaryPlus | UnaryOperator::UnaryNegation
            | UnaryOperator::BitwiseNot | UnaryOperator::LogicalNot => collect_free_idents(&u.argument, out),
            _ => false,
        },
        Expression::BinaryExpression(b) => {
            let allowed = matches!(b.operator,
                BinaryOperator::Addition | BinaryOperator::Subtraction | BinaryOperator::Multiplication
                | BinaryOperator::Division | BinaryOperator::Remainder | BinaryOperator::Exponential
                | BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseOR | BinaryOperator::BitwiseXOR
                | BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight | BinaryOperator::ShiftRightZeroFill
                | BinaryOperator::Equality | BinaryOperator::Inequality
                | BinaryOperator::StrictEquality | BinaryOperator::StrictInequality
                | BinaryOperator::LessThan | BinaryOperator::LessEqualThan
                | BinaryOperator::GreaterThan | BinaryOperator::GreaterEqualThan);
            if !allowed { return false; }
            collect_free_idents(&b.left, out) && collect_free_idents(&b.right, out)
        }
        Expression::ParenthesizedExpression(p) => collect_free_idents(&p.expression, out),
        _ => false,
    }
}

impl<'a> VisitMut<'a> for OpaqueFolder<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        let Some(v) = self.try_fold(expr) else { return };
        let ast = AstBuilder::new(self.alloc);
        let new_expr = match v {
            FoldResult::Const(JsValue::Num(n)) => {
                if !n.is_finite() { return; }
                if n < 0.0 {
                    let inner = ast.expression_numeric_literal(SPAN, -n, None, NumberBase::Decimal);
                    ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, inner)
                } else {
                    ast.expression_numeric_literal(SPAN, n, None, NumberBase::Decimal)
                }
            }
            FoldResult::Const(JsValue::Bool(b)) => ast.expression_boolean_literal(SPAN, b),
            FoldResult::Linear { names, coeffs } => {
                let new_node = build_linear(&ast, coeffs, &names);
                if approximate_size(&new_node) >= approximate_size(expr) { return; }
                new_node
            }
            _ => return,
        };
        *expr = new_expr;
        self.count += 1;
    }
}

fn approximate_size(e: &Expression) -> usize {
    let mut n = 0usize;
    fn walk(e: &Expression, n: &mut usize) {
        *n += 4;
        match e {
            Expression::BinaryExpression(b) => { walk(&b.left, n); walk(&b.right, n); }
            Expression::LogicalExpression(l) => { walk(&l.left, n); walk(&l.right, n); }
            Expression::UnaryExpression(u) => walk(&u.argument, n),
            Expression::ConditionalExpression(c) => { walk(&c.test, n); walk(&c.consequent, n); walk(&c.alternate, n); }
            Expression::ParenthesizedExpression(p) => walk(&p.expression, n),
            _ => {}
        }
    }
    walk(e, &mut n);
    n
}
