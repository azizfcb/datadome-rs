use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::SPAN;

use crate::core::is_valid_ident;

pub fn hex_decode<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut v = HexVisit { alloc, count: 0 };
    v.visit_program(program);
    v.count
}

struct HexVisit<'a> { alloc: &'a Allocator, count: usize }

impl<'a> VisitMut<'a> for HexVisit<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        if let Expression::TemplateLiteral(t) = expr {
            if t.expressions.is_empty() && t.quasis.len() == 1 {
                let raw = t.quasis[0].value.raw.as_str();
                if let Some(s) = decode_hex_only(raw) {
                    let ast = AstBuilder::new(self.alloc);
                    *expr = ast.expression_string_literal(SPAN, ast.str(&s), None);
                    self.count += 1;
                }
            }
        }
    }
    fn visit_string_literal(&mut self, lit: &mut StringLiteral<'a>) {
        if let Some(raw) = &lit.raw {
            let inner = raw.as_str();
            if inner.len() >= 2 {
                let body = &inner[1..inner.len() - 1];
                if let Some(s) = decode_hex_only(body) {
                    let ast = AstBuilder::new(self.alloc);
                    lit.value = ast.str(&s);
                    lit.raw = None;
                    self.count += 1;
                }
            }
        }
    }
}

fn decode_hex_only(s: &str) -> Option<String> {
    if s.is_empty() { return None; }
    let bytes = s.as_bytes();
    let mut i = 0;
    let mut out = String::with_capacity(s.len() / 4);
    while i < bytes.len() {
        if i + 3 >= bytes.len() { return None; }
        if bytes[i] != b'\\' || bytes[i + 1] != b'x' { return None; }
        let h1 = (bytes[i + 2] as char).to_digit(16)?;
        let h2 = (bytes[i + 3] as char).to_digit(16)?;
        out.push(((h1 * 16 + h2) as u8) as char);
        i += 4;
    }
    Some(out)
}

pub fn cleanup_codegen_hex(code: &str) -> String {
    let bytes = code.as_bytes();
    let mut out = String::with_capacity(code.len());
    let mut i = 0;
    while i < bytes.len() {
        if i + 3 < bytes.len() && bytes[i] == b'\\' && bytes[i + 1] == b'x' {
            if let (Some(h1), Some(h2)) = ((bytes[i + 2] as char).to_digit(16), (bytes[i + 3] as char).to_digit(16)) {
                let c = ((h1 * 16 + h2) as u8) as char;
                if c.is_ascii_graphic() || c == ' ' { out.push(c); i += 4; continue; }
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

pub fn unwrap_double_brackets<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut v = DoubleBracketVisit { alloc, count: 0 };
    v.visit_program(program);
    v.count
}

struct DoubleBracketVisit<'a> { alloc: &'a Allocator, count: usize }

impl<'a> VisitMut<'a> for DoubleBracketVisit<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        if let Expression::ComputedMemberExpression(mem) = expr {
            let mut new_key: Option<Expression<'a>> = None;
            if let Expression::ArrayExpression(arr) = &mem.expression {
                if arr.elements.len() == 1 {
                    if let ArrayExpressionElement::SpreadElement(_) = &arr.elements[0] {
                    } else {
                        let inner = arr.elements[0].to_expression();
                        new_key = Some(self.clone_expr(inner));
                    }
                }
            }
            if let Some(mut k) = new_key {
                if let Expression::TemplateLiteral(t) = &k {
                    if t.expressions.is_empty() && t.quasis.len() == 1 {
                        let cooked = t.quasis[0].value.cooked.as_ref()
                            .map(|c| c.as_str().to_string())
                            .unwrap_or_else(|| t.quasis[0].value.raw.as_str().to_string());
                        let ast = AstBuilder::new(self.alloc);
                        k = ast.expression_string_literal(SPAN, ast.str(&cooked), None);
                    }
                }
                mem.expression = k;
                self.count += 1;
            }
        }
    }
}

impl<'a> DoubleBracketVisit<'a> {
    fn clone_expr(&self, e: &Expression<'a>) -> Expression<'a> {
        let ast = AstBuilder::new(self.alloc);
        match e {
            Expression::StringLiteral(s) => ast.expression_string_literal(SPAN, s.value, None),
            Expression::NumericLiteral(n) => ast.expression_numeric_literal(SPAN, n.value, None, n.base),
            Expression::BooleanLiteral(b) => ast.expression_boolean_literal(SPAN, b.value),
            Expression::NullLiteral(_) => ast.expression_null_literal(SPAN),
            Expression::Identifier(id) => ast.expression_identifier(SPAN, id.name),
            Expression::TemplateLiteral(t) => {
                let mut quasis = oxc_allocator::Vec::with_capacity_in(t.quasis.len(), self.alloc);
                for q in &t.quasis {
                    let lone = q.value.cooked.is_none();
                    quasis.push(ast.template_element(SPAN, q.value.clone(), q.tail, lone));
                }
                let mut exprs = oxc_allocator::Vec::with_capacity_in(t.expressions.len(), self.alloc);
                for ex in &t.expressions {
                    exprs.push(self.clone_expr(ex));
                }
                ast.expression_template_literal(SPAN, quasis, exprs)
            }
            _ => ast.void_0(SPAN),
        }
    }
}

pub fn normalize_to_dot<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut v = DotVisit { alloc, count: 0 };
    v.visit_program(program);
    v.count
}

struct DotVisit<'a> { alloc: &'a Allocator, count: usize }

impl<'a> VisitMut<'a> for DotVisit<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        if let Expression::ComputedMemberExpression(mem) = expr {
            let key_owned = std::mem::replace(&mut mem.expression, AstBuilder::new(self.alloc).void_0(SPAN));
            let (replace_with, key_back) = match key_owned {
                Expression::StringLiteral(s) if is_valid_ident(s.value.as_str()) => {
                    let ast = AstBuilder::new(self.alloc);
                    let new = ast.member_expression_static(
                        mem.span,
                        std::mem::replace(&mut mem.object, ast.void_0(SPAN)),
                        ast.identifier_name(SPAN, s.value),
                        false,
                    );
                    self.count += 1;
                    (Some(Expression::from(new)), None)
                }
                Expression::TemplateLiteral(t) if t.expressions.is_empty() && t.quasis.len() == 1 => {
                    let cooked = t.quasis[0].value.cooked.as_ref()
                        .map(|c| c.as_str().to_string())
                        .unwrap_or_else(|| t.quasis[0].value.raw.as_str().to_string());
                    if is_valid_ident(&cooked) {
                        let ast = AstBuilder::new(self.alloc);
                        let new = ast.member_expression_static(
                            mem.span,
                            std::mem::replace(&mut mem.object, ast.void_0(SPAN)),
                            ast.identifier_name(SPAN, ast.str(&cooked)),
                            false,
                        );
                        self.count += 1;
                        (Some(Expression::from(new)), None)
                    } else {
                        (None, Some(Expression::TemplateLiteral(t)))
                    }
                }
                other => (None, Some(other)),
            };
            if let Some(k) = key_back { mem.expression = k; }
            if let Some(new_expr) = replace_with { *expr = new_expr; }
        }
    }
}

pub fn inline_settimeout_zero<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut v = SetTimeoutVisit { alloc, count: 0 };
    v.visit_program(program);
    v.count
}

struct SetTimeoutVisit<'a> { alloc: &'a Allocator, count: usize }

impl<'a> VisitMut<'a> for SetTimeoutVisit<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        let Expression::CallExpression(call) = expr else { return };
        if !is_set_timeout(&call.callee) { return; }
        if call.arguments.len() < 2 { return; }
        let delay_zero = matches!(&call.arguments[1], Argument::NumericLiteral(n) if n.value == 0.0);
        if !delay_zero { return; }

        let take_args = std::mem::replace(&mut call.arguments, oxc_allocator::Vec::new_in(self.alloc));
        let mut iter = take_args.into_iter();
        let fn_arg = iter.next().unwrap();
        let _ = iter.next();
        let ast = AstBuilder::new(self.alloc);
        let body_stmts = match fn_arg {
            Argument::FunctionExpression(fe) => {
                let fe = fe.unbox();
                if !fe.params.items.is_empty() { return; }
                fe.body.map(|b| b.unbox().statements)
            }
            Argument::ArrowFunctionExpression(ar) => {
                let ar = ar.unbox();
                if !ar.params.items.is_empty() { return; }
                Some(ar.body.unbox().statements)
            }
            _ => return,
        };
        let Some(stmts) = body_stmts else { return };
        let mut exprs: Vec<Expression<'a>> = Vec::new();
        for s in stmts {
            if let Statement::ExpressionStatement(es) = s {
                exprs.push(es.unbox().expression);
            } else { return; }
        }
        if exprs.is_empty() {
            *expr = ast.expression_numeric_literal(SPAN, 0.0, None, oxc_syntax::number::NumberBase::Decimal);
            self.count += 1;
            return;
        }
        if exprs.len() == 1 {
            *expr = exprs.into_iter().next().unwrap();
            self.count += 1;
            return;
        }
        let mut alloc_exprs = oxc_allocator::Vec::with_capacity_in(exprs.len(), self.alloc);
        for e in exprs { alloc_exprs.push(e); }
        *expr = ast.expression_sequence(SPAN, alloc_exprs);
        self.count += 1;
    }
}

fn is_set_timeout(callee: &Expression) -> bool {
    match callee {
        Expression::Identifier(id) => id.name.as_str() == "setTimeout",
        Expression::StaticMemberExpression(m) => m.property.name.as_str() == "setTimeout",
        Expression::ComputedMemberExpression(m) => matches!(&m.expression, Expression::StringLiteral(s) if s.value.as_str() == "setTimeout"),
        _ => false,
    }
}
