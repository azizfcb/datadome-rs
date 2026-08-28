mod collect;
mod decode;
mod eval;
mod flatten;
mod fold;
mod opaque;
mod simplify;

use oxc_allocator::{Allocator, TakeIn};
use oxc_ast::ast::*;
use oxc_ast::builder::AstBuilder;
use oxc_ast_visit::{VisitMut, walk_mut};
use oxc_ast_visit::Visit;
use oxc_codegen::Codegen;
use oxc_parser::{ParseOptions, Parser};
use oxc_semantic::SemanticBuilder;
use oxc_span::{GetSpan, SourceType, Span};
use rustc_hash::FxHashMap as Map;

#[derive(Clone, Debug)]
pub enum Const {
    Num(f64),
    Str(String),
    Bool(bool),
    Null,
}

pub struct Folded {
    pub value: Const,
    pub assigns: bool,
}

pub type Folds = Map<Span, Folded>;

pub fn deobfuscate(source: &str) -> Result<String, String> {
    let alloc = Allocator::default();
    let program = transform(&alloc, source)?;
    Ok(Codegen::default().build(&program).code)
}

pub fn transform<'a>(alloc: &'a Allocator, source: &'a str) -> Result<Program<'a>, String> {
    let ret = Parser::new(alloc, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    if let Some(e) = ret.diagnostics.first() {
        return Err(e.to_string());
    }
    let mut program = ret.program;

    let folds = {
        let semantic = SemanticBuilder::new().build(&program).semantic;
        let mut c = collect::Collect::default();
        c.visit_program(&program);

        let mut ctx = eval::Ctx {
            scoping: semantic.scoping(),
            fns: c.fns,
            char_code: c.char_code,
            globals: c.globals,
            decoders: Map::default(),
            consts: Map::default(),
            grids: Map::default(),
        };
        ctx.grids = opaque::run(&ctx, &program);
        ctx.decoders = decode::run(&ctx, &program);
        fold::propagate(&mut ctx, &program);
        fold::run(&ctx, &program)
    };
    apply(alloc, &mut program, &folds);
    simplify::run(&alloc, &mut program);
    flatten::run(&alloc, &mut program);
    simplify::run(&alloc, &mut program);

    Ok(program)
}

fn apply<'a>(alloc: &'a Allocator, program: &mut Program<'a>, folds: &Folds) {
    struct Apply<'a, 'f> {
        alloc: &'a Allocator,
        ast: AstBuilder<'a>,
        folds: &'f Folds,
    }
    fn assignments<'a>(
        e: &mut Expression<'a>,
        ast: &AstBuilder<'a>,
        out: &mut oxc_allocator::Vec<'a, Expression<'a>>,
    ) {
        match e {
            Expression::AssignmentExpression(_) => out.push(e.take_in(ast)),
            Expression::BinaryExpression(b) => {
                assignments(&mut b.left, ast, out);
                assignments(&mut b.right, ast, out);
            }
            Expression::UnaryExpression(u) => assignments(&mut u.argument, ast, out),
            Expression::SequenceExpression(q) => {
                for x in q.expressions.iter_mut() {
                    assignments(x, ast, out);
                }
            }
            _ => {}
        }
    }

    impl<'a, 'f> Apply<'a, 'f> {
        fn literal(&self, span: Span, value: &Const) -> Expression<'a> {
            match value {
                Const::Num(n) => Expression::new_numeric_literal(
                    span,
                    *n,
                    None,
                    oxc_syntax::number::NumberBase::Decimal,
                    &self.ast,
                ),
                Const::Str(s) => {
                    Expression::new_string_literal(span, self.alloc.alloc_str(s), None, &self.ast)
                }
                Const::Bool(b) => Expression::new_boolean_literal(span, *b, &self.ast),
                Const::Null => Expression::new_null_literal(span, &self.ast),
            }
        }
    }

    impl<'a, 'f> VisitMut<'a> for Apply<'a, 'f> {
        fn visit_expression(&mut self, e: &mut Expression<'a>) {
            let span = e.span();
            let Some(folded) = self.folds.get(&span) else {
                walk_mut::walk_expression(self, e);
                return;
            };
            let value = self.literal(span, &folded.value);
            if !folded.assigns {
                *e = value;
                return;
            }
            walk_mut::walk_expression(self, e);
            let mut parts = oxc_allocator::Vec::new_in(&self.ast);
            assignments(e, &self.ast, &mut parts);
            *e = if parts.is_empty() {
                value
            } else {
                parts.push(value);
                Expression::new_sequence_expression(span, parts, &self.ast)
            };
        }
    }
    Apply { alloc, ast: AstBuilder::new(alloc), folds }.visit_program(program);
}
