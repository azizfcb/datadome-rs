use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_span::GetSpan;

use crate::Folds;
use crate::eval::Ctx;

pub fn propagate(ctx: &mut Ctx, program: &Program) {
    struct Props<'a, 'c>(&'a mut Ctx<'c>);
    impl<'a, 'c, 'b> Visit<'b> for Props<'a, 'c> {
        fn visit_variable_declarator(&mut self, d: &VariableDeclarator<'b>) {
            walk::walk_variable_declarator(self, d);
            let (BindingPattern::BindingIdentifier(id), Some(init)) = (&d.id, &d.init) else {
                return;
            };
            let id = id.symbol_id();
            if self.0.scoping.symbol_is_mutated(id) {
                return;
            }
            let mut env = Vec::new();
            if let Some(v) = self.0.value_in(init, &mut env)
                && env.is_empty()
            {
                self.0.consts.insert(id, v);
            }
        }
    }
    Props(ctx).visit_program(program);
}

fn assigns(e: &Expression) -> bool {
    let mut found = false;
    struct Look<'x>(&'x mut bool);
    impl<'a, 'x> Visit<'a> for Look<'x> {
        fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'a>) {
            walk::walk_assignment_expression(self, a);
            *self.0 = true;
        }
        fn visit_update_expression(&mut self, u: &UpdateExpression<'a>) {
            walk::walk_update_expression(self, u);
            *self.0 = true;
        }
    }
    Look(&mut found).visit_expression(e);
    found
}

fn unconditional(e: &Expression) -> bool {
    match e {
        Expression::UnaryExpression(u) => unconditional(&u.argument),
        Expression::BinaryExpression(b) => unconditional(&b.left) && unconditional(&b.right),
        Expression::SequenceExpression(q) => q.expressions.iter().all(unconditional),
        Expression::AssignmentExpression(a) => {
            matches!(&a.left, AssignmentTarget::AssignmentTargetIdentifier(_))
                && a.operator == oxc_syntax::operator::AssignmentOperator::Assign
                && unconditional(&a.right)
        }
        other => !assigns(other),
    }
}

pub fn run(ctx: &Ctx, program: &Program) -> Folds {
    struct Fold<'a, 'c> {
        ctx: &'a Ctx<'c>,
        out: Folds,
    }
    impl<'a, 'c, 'b> Visit<'b> for Fold<'a, 'c> {
        fn visit_expression(&mut self, e: &Expression<'b>) {
            walk::walk_expression(self, e);
            if matches!(
                e,
                Expression::NumericLiteral(_)
                    | Expression::StringLiteral(_)
                    | Expression::BooleanLiteral(_)
                    | Expression::AssignmentExpression(_)
                    | Expression::SequenceExpression(_)
            ) {
                return;
            }
            let mut env = Vec::new();
            let Some(value) = self.ctx.value_in(e, &mut env) else { return };
            if env.is_empty() {
                self.out.insert(e.span(), crate::Folded { value, assigns: false });
            } else if unconditional(e) {
                self.out.insert(e.span(), crate::Folded { value, assigns: true });
            }
        }
    }
    let mut fold = Fold { ctx, out: Folds::default() };
    fold.visit_program(program);
    fold.out
}
