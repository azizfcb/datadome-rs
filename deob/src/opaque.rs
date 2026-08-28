use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_semantic::SymbolId;
use oxc_syntax::operator::BinaryOperator as Bin;
use rustc_hash::FxHashMap as Map;

use crate::Const;
use crate::eval::Ctx;

pub struct Grid {
    rows: usize,
    cols: usize,
    root: usize,
    cells: Vec<usize>,
}

impl Grid {
    fn cell(&self, row: usize, col: usize) -> Option<usize> {
        (row < self.rows && col < self.cols).then(|| self.cells[row * self.cols + col])
    }

    pub fn at(&self, i: f64, j: f64) -> Option<usize> {
        let row = self.cell(self.root, i as usize)?;
        self.cell(row, j as usize)
    }
}

pub fn run(ctx: &Ctx, program: &Program) -> Map<SymbolId, Grid> {
    let mut shape = Shape { ctx, sizes: Map::default(), roots: Vec::new() };
    shape.visit_program(program);

    let mut cells = Cells { ctx, sizes: &shape.sizes, out: Map::default() };
    cells.visit_program(program);

    let mut out = Map::default();
    for (grid, array, root) in shape.roots {
        let (Some((rows, cols)), Some(cells)) = (shape.sizes.get(&array), cells.out.get(&array))
        else {
            continue;
        };
        if root < *rows {
            out.insert(grid, Grid { rows: *rows, cols: *cols, root, cells: cells.clone() });
        }
    }
    out
}

struct Shape<'a, 'c> {
    ctx: &'a Ctx<'c>,
    sizes: Map<SymbolId, (usize, usize)>,
    roots: Vec<(SymbolId, SymbolId, usize)>,
}

impl<'a, 'c, 'b> Visit<'b> for Shape<'a, 'c> {
    fn visit_for_statement(&mut self, f: &ForStatement<'b>) {
        walk::walk_for_statement(self, f);
        let Some(Expression::BinaryExpression(test)) = &f.test else { return };
        if test.operator != Bin::LessThan {
            return;
        }
        let Some(Const::Num(rows)) = self.ctx.value(&test.right) else { return };
        let Statement::ExpressionStatement(body) = &f.body else { return };
        let Expression::AssignmentExpression(a) = &body.expression else { return };
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else { return };
        let Expression::Identifier(array) = &m.object else { return };
        let Expression::NewExpression(new) = &a.right else { return };
        let Some(cols) = new
            .arguments
            .first()
            .and_then(|x| x.as_expression())
            .and_then(|x| self.ctx.value(x))
        else {
            return;
        };
        let (Const::Num(cols), Some(array)) = (cols, self.ctx.symbol(array)) else { return };
        self.sizes.insert(array, (rows as usize, cols as usize));
    }

    fn visit_variable_declarator(&mut self, d: &VariableDeclarator<'b>) {
        walk::walk_variable_declarator(self, d);
        let (BindingPattern::BindingIdentifier(id), Some(Expression::CallExpression(call))) =
            (&d.id, &d.init)
        else {
            return;
        };
        let Expression::FunctionExpression(f) = &call.callee else { return };
        let Some(body) = f.body.as_deref() else { return };
        let Some(Statement::ReturnStatement(ret)) = body.statements.last() else { return };
        if let Some((array, root)) = ret.argument.as_ref().and_then(|a| self.rooted(a)) {
            self.roots.push((id.symbol_id(), array, root));
        }
    }

    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'b>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left else { return };
        let (Some((array, root)), Some(grid)) = (self.rooted(&a.right), self.ctx.symbol(id))
        else {
            return;
        };
        self.roots.push((grid, array, root));
    }
}

impl<'a, 'c> Shape<'a, 'c> {
    fn rooted(&self, e: &Expression) -> Option<(SymbolId, usize)> {
        let Expression::ComputedMemberExpression(m) = e else { return None };
        let Expression::Identifier(array) = &m.object else { return None };
        let Const::Num(root) = self.ctx.value(&m.expression)? else { return None };
        Some((self.ctx.symbol(array)?, root as usize))
    }
}

struct Cells<'a, 'c> {
    ctx: &'a Ctx<'c>,
    sizes: &'a Map<SymbolId, (usize, usize)>,
    out: Map<SymbolId, Vec<usize>>,
}

impl<'a, 'c, 'b> Visit<'b> for Cells<'a, 'c> {
    fn visit_assignment_expression(&mut self, a: &AssignmentExpression<'b>) {
        walk::walk_assignment_expression(self, a);
        let AssignmentTarget::ComputedMemberExpression(outer) = &a.left else { return };
        let Expression::ComputedMemberExpression(inner) = &outer.object else { return };
        let (Expression::Identifier(name), Expression::ComputedMemberExpression(src)) =
            (&inner.object, &a.right)
        else {
            return;
        };
        let (Expression::Identifier(row), Expression::Identifier(col)) =
            (&inner.expression, &outer.expression)
        else {
            return;
        };
        let (Some(array), Some(source)) = (self.ctx.symbol(name), src.object.get_identifier_reference())
        else {
            return;
        };
        if self.ctx.symbol(source) != Some(array) || self.out.contains_key(&array) {
            return;
        }
        let Some((rows, cols)) = self.sizes.get(&array).copied() else { return };
        let (Some(row), Some(col)) = (self.ctx.symbol(row), self.ctx.symbol(col)) else { return };

        let mut cells = Vec::with_capacity(rows * cols);
        for r in 0..rows {
            for c in 0..cols {
                let mut binds = vec![(row, Const::Num(r as f64)), (col, Const::Num(c as f64))];
                let Some(Const::Num(v)) = self.ctx.value_in(&src.expression, &mut binds) else {
                    return;
                };
                if !(0.0..rows as f64).contains(&v) {
                    return;
                }
                cells.push(v as usize);
            }
        }
        self.out.insert(array, cells);
    }
}
