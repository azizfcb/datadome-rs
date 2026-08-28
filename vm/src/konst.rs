use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_syntax::scope::ScopeFlags;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug)]
pub enum Tag {
    Bool(bool),
    Null,
    Undefined,
    Float,
    Int(usize),
}

#[derive(Debug)]
pub struct Consts {
    pub small_bit: u32,
    pub small_mask: u32,
    pub tags: BTreeMap<u8, Tag>,
}

pub fn consts(program: &Program) -> Option<Consts> {
    let mut find = Find { out: None };
    find.visit_program(program);
    find.out
}

struct Find {
    out: Option<Consts>,
}

impl<'a> Visit<'a> for Find {
    fn visit_function(&mut self, f: &Function<'a>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        if self.out.is_none() {
            self.out = decoder(f);
        }
    }
}

fn decoder(f: &Function) -> Option<Consts> {
    {
        let body = f.body.as_deref()?;
        let [FormalParameter { pattern: BindingPattern::BindingIdentifier(read), .. }] =
            f.params.items.as_slice()
        else {
            return None;
        };

        let small = body.statements.iter().find_map(small)?;
        let switch = body.statements.iter().find_map(|s| match s {
            Statement::SwitchStatement(s) => Some(s),
            _ => None,
        })?;

        let mut tags = BTreeMap::new();
        for (i, case) in switch.cases.iter().enumerate() {
            let Some(Expression::NumericLiteral(label)) = &case.test else { return None };
            let mut run = Run { param: &read.name, bytes: 0, first: None };
            for case in &switch.cases[i..] {
                run.visit_statements(&case.consequent);
                if case.consequent.iter().any(|s| matches!(s, Statement::ReturnStatement(_))) {
                    break;
                }
            }
            tags.insert(label.value as u8, run.tag()?);
        }
        Some(Consts { small_bit: small.0, small_mask: small.1, tags })
    }
}

fn small(s: &Statement) -> Option<(u32, u32)> {
    let Statement::IfStatement(i) = s else { return None };
    let Statement::ReturnStatement(r) = &i.consequent else { return None };
    Some((mask(&i.test)?, mask(r.argument.as_ref()?)?))
}

fn mask(e: &Expression) -> Option<u32> {
    let Expression::BinaryExpression(b) = e else { return None };
    if b.operator != BinaryOperator::BitwiseAnd {
        return None;
    }
    let Expression::NumericLiteral(n) = &b.left else { return None };
    Some(n.value as u32)
}

struct Run<'a> {
    param: &'a str,
    bytes: usize,
    first: Option<Tag>,
}

impl<'a, 'b> Visit<'b> for Run<'a> {
    fn visit_call_expression(&mut self, c: &CallExpression<'b>) {
        walk::walk_call_expression(self, c);
        if matches!(&c.callee, Expression::Identifier(i) if i.name == self.param) {
            self.bytes += 1;
        }
    }

    fn visit_object_property(&mut self, p: &ObjectProperty<'b>) {
        walk::walk_object_property(self, p);
        if let (PropertyKey::StaticIdentifier(k), Expression::NumericLiteral(n)) = (&p.key, &p.value)
            && k.name == "length"
        {
            self.first = Some(Tag::Float);
            self.bytes = n.value as usize;
        }
    }

    fn visit_return_statement(&mut self, r: &ReturnStatement<'b>) {
        walk::walk_return_statement(self, r);
        if self.first.is_some() {
            return;
        }
        self.first = Some(match r.argument.as_ref() {
            None => Tag::Undefined,
            Some(Expression::BooleanLiteral(b)) => Tag::Bool(b.value),
            Some(Expression::NullLiteral(_)) => Tag::Null,
            _ => return,
        });
    }
}

impl<'a> Run<'a> {
    fn tag(&self) -> Option<Tag> {
        Some(match self.first {
            Some(Tag::Float) => Tag::Float,
            Some(t) if self.bytes == 0 => t,
            _ => Tag::Int(self.bytes),
        })
    }
}
