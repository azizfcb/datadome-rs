use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

const MAGIC: [u8; 8] = [0, 0x61, 0x73, 0x6d, 1, 0, 0, 0];
const ALPHABET: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub fn modules(source: &str) -> Vec<Vec<u8>> {
    let alloc = Allocator::default();
    let ret = Parser::new(&alloc, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    let mut scan = Scan { out: Vec::new() };
    scan.visit_program(&ret.program);
    scan.out.sort();
    scan.out.dedup();
    scan.out
}

struct Scan {
    out: Vec<Vec<u8>>,
}

impl<'a> Visit<'a> for Scan {
    fn visit_string_literal(&mut self, s: &StringLiteral<'a>) {
        walk::walk_string_literal(self, s);
        if let Some(bytes) = base64(s.value.as_bytes())
            && bytes.starts_with(&MAGIC)
        {
            self.out.push(bytes);
        }
    }
}

pub fn base64(text: &[u8]) -> Option<Vec<u8>> {
    let mut order = [255u8; 256];
    for (i, c) in ALPHABET.iter().enumerate() {
        order[*c as usize] = i as u8;
    }
    let body: Vec<u8> = text.iter().copied().take_while(|c| *c != b'=').collect();
    if body.len() < 8 || body.len() % 4 == 1 {
        return None;
    }
    let mut out = Vec::with_capacity(body.len() / 4 * 3);
    for group in body.chunks(4) {
        let mut acc: u32 = 0;
        for k in 0..4 {
            let index = match group.get(k) {
                Some(c) => *order.get(*c as usize)?,
                None => 0,
            };
            if index == 255 {
                return None;
            }
            acc = acc << 6 | index as u32;
        }
        for k in 0..group.len() - 1 {
            out.push((acc >> (16 - 8 * k)) as u8);
        }
    }
    Some(out)
}
