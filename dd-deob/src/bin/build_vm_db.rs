use std::collections::BTreeMap;
use std::collections::HashSet;
use std::path::PathBuf;

use dd_deob::vm::LabeledNormalizer;
use sha2::{Digest, Sha256};

fn main() {
    let vm_path = std::env::var("VM_LABELED").unwrap_or_else(|_| "/tmp/datadome-vm/vm_labeled.js".into());
    let names_path = std::env::var("VM_NAMES").unwrap_or_else(|_| "/tmp/datadome-vm/disasm.js".into());
    let out_path = PathBuf::from(std::env::var("VM_DB_OUT").unwrap_or_else(|_| "dd-deob/src/vm_db.rs".into()));

    let labeled = std::fs::read_to_string(&vm_path).expect("read vm_labeled.js");
    let names_src = std::fs::read_to_string(&names_path).expect("read disasm.js");

    let names = parse_names(&names_src);
    let handlers = extract_handlers(&labeled);

    let mut entries: Vec<(String, String, String)> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for (slot, body) in &handlers {
        let op_idx = slot - 4783;
        let (name, fmt) = names.get(&op_idx).cloned().unwrap_or_else(|| (format!("OP_{}", op_idx), "?".into()));
        let normalized = LabeledNormalizer::normalize(body);
        let mut h = Sha256::new();
        h.update(normalized.as_bytes());
        let d = h.finalize();
        let mut hash = String::with_capacity(16);
        for b in &d[..8] {
            hash.push_str(&format!("{:02x}", b));
        }
        if seen.contains(&hash) { continue; }
        seen.insert(hash.clone());
        if std::env::var_os("VM_DB_DUMP").is_some() {
            eprintln!("---op {} {} {} ({})---\n{}", op_idx, name, fmt, hash, normalized);
        }
        entries.push((hash, name, fmt));
    }

    let mut out = String::new();
    out.push_str("// Auto-generated\n");
    out.push_str("// Maps shape-hash of normalized opcode body -> (name, operand format).\n");
    out.push_str("// Each new DataDome build randomizes opcode indices but body shape is stable.\n\n");
    out.push_str("pub static KNOWN_OPCODES: &[(&str, &str, &str)] = &[\n");
    for (h, n, f) in &entries {
        let ne = n.replace('"', "\\\"");
        let fe = f.replace('"', "\\\"");
        out.push_str(&format!("    (\"{}\", \"{}\", \"{}\"),\n", h, ne, fe));
    }
    out.push_str("];\n");
    std::fs::write(&out_path, out).expect("write vm_db.rs");
    eprintln!("wrote {} ({} unique opcode shapes)", out_path.display(), entries.len());
}

fn parse_names(src: &str) -> BTreeMap<i64, (String, String)> {
    let mut out = BTreeMap::new();
    // pattern: `<num>: ['NAME', 'fmt']`
    let bytes = src.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i].is_ascii_digit() {
            let start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() { i += 1; }
            let num: i64 = src[start..i].parse().unwrap_or(-1);
            // skip ws
            while i < bytes.len() && (bytes[i] as char).is_whitespace() { i += 1; }
            if i < bytes.len() && bytes[i] == b':' {
                i += 1;
                while i < bytes.len() && (bytes[i] as char).is_whitespace() { i += 1; }
                if i < bytes.len() && bytes[i] == b'[' {
                    i += 1;
                    // skip ws
                    while i < bytes.len() && (bytes[i] as char).is_whitespace() { i += 1; }
                    if i < bytes.len() && bytes[i] == b'\'' {
                        i += 1;
                        let nstart = i;
                        while i < bytes.len() && bytes[i] != b'\'' { i += 1; }
                        let name = src[nstart..i].to_string();
                        if i < bytes.len() { i += 1; }
                        // skip , and ws
                        while i < bytes.len() && (bytes[i] == b',' || (bytes[i] as char).is_whitespace()) { i += 1; }
                        if i < bytes.len() && bytes[i] == b'\'' {
                            i += 1;
                            let fstart = i;
                            while i < bytes.len() && bytes[i] != b'\'' { i += 1; }
                            let fmt = src[fstart..i].to_string();
                            if i < bytes.len() { i += 1; }
                            out.insert(num, (name, fmt));
                        }
                    }
                }
            }
        } else {
            i += 1;
        }
    }
    out
}

fn extract_handlers(text: &str) -> Vec<(i64, String)> {
    // Match A[<digits>] = function () { <body> }   (brace-balanced)
    let bytes = text.as_bytes();
    let mut out: Vec<(i64, String)> = Vec::new();
    let mut i = 0usize;
    while i + 8 < bytes.len() {
        if bytes[i] == b'A' && bytes[i + 1] == b'[' {
            let s = i + 2;
            let mut k = s;
            while k < bytes.len() && bytes[k].is_ascii_digit() { k += 1; }
            if k > s && k < bytes.len() && bytes[k] == b']' {
                let num: i64 = text[s..k].parse().unwrap_or(-1);
                let mut p = k + 1;
                while p < bytes.len() && (bytes[p] as char).is_whitespace() { p += 1; }
                if p < bytes.len() && bytes[p] == b'=' {
                    p += 1;
                    while p < bytes.len() && (bytes[p] as char).is_whitespace() { p += 1; }
                    if p + 8 < bytes.len() && &bytes[p..p + 8] == b"function" {
                        // skip to first '{' that opens the body
                        let mut q = p + 8;
                        while q < bytes.len() && bytes[q] != b'{' { q += 1; }
                        if q < bytes.len() {
                            let body_start = q + 1;
                            let mut depth: i32 = 1;
                            let mut r = body_start;
                            while r < bytes.len() && depth > 0 {
                                let c = bytes[r];
                                if c == b'\'' || c == b'"' || c == b'`' {
                                    let qq = c;
                                    r += 1;
                                    while r < bytes.len() {
                                        let cc = bytes[r];
                                        r += 1;
                                        if cc == b'\\' && r < bytes.len() { r += 1; continue; }
                                        if cc == qq { break; }
                                    }
                                    continue;
                                }
                                if c == b'/' && r + 1 < bytes.len() && bytes[r + 1] == b'/' {
                                    while r < bytes.len() && bytes[r] != b'\n' { r += 1; }
                                    continue;
                                }
                                if c == b'/' && r + 1 < bytes.len() && bytes[r + 1] == b'*' {
                                    r += 2;
                                    while r + 1 < bytes.len() && !(bytes[r] == b'*' && bytes[r + 1] == b'/') { r += 1; }
                                    r = (r + 2).min(bytes.len());
                                    continue;
                                }
                                if c == b'{' { depth += 1; }
                                if c == b'}' { depth -= 1; if depth == 0 { break; } }
                                r += 1;
                            }
                            let body = text[body_start..r].trim().to_string();
                            out.push((num, body));
                            i = r + 1;
                            continue;
                        }
                    }
                }
            }
        }
        i += 1;
    }
    out
}
