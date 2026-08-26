use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use base64::Engine as _;
use futures_util::StreamExt;
use wreq::Client;
use wreq::cookie::Jar;
use wreq::header::HeaderMap;
use wreq::redirect::Policy;
use wreq_util::{Emulation, Platform, Profile};

const RESET: &str = "\x1b[0m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const RED: &str = "\x1b[31m";
const GREY: &str = "\x1b[90m";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";

fn status_color(code: u16) -> &'static str {
    match code {
        200..=299 => GREEN,
        300..=399 => YELLOW,
        _ => RED,
    }
}

#[derive(Debug)]
struct E(String);
impl std::fmt::Display for E { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0) } }
impl From<&str> for E { fn from(x: &str) -> Self { E(x.into()) } }
impl From<String> for E { fn from(x: String) -> Self { E(x) } }
impl From<wreq::Error> for E { fn from(x: wreq::Error) -> Self { E(x.to_string()) } }
impl From<std::io::Error> for E { fn from(x: std::io::Error) -> Self { E(x.to_string()) } }
impl From<serde_json::Error> for E { fn from(x: serde_json::Error) -> Self { E(x.to_string()) } }

#[derive(Debug, Default)]
struct DdConfig {
    rt: String,
    cid: String,
    hsh: String,
    s: i64,
    t: Option<String>,
    e: Option<String>,
    b: Option<i64>,
    cookie_cid: String,
}

fn slice_dd_object(body: &str) -> Option<&str> {
    let i = body.find("var dd=")
        .or_else(|| body.find("window.dd ="))
        .or_else(|| body.find("window.dd="))?;
    let rest = &body[i..];
    let lb = rest.find('{')?;
    let mut depth = 0i32;
    for (j, ch) in rest[lb..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(&rest[lb..lb + j + 1]);
                }
            }
            _ => {}
        }
    }
    None
}

fn read_str(o: &str, key: &str) -> Option<String> {
    let needle = format!("'{}'", key);
    let mut p = o.find(&needle)?;
    p += needle.len();
    let after = &o[p..];
    let colon = after.find(':')?;
    let rest = &after[colon + 1..];
    let q = rest.find('\'')?;
    let inner = &rest[q + 1..];
    let end = inner.find('\'')?;
    Some(inner[..end].to_string())
}

fn read_num(o: &str, key: &str) -> Option<i64> {
    let needle = format!("'{}'", key);
    let mut p = o.find(&needle)?;
    p += needle.len();
    let after = &o[p..];
    let colon = after.find(':')?;
    let rest = after[colon + 1..].trim_start();
    let mut end = 0usize;
    for (i, ch) in rest.char_indices() {
        if ch == ',' || ch == '}' { end = i; break; }
    }
    rest[..end].trim().parse().ok()
}

fn parse_dd(body: &str) -> Option<DdConfig> {
    let obj = slice_dd_object(body)?;
    let mut cfg = DdConfig::default();
    cfg.rt = read_str(obj, "rt")?;
    cfg.cid = read_str(obj, "cid")?;
    cfg.hsh = read_str(obj, "hsh")?;
    cfg.s = read_num(obj, "s")?;
    cfg.t = read_str(obj, "t");
    cfg.e = read_str(obj, "e");
    cfg.b = read_num(obj, "b");
    Some(cfg)
}

fn extract_cookie_cid(headers: &HeaderMap) -> Option<String> {
    for v in headers.get_all(wreq::header::SET_COOKIE).iter() {
        let s = v.to_str().ok()?;
        if let Some(rest) = s.strip_prefix("datadome=") {
            let end = rest.find(';').unwrap_or(rest.len());
            return Some(rest[..end].to_string());
        }
    }
    None
}

fn url_encode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for &b in s.as_bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => out.push(b as char),
            _ => out.push_str(&format!("%{:02X}", b)),
        }
    }
    out
}

fn build_challenge_url(cfg: &DdConfig, referer: &str) -> String {
    let kind = if cfg.rt == "i" { "interstitial" } else { "captcha" };
    let mut u = format!(
        "https://geo.captcha-delivery.com/{}/?initialCid={}&hash={}&cid={}",
        kind,
        url_encode(&cfg.cid),
        url_encode(&cfg.hsh),
        url_encode(&cfg.cookie_cid),
    );
    if let Some(t) = &cfg.t { u.push_str(&format!("&t={}", url_encode(t))); }
    u.push_str(&format!("&referer={}", url_encode(referer)));
    u.push_str(&format!("&s={}", cfg.s));
    if let Some(b) = cfg.b { u.push_str(&format!("&b={}", b)); }
    if let Some(e) = &cfg.e { u.push_str(&format!("&e={}", url_encode(e))); }
    u.push_str("&dm=cd");
    u
}

fn find_image(body: &str, suffix: &str) -> Option<String> {
    let host = "https://dd.prod.captcha-delivery.com/image/";
    let i = body.find(host)?;
    let rest = &body[i..];
    let end = rest.find(suffix)?;
    Some(rest[..end + suffix.len()].to_string())
}

fn extract_bundler(body: &str) -> Option<&str> {
    // The obfuscated bundler is the longest <script>…</script> in the challenge
    // page. Shapes seen in the wild:
    //   * Interstitial / device-check: `;(function(){var A={…},Q={};…})()`
    //   * Captcha (e2e shutterstock build): `!function(e,B,s){…}({1:[…]})`
    //   * Captcha (slider, leboncoin build): `!function A(B,g,a){function s(w,D){if(!g[w])…`
    //     (named-function browserify wrapper preceded by a `cyberfraud
    //      solution … v<ver>` banner comment)
    //   * Captcha alt: `!function(A,e,B){…}`
    let markers = [
        ";(function(){var A={",
        "!function(e,B,s){",
        "!function(A,e,B){",
    ];
    let mut best: Option<usize> = None;
    for m in &markers {
        if let Some(start) = body.find(m) {
            best = Some(best.map(|b| b.min(start)).unwrap_or(start));
        }
    }
    if best.is_none() {
        // Generic fallback: anchor on the browserify wrapper body (stable across
        // DD versions and used by every captcha-slider build), then walk back
        // to the nearest `!function` opener (named or anonymous).
        let anchor = body.find("if(!g[w]){if(!B[w]){var I=\"function\"==typeof require&&require")
            .or_else(|| body.find("if(!n[i]){if(!t[i]){var s=\"function\"==typeof require&&require"))
            .or_else(|| body.find("\"function\"==typeof require&&require"));
        if let Some(a) = anchor {
            if let Some(opener) = body[..a].rfind("!function") {
                best = Some(opener);
            }
        }
    }
    let start = best?;
    let after = &body[start..];
    // The IIFE has shape `!function(...){body}(...)` (or `;(function(){body})()`).
    // Slice through the function body and its trailing invocation argument list,
    // brace-balanced and string-aware. Earlier we sliced to `</script>`, but in
    // slider builds the bundler is followed by inline page JS in the same
    // <script> tag, which over-captures by hundreds of KB and breaks the parser.
    let end_rel = balance_iife(after).unwrap_or_else(|| after.find("</script>").unwrap_or(after.len()));
    Some(&after[..end_rel])
}

/// Walk past `<prefix>(<paramlist>){<body>}(<arglist>)` from byte 0, returning
/// the offset just past the closing arglist `)`. Strings, regex, and comments
/// are skipped so braces inside them don't affect depth.
fn balance_iife(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 0usize;
    // Skip leading `;` or `!` if present
    while i < bytes.len() && (bytes[i] == b';' || bytes[i] == b'!') { i += 1; }
    // Expect `function`
    if i + 8 > bytes.len() || &bytes[i..i + 8] != b"function" { return None; }
    i += 8;
    // Optional name
    while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'$') { i += 1; }
    // Param list `(...)` — paren-balanced
    i = skip_paren_group(bytes, i)?;
    // Body `{...}` — brace-balanced
    i = skip_brace_group(bytes, i)?;
    // Trailing `()` (invocation)
    i = skip_paren_group(bytes, i)?;
    Some(i)
}

fn skip_paren_group(bytes: &[u8], mut i: usize) -> Option<usize> {
    while i < bytes.len() && bytes[i].is_ascii_whitespace() { i += 1; }
    if i >= bytes.len() || bytes[i] != b'(' { return None; }
    let mut depth = 0i32;
    while i < bytes.len() {
        let c = bytes[i];
        match c {
            b'(' => depth += 1,
            b')' => { depth -= 1; if depth == 0 { return Some(i + 1); } }
            b'\'' | b'"' | b'`' => i = skip_string(bytes, i, c)?.saturating_sub(1),
            b'/' if i + 1 < bytes.len() && (bytes[i + 1] == b'/' || bytes[i + 1] == b'*') => i = skip_comment(bytes, i).saturating_sub(1),
            _ => {}
        }
        i += 1;
    }
    None
}

fn skip_brace_group(bytes: &[u8], mut i: usize) -> Option<usize> {
    while i < bytes.len() && bytes[i].is_ascii_whitespace() { i += 1; }
    if i >= bytes.len() || bytes[i] != b'{' { return None; }
    let mut depth = 0i32;
    while i < bytes.len() {
        let c = bytes[i];
        match c {
            b'{' => depth += 1,
            b'}' => { depth -= 1; if depth == 0 { return Some(i + 1); } }
            b'\'' | b'"' | b'`' => i = skip_string(bytes, i, c)?.saturating_sub(1),
            b'/' if i + 1 < bytes.len() && (bytes[i + 1] == b'/' || bytes[i + 1] == b'*') => i = skip_comment(bytes, i).saturating_sub(1),
            _ => {}
        }
        i += 1;
    }
    None
}

fn skip_string(bytes: &[u8], i: usize, quote: u8) -> Option<usize> {
    let mut j = i + 1;
    while j < bytes.len() {
        let c = bytes[j];
        if c == b'\\' { j += 2; continue; }
        if c == quote { return Some(j + 1); }
        j += 1;
    }
    None
}

fn skip_comment(bytes: &[u8], i: usize) -> usize {
    if bytes[i + 1] == b'/' {
        let mut j = i + 2;
        while j < bytes.len() && bytes[j] != b'\n' { j += 1; }
        j
    } else {
        let mut j = i + 2;
        while j + 1 < bytes.len() && !(bytes[j] == b'*' && bytes[j + 1] == b'/') { j += 1; }
        (j + 2).min(bytes.len())
    }
}

async fn fetch_full(client: &Client, url: &str) -> Result<(u16, HeaderMap, Vec<u8>, u128), E> {
    let t0 = Instant::now();
    let resp = client.get(url).send().await?;
    let status = resp.status().as_u16();
    let headers = resp.headers().clone();
    let mut buf: Vec<u8> = Vec::new();
    let mut s = resp.bytes_stream();
    while let Some(c) = s.next().await { buf.extend_from_slice(&c?); }
    Ok((status, headers, buf, t0.elapsed().as_millis()))
}

async fn fetch_run(target: &str, out_dir: &PathBuf) -> Result<(), E> {
    let jar = Arc::new(Jar::default());
    let client = Client::builder()
        .emulation(Emulation::builder().profile(Profile::Chrome147).platform(Platform::MacOS).build())
        .cookie_provider(jar.clone())
        .redirect(Policy::limited(6))
        .build()?;

    let t_all = Instant::now();
    let (s1, h1, b1, ms1) = fetch_full(&client, target).await?;
    println!("  {GREY}[1]{RESET} GET {target}  → {}{}{RESET} {GREY}{}b/{}ms{RESET}",
             status_color(s1), s1, b1.len(), ms1);
    tokio::fs::create_dir_all(out_dir).await?;
    tokio::fs::write(out_dir.join("first.html"), &b1).await?;

    let body1 = String::from_utf8_lossy(&b1).into_owned();
    let mut cfg = parse_dd(&body1).ok_or_else(|| E(format!("no dd object (status {s1}, {}b body) — see {}/first.html", b1.len(), out_dir.display())))?;
    cfg.cookie_cid = extract_cookie_cid(&h1)
        .ok_or_else(|| E("no datadome cookie in Set-Cookie".into()))?;
    println!("  {GREY}[2]{RESET} dd: rt={} cid={}… hsh={}… s={} t={:?} e={:?} b={:?}",
             cfg.rt, &cfg.cid[..cfg.cid.len().min(16)], &cfg.hsh[..cfg.hsh.len().min(16)],
             cfg.s, cfg.t, cfg.e, cfg.b);
    println!("       cookie cid={}…", &cfg.cookie_cid[..cfg.cookie_cid.len().min(32)]);

    let challenge = build_challenge_url(&cfg, target);
    let (s2, _h2, b2, ms2) = fetch_full(&client, &challenge).await?;
    println!("  {GREY}[3]{RESET} GET captcha bundle  → {}{}{RESET} {GREY}{}b/{}ms{RESET}",
             status_color(s2), s2, b2.len(), ms2);
    if s2 != 200 { return Err(E(format!("challenge status {s2}"))); }

    tokio::fs::create_dir_all(out_dir).await?;
    tokio::fs::write(out_dir.join("403.html"), &b1).await?;
    tokio::fs::write(out_dir.join("challenge.html"), &b2).await?;
    tokio::fs::write(out_dir.join("dd.json"), serde_json::to_vec_pretty(&serde_json::json!({
        "rt": cfg.rt, "cid": cfg.cid, "hsh": cfg.hsh, "s": cfg.s,
        "t": cfg.t, "e": cfg.e, "b": cfg.b,
        "cookie_cid": cfg.cookie_cid,
        "referer": target,
        "challenge_url": challenge,
    }))?).await?;

    let body2 = String::from_utf8_lossy(&b2).into_owned();
    if let Some(bundler) = extract_bundler(&body2) {
        let path = out_dir.join(if cfg.rt == "c" { "captcha.js" } else { "interstitial.js" });
        tokio::fs::write(&path, bundler).await?;
        println!("       bundler  → {GREY}{}b → {}{RESET}", bundler.len(), path.display());
    } else {
        println!("  {YELLOW}!{RESET} could not slice obfuscated bundler from challenge HTML");
    }

    if cfg.rt == "c" {
        let puzzle = find_image(&body2, ".jpg");
        let piece = find_image(&body2, ".frag.png");
        if let (Some(p), Some(f)) = (&puzzle, &piece) {
            let (sp, _hp, bp, msp) = fetch_full(&client, p).await?;
            let (sf, _hf, bf, msf) = fetch_full(&client, f).await?;
            println!("  {GREY}[4]{RESET} puzzle  → {}{}{RESET} {GREY}{}b/{}ms{RESET}", status_color(sp), sp, bp.len(), msp);
            println!("       piece   → {}{}{RESET} {GREY}{}b/{}ms{RESET}", status_color(sf), sf, bf.len(), msf);
            tokio::fs::write(out_dir.join("puzzle.jpg"), &bp).await?;
            tokio::fs::write(out_dir.join("piece.png"), &bf).await?;
            let b64 = base64::engine::general_purpose::STANDARD;
            tokio::fs::write(out_dir.join("puzzle.b64"), b64.encode(&bp)).await?;
            tokio::fs::write(out_dir.join("piece.b64"), b64.encode(&bf)).await?;
        } else {
            println!("  {YELLOW}[4]{RESET} no puzzle/piece links in challenge HTML");
        }
    }

    println!("  {DIM}wrote {}  total {}ms{RESET}", out_dir.display(), t_all.elapsed().as_millis());
    Ok(())
}

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let target = args.get(1).map(String::as_str).unwrap_or("https://www.footlocker.com/");
    let out_dir = PathBuf::from(args.get(2).map(String::as_str).unwrap_or("run/last"));

    println!("{BOLD}dd-fetch{RESET} target={target} out={}", out_dir.display());
    if let Err(e) = fetch_run(target, &out_dir).await {
        eprintln!("{RED}error{RESET}: {e}");
        std::process::exit(1);
    }
}
