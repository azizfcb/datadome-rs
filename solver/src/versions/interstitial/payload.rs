use crate::fetch;
use crate::plv2;
use crate::profile::Profile;

pub struct Block {
    pub fields: Vec<(String, String)>,
}

impl Block {
    pub fn field(&self, name: &str) -> String {
        self.fields
            .iter()
            .find(|(key, _)| key == name)
            .map(|(_, value)| value.clone())
            .unwrap_or_default()
    }

    pub fn frame(&self, referer: &str) -> String {
        let host = self.field("host");
        let kind = if self.field("rt") == "c" { "captcha" } else { "interstitial" };
        let mut url = format!(
            "https://{host}/{kind}/?initialCid={}&hash={}&cid={}",
            escape(&self.field("cid")),
            escape(&self.field("hsh")),
            escape(&self.field("cookie"))
        );
        url.push_str(&format!("&referer={}", escape(referer)));
        url.push_str(&format!("&s={}", self.field("s")));
        let extra = self.field("e");
        if !extra.is_empty() {
            url.push_str(&format!("&e={extra}"));
        }
        url.push_str(&format!("&b={}", self.field("b")));
        url.push_str("&dm=cd");
        url
    }
}

pub fn read(page: &str) -> Option<Block> {
    let start = page.find("var dd=")? + 7;
    let rest = &page[start..];
    let open = rest.find('{')?;
    let close = rest.find('}')?;
    let body = &rest[open + 1..close];
    let mut fields = Vec::new();
    for piece in body.split(',') {
        let Some((name, value)) = piece.split_once(':') else { continue };
        fields.push((
            name.trim().trim_matches('\'').trim_matches('"').to_string(),
            value.trim().trim_matches('\'').trim_matches('"').to_string(),
        ));
    }
    if fields.is_empty() { None } else { Some(Block { fields }) }
}

pub fn run(profile: &Profile, target: &str) {
    let page = match std::env::var("DD_PAGE") {
        Ok(path) => std::fs::read_to_string(path).unwrap_or_default(),
        Err(_) => match fetch::document(profile, target, None) {
            Ok(reply) => {
                if std::env::var("DD_RUNS").is_err() {
                    std::fs::create_dir_all("run/live").ok();
                    std::fs::write("run/live/block.html", &reply.body).ok();
                }
                eprintln!("block {} {} bytes", reply.status, reply.body.len());
                reply.text()
            }
            Err(error) => {
                eprintln!("block {error}");
                return;
            }
        },
    };
    let Some(block) = read(&page) else {
        eprintln!("no dd block on the page");
        return;
    };
    let frame = block.frame(target);
    eprintln!("rt {} frame {frame}", block.field("rt"));

    let inner = match std::env::var("DD_FRAME") {
        Ok(path) => std::fs::read_to_string(path).unwrap_or_default(),
        Err(_) => match fetch::document(profile, &frame, None) {
            Ok(reply) => {
                if std::env::var("DD_RUNS").is_err() {
                    std::fs::write("run/live/frame.html", &reply.body).ok();
                }
                eprintln!("frame {} {} bytes", reply.status, reply.body.len());
                reply.text()
            }
            Err(error) => {
                eprintln!("frame {error}");
                return;
            }
        },
    };

    let Some(bundle) = biggest(&inner) else {
        eprintln!("no inline bundle");
        return;
    };
    if std::env::var("DD_RUNS").is_err() {
        std::fs::write("run/live/frame.js", &bundle).ok();
    }
    let raw = bundle.clone();
    let source = match std::env::var("DD_SOURCE") {
        Ok(path) => std::fs::read_to_string(path).unwrap_or_default(),
        Err(_) => deob::deobfuscate(&bundle).unwrap_or(bundle),
    };
    let frames = plv2::stack(&raw, &frame);
    eprintln!("stack {:?}", frames.as_deref().map(|found| &found[..found.len().min(60)]));
    if std::env::var("DD_RUNS").is_err() {
        std::fs::write("run/live/frame.deob.js", &source).ok();
    }
    eprintln!("bundle {} bytes", source.len());

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0.0, |gone| gone.as_millis() as f64);
    let origin = format!("https://{}", block.field("host"));
    let host = profile.host(&origin, "/interstitial/", inner.clone(), now);
    let (output, trace) = vm::plv3(&source, host, 6_000_000);
    eprintln!("vm {} steps note {:?} wanted {:?}", trace.steps, trace.note, trace.wanted);
    let plv3 = output.r.as_deref().map(vm::urlsafe).unwrap_or_default();
    eprintln!("plv3 {} chars", plv3.len());
    if std::env::var("DD_PLV3").is_ok() {
        if let Some(raw) = output.r.as_deref() {
            let body: String = plv2::unbase64(raw).iter().map(|byte| *byte as char).collect();
            eprintln!("plv3 body {body}");
        }
    }

    let Some(ddm) = object(&inner, "var ddm = {") else {
        eprintln!("no ddm in the frame");
        return;
    };
    let read = |name: &str| {
        ddm.iter()
            .find(|(key, _)| key == name)
            .map(|(_, value)| value.clone())
            .unwrap_or_else(|| decoded(&inner, name))
    };

    let Some(spec) = plv2::spec(&source) else {
        eprintln!("no encoder spec in the frame bundle");
        return;
    };
    let listed = plv2::fields(&source);
    let session = plv2::Session {
        heap: [4_294_705_152, 19_000_000, 11_500_000],
        seed: read("seed"),
        seed_env: read("userEnv"),
        spent: 0.7,
        elapsed: 1284.5,
        pressure: 0.5,
        seconds: (now / 1000.0) as i64,
        built: 1,
        script: None,
        draws: [0.0; 9],
        quota: 0,
        usage: 0,
        downlink: 10.0,
        rtt: 50,
        timing: [0.0; 19],
        protocol: "h2".to_string(),
    };
    let attest = wasm::attestation(
        &source,
        &wasm::attest::Env {
            user_env: read("userEnv"),
            touch: profile.touch,
            cores: profile.cores,
            outer_height: profile.outer_height,
        },
    );
    eprintln!("wasm attestation {attest:?}");
    let mut extras: Vec<(String, String)> = Vec::new();
    if let Some((first, second)) = attest {
        for field in &listed {
            let Some(rest) = field.value.split("[\"exports\"][\"").nth(1) else { continue };
            let Some((_, call)) = rest.split_once("\"]") else { continue };
            let empty = call.trim_start().starts_with("()");
            extras.push((
                field.value.clone(),
                if empty { second.to_string() } else { first.to_string() },
            ));
        }
    }
    let checks = plv2::checks(&source);
    let (mut fields, open) = plv2::build(
        profile,
        &listed,
        &session,
        checks.as_ref(),
        frames.as_deref(),
        &tagged(&source),
        &extras,
        Some(&spec),
    );
    eprintln!("interstitial fields {} open {}", fields.len(), open.len());
    if std::env::var("DD_OPEN").is_ok() {
        for note in &open {
            println!("{note}");
        }
        return;
    }

    let steps = plv2::emits(&source);
    eprintln!("driver emits {}", steps.len());
    for emit in &steps {
        let Some(value) = step(emit, &read, &output, now) else { continue };
        match fields.iter_mut().find(|(key, _)| key == &emit.key) {
            Some(slot) => slot.1 = value,
            None => fields.push((emit.key.clone(), value)),
        }
    }

    let payload = plv2::encode(&spec, &read("hash"), &read("cid"), &fields, now);
    let body = post(&read, &payload, &plv3);
    println!("{body}");
    if std::env::var("DD_POST").is_err() {
        return;
    }
    let url = format!("{origin}/interstitial/");
    match fetch::submit(profile, &url, &origin, &frame, &body) {
        Ok(reply) => {
            eprintln!("interstitial {} {:?}", reply.status, reply.headers);
            eprintln!("body {}", reply.text());
        }
        Err(error) => eprintln!("interstitial {error}"),
    }
}

fn tagged(source: &str) -> Vec<(String, String)> {
    let mut list = plv2::slots(source);
    for (name, role) in plv2::helpers(source) {
        list.push((format!("fn:{name}"), role.to_string()));
    }
    for (prop, role) in plv2::stores(source) {
        list.push((format!("st:{prop}"), role.to_string()));
    }
    list
}

fn post(read: &dyn Fn(&str) -> String, payload: &str, plv3: &str) -> String {
    let mut body = String::new();
    let mut add = |name: &str, value: &str| {
        if !body.is_empty() {
            body.push('&');
        }
        body.push_str(name);
        body.push('=');
        body.push_str(&escape(value));
    };
    add("cid", &read("cid"));
    add("hash", &read("hash"));
    add("referer", &read("referer"));
    add("url", &read("url"));
    add("s", &read("s"));
    add("e", &read("e"));
    add("env", &read("env"));
    add("userEnv", &read("userEnv"));
    add("seed", &read("seed"));
    add("b", &read("b"));
    add("dm", &read("dm"));
    add("ddMessageFormat", &read("sdkMsgFormat"));
    add("payload", payload);
    if !plv3.is_empty() {
        add("plv3", plv3);
    }
    add("ps", "0");
    body
}

fn step(
    emit: &plv2::Emit,
    read: &dyn Fn(&str) -> String,
    output: &vm::Output,
    now: f64,
) -> Option<String> {
    let argument = emit.argument.as_str();
    if let Some(text) = literal(argument) {
        return Some(format!("\"{text}\""));
    }
    if argument.contains("fastMode") {
        let shown = read("displayEnabled") == "true";
        return Some(format!("\"{}\"", if shown { "display" } else { "invisible" }));
    }
    if argument.contains(".seed") {
        return Some(format!("{}", spread(&read("seed"))));
    }
    if argument.ends_with(".i") {
        return Some(format!("{}", output.i));
    }
    if argument.ends_with(".u") {
        return match &output.u {
            Some(found) => Some(format!("\"{found}\"")),
            None => Some("undefined".to_string()),
        };
    }
    if argument.contains(".e") || argument.contains("message") {
        return None;
    }
    if argument.contains('-') && argument.len() <= 6 {
        return Some(format!("{}", output.t.max(1)));
    }
    if argument.len() <= 2 {
        let _ = now;
        return Some("0".to_string());
    }
    None
}

fn literal(argument: &str) -> Option<String> {
    let body = argument.trim();
    if body.len() < 2 {
        return None;
    }
    let head = body.chars().next()?;
    if head != '"' && head != '\'' {
        return None;
    }
    if !body.ends_with(head) {
        return None;
    }
    Some(body[1..body.len() - 1].to_string())
}

fn spread(seed: &str) -> u32 {
    seed.chars().map(|letter| (letter as u32) % 240).sum()
}

fn decoded(page: &str, name: &str) -> String {
    let anchor = format!("ddm.{name} = htmlDecode(");
    let Some(spot) = page.find(&anchor) else { return String::new() };
    let rest = &page[spot + anchor.len()..];
    let Some(stop) = rest.find(')') else { return String::new() };
    unescape(rest[..stop].trim().trim_matches(|c| c == '\'' || c == '"'))
}

fn unescape(body: &str) -> String {
    body.replace("&amp;", "&").replace("&#x2d;", "-").replace("&quot;", "\"")
}

fn object(page: &str, head: &str) -> Option<Vec<(String, String)>> {
    let start = page.find(head)? + head.len();
    let rest = &page[start..];
    let mut depth = 1i32;
    let mut stop = rest.len();
    for (at, letter) in rest.char_indices() {
        match letter {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    stop = at;
                    break;
                }
            }
            _ => {}
        }
    }
    let mut found = Vec::new();
    for piece in rest[..stop].split(',') {
        let Some((name, value)) = piece.split_once(':') else { continue };
        let name = name.trim();
        if name.is_empty() || name.contains(char::is_whitespace) {
            continue;
        }
        found.push((
            name.trim_matches(|c| c == '\'' || c == '"').to_string(),
            unescape(value.trim().trim_matches(|c| c == '\'' || c == '"')),
        ));
    }
    Some(found)
}

fn biggest(page: &str) -> Option<String> {
    let mut best: Option<&str> = None;
    let mut at = 0usize;
    while let Some(open) = page[at..].find("<script") {
        let start = at + open;
        let Some(head) = page[start..].find('>') else { break };
        let body = start + head + 1;
        let Some(stop) = page[body..].find("</script>") else { break };
        let text = &page[body..body + stop];
        if best.map_or(true, |found| text.len() > found.len()) {
            best = Some(text);
        }
        at = body + stop;
    }
    best.map(|found| found.to_string())
}

fn escape(body: &str) -> String {
    let mut out = String::new();
    for byte in body.as_bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'!' | b'~' | b'*'
            | b'\'' | b'(' | b')' => out.push(*byte as char),
            other => out.push_str(&format!("%{other:02X}")),
        }
    }
    out
}
