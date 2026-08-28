use crate::fetch;
use crate::mouse;
use crate::plv2;
use crate::profile::Profile;

pub struct Challenge {
    pub order: Vec<(String, Source)>,
    pub ddm: Vec<(String, String)>,
    pub config: Vec<(String, String)>,
    pub referer: String,
}

pub enum Source {
    Literal(String),
    Member(String),
    Built(String),
}

pub fn read(page: &str) -> Option<Challenge> {
    let mut order = Vec::new();
    let mut rest = page;
    while let Some(spot) = rest.find("getRequest += ") {
        rest = &rest[spot + 14..];
        let Some(stop) = rest.find(';') else { break };
        let line = &rest[..stop];
        rest = &rest[stop..];
        let Some(name) = between(line, "'&", "=") else { continue };
        let Some(inner) = between(line, "encodeURIComponent(", ")") else { continue };
        let inner = inner.trim();
        let source = if inner.starts_with('\'') || inner.starts_with('"') {
            Source::Literal(inner.trim_matches(|c| c == '\'' || c == '"').to_string())
        } else if let Some(field) = inner.strip_prefix("ddm.") {
            Source::Member(field.to_string())
        } else {
            Source::Built(inner.to_string())
        };
        order.push((name, source));
    }
    if order.is_empty() {
        return None;
    }
    let mut challenge = Challenge {
        order,
        ddm: object(page, "var ddm = {")?,
        config: object(page, "sliderCaptcha({").unwrap_or_default(),
        referer: between(page, "ddm.referer = htmlDecode(", ")")
            .map(|body| body.trim().trim_matches(|c| c == '\'' || c == '"').to_string())
            .unwrap_or_default(),
    };
    challenge.order.insert(0, ("cid".to_string(), Source::Member("cid".to_string())));
    Some(challenge)
}

impl Challenge {
    pub fn field(&self, name: &str) -> String {
        self.ddm
            .iter()
            .find(|(key, _)| key == name)
            .map(|(_, value)| value.clone())
            .unwrap_or_default()
    }

    pub fn option(&self, name: &str) -> String {
        self.config
            .iter()
            .find(|(key, _)| key == name)
            .map(|(_, value)| value.clone())
            .unwrap_or_default()
    }

    pub fn measure(&self, name: &str, fallback: f64) -> f64 {
        self.option(name).parse().unwrap_or(fallback)
    }

    pub fn landing(&self) -> f64 {
        self.measure("width", 280.0)
            - self.measure("sliderR", 9.0)
            - self.measure("offset", 5.0)
    }

    pub fn puzzle(&self) -> bool {
        self.field("noPuzzle") != "true"
    }
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
    let body = &rest[..stop];
    let mut found = Vec::new();
    let mut depth = 0i32;
    let mut piece = String::new();
    for letter in body.chars() {
        match letter {
            '{' | '[' | '(' => {
                depth += 1;
                piece.push(letter);
            }
            '}' | ']' | ')' => {
                depth -= 1;
                piece.push(letter);
            }
            ',' if depth == 0 => {
                entry(&mut found, &piece);
                piece.clear();
            }
            other => piece.push(other),
        }
    }
    entry(&mut found, &piece);
    Some(found)
}

fn entry(into: &mut Vec<(String, String)>, piece: &str) {
    let Some((name, value)) = piece.split_once(':') else { return };
    let name = name.trim();
    if name.is_empty() || name.contains(char::is_whitespace) {
        return;
    }
    let value = value.trim().trim_matches(|c| c == '\'' || c == '"').trim().to_string();
    into.push((name.to_string(), value));
}

pub fn check(challenge: &Challenge, plv3: &str, payload: &str) -> String {
    let mut query = String::new();
    let mut add = |name: &str, value: &str| {
        if !query.is_empty() {
            query.push('&');
        }
        query.push_str(name);
        query.push('=');
        query.push_str(&escape(value));
    };
    for (name, source) in &challenge.order {
        let value = match source {
            Source::Literal(found) => found.clone(),
            Source::Member(found) => {
                if found == "referer" { challenge.referer.clone() } else { challenge.field(found) }
            }
            Source::Built(found) => match found.as_str() {
                "window.captchaEncodedPayload" => payload.to_string(),
                "window.plv3" => plv3.to_string(),
                "parentFrameUrl" => challenge.referer.clone(),
                other => challenge.field(other),
            },
        };
        if matches!(source, Source::Built(_)) && value.is_empty() {
            continue;
        }
        add(name, &value);
    }
    query
}

pub fn run(profile: &Profile, target: &str) {
    let page = match std::env::var("DD_CAPTCHA") {
        Ok(path) => std::fs::read_to_string(path).unwrap_or_default(),
        Err(_) => match fetch::document(profile, target, None) {
            Ok(reply) => reply.text(),
            Err(error) => {
                eprintln!("captcha {error}");
                return;
            }
        },
    };
    let Some(challenge) = read(&page) else {
        eprintln!("no challenge in page");
        return;
    };
    eprintln!(
        "hash {} s {} puzzle {} landing {}",
        challenge.field("hash"),
        challenge.field("s"),
        challenge.puzzle(),
        challenge.landing()
    );

    let source = match std::env::var("DD_SCRIPT") {
        Ok(path) => match std::fs::read_to_string(&path) {
            Ok(found) => found,
            Err(_) => {
                eprintln!("no script at {path}");
                return;
            }
        },
        Err(_) => {
            let Some(bundle) = biggest(&page) else {
                eprintln!("no inline bundle");
                return;
            };
            eprintln!("bundle {} bytes", bundle.len());
            deob::deobfuscate(&bundle).unwrap_or(bundle)
        }
    };

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0.0, |gone| gone.as_millis() as f64);
    let origin = root(target);
    let host = profile.host(&origin, "/captcha/", page.clone(), now);
    let (found, trace) = vm::plv3(&source, host, 4_000_000);
    eprintln!("vm {} steps note {:?} wanted {:?}", trace.steps, trace.note, trace.wanted);
    let plv3 = found.r.map(|body| vm::urlsafe(&body)).unwrap_or_default();

    let target = match challenge.puzzle() {
        true => puzzle(profile, &challenge).unwrap_or_else(|| challenge.landing()),
        false => challenge.landing(),
    };
    eprintln!("target {target}");
    let trail = mouse::drag(profile, target, now);
    let payload = answer(profile, &source, &challenge, &trail, now, target);
    eprintln!("plv3 {} chars payload {} chars trail {}", plv3.len(), payload.len(), trail.len());

    let query = check(&challenge, &plv3, &payload);
    let url = format!("{origin}/captcha/check?{query}");
    println!("{url}");
    if std::env::var("DD_POST").is_err() {
        return;
    }
    match fetch::xhr(profile, &url, &origin, &format!("{origin}/captcha/")) {
        Ok(reply) => eprintln!("check {} {}", reply.status, reply.text()),
        Err(error) => eprintln!("check {error}"),
    }
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

fn root(target: &str) -> String {
    let rest = target.strip_prefix("https://").or_else(|| target.strip_prefix("http://"));
    match rest {
        Some(body) => {
            let host = body.split('/').next().unwrap_or("geo.captcha-delivery.com");
            format!("https://{host}")
        }
        None => "https://geo.captcha-delivery.com".to_string(),
    }
}

fn puzzle(profile: &Profile, challenge: &Challenge) -> Option<f64> {
    let path = challenge.option("captchaChallengePath");
    if path.is_empty() {
        return None;
    }
    let reply = fetch::script(profile, &path, "https://geo.captcha-delivery.com/").ok()?;
    let picture = crate::image::decode(&reply.body)?;
    let (left, span) = crate::image::notch(&picture, 55)?;
    eprintln!("notch at {left} span {span}");
    Some(left as f64 - challenge.measure("offset", 5.0))
}

fn answer(
    profile: &Profile,
    source: &str,
    challenge: &Challenge,
    trail: &[mouse::Point],
    now: f64,
    target: f64,
) -> String {
    let Some(spec) = plv2::spec(source) else { return String::new() };
    let listed = plv2::fields(source);
    let session = plv2::Session {
        heap: [4_294_705_152, 19_000_000, 11_500_000],
        seed: String::new(),
        seed_env: String::new(),
        spent: 0.7,
        elapsed: trail.last().map_or(0.0, |point| point.at),
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
        source,
        &wasm::attest::Env {
            user_env: challenge.field("userEnv"),
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
    let checks = plv2::checks(source);
    let (mut fields, open) = plv2::build(
        profile,
        &listed,
        &session,
        checks.as_ref(),
        None,
        &tagged(source),
        &extras,
        Some(&spec),
    );
    eprintln!("captcha fields {} open {}", fields.len(), open.len());
    if std::env::var("DD_OPEN").is_ok() {
        for note in &open {
            println!("{note}");
        }
    }
    let moves = plv2::within(source, "computeSignals = function");
    let initial = mouse::wander(profile, now);
    let signals = mouse::signals(trail, &initial);
    eprintln!("computeSignals emits {}", moves.len());
    if let Some(found) = &signals {
        eprintln!(
            "signals left {:.1} right {:.1} up {:.1} down {:.1} speed {:.1}/{:.1} straight {:.4} areas {:.1}/{:.1} segments {}",
            found.left, found.right, found.up, found.down, found.speed_avg, found.speed_sd,
            found.straight, found.lower, found.upper, found.segments
        );
    }
    if let Some(found) = &signals {
        for emit in &moves {
            let Some(value) = mouse::mapped(emit, found) else { continue };
            match fields.iter_mut().find(|(key, _)| key == &emit.key) {
                Some(slot) => slot.1 = value,
                None => fields.push((emit.key.clone(), value)),
            }
        }
    }

    let sent = plv2::emits(source);
    eprintln!("sendPayload emits {}", sent.len());
    for emit in &sent {
        let Some(value) = slider(emit, challenge, trail, target) else { continue };
        match fields.iter_mut().find(|(key, _)| key == &emit.key) {
            Some(slot) => slot.1 = value,
            None => fields.push((emit.key.clone(), value)),
        }
    }
    eprintln!("payload fields {}", fields.len());
    plv2::encode(&spec, &challenge.field("hash"), &challenge.field("cid"), &fields, now)
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

fn slider(
    emit: &plv2::Emit,
    challenge: &Challenge,
    trail: &[mouse::Point],
    target: f64,
) -> Option<String> {
    let spent = trail.last().map_or(0.0, |point| point.at) as i64;
    let argument = emit.argument.as_str();
    if argument.contains("audioAnswer") {
        return Some("\"\"".to_string());
    }
    if argument.contains("\"audio\"") {
        let mode = if challenge.puzzle() { "puzzle" } else { "simple" };
        return Some(format!("\"{mode}\""));
    }
    if argument.contains("style.left") || argument.contains("parseInt") {
        return Some(format!("{}", target as i64));
    }
    if argument.contains("displayStartTime") {
        return Some(format!("{}", spent + 640));
    }
    if argument.contains("challengeStartTime") {
        return Some(format!("{spent}"));
    }
    if argument.len() <= 2 {
        return Some("false".to_string());
    }
    None
}

fn between(page: &str, head: &str, tail: &str) -> Option<String> {
    let start = page.find(head)? + head.len();
    let rest = &page[start..];
    let stop = rest.find(tail)?;
    Some(rest[..stop].to_string())
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

