use crate::fetch;
use crate::plv2;
use crate::profile::Profile;

fn release(url: &str) -> String {
    for piece in url.split('/') {
        let parts: Vec<&str> = piece.split('.').collect();
        if parts.len() == 3 && parts.iter().all(|part| part.parse::<u32>().is_ok()) {
            return piece.to_string();
        }
    }
    "5.9.3".to_string()
}

fn script(page: &str) -> Option<String> {
    let spot = page.find("tags.js")?;
    let head = page[..spot].rfind("https://")?;
    Some(page[head..spot + 7].to_string())
}

fn root(target: &str) -> String {
    let rest = target.strip_prefix("https://").or_else(|| target.strip_prefix("http://"));
    match rest {
        Some(body) => format!("https://{}", body.split('/').next().unwrap_or(body)),
        None => target.trim_end_matches('/').to_string(),
    }
}

fn client(page: &str) -> Option<String> {
    for head in ["DATADOME_CLIENT_SIDE_KEY", "ddjskey", "dd_key", "hsh"] {
        let mut at = 0usize;
        while let Some(spot) = page[at..].find(head) {
            let start = at + spot + head.len();
            at = start;
            let body: String = page[start..]
                .chars()
                .skip_while(|c| !c.is_ascii_alphanumeric())
                .take_while(|c| c.is_ascii_alphanumeric())
                .collect();
            if body.len() >= 20
                && body.len() <= 40
                && body.chars().all(|c| c.is_ascii_digit() || c.is_ascii_uppercase())
            {
                return Some(body);
            }
        }
    }
    None
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

pub fn run(profile: &Profile, target: &str) {
    let mut page = String::new();
    if std::env::var("DD_OFFLINE").is_err() {
        match fetch::document(profile, target, None) {
            Ok(reply) => {
                eprintln!("page {} {} bytes", reply.status, reply.body.len());
                page = reply.text();
                if std::env::var("DD_RUNS").is_err() {
                    std::fs::create_dir_all("run/live").ok();
                    std::fs::write("run/live/page.html", &reply.body).ok();
                }
            }
            Err(error) => eprintln!("fetch {error}"),
        }
    }

    let source_url = script(&page).unwrap_or_else(|| "https://js.datadome.co/tags.js".to_string());
    let bundle = match std::env::var("DD_BUNDLE") {
        Ok(path) => std::fs::read_to_string(path).ok(),
        Err(_) => fetch::script(profile, &source_url, target).ok().map(|reply| reply.text()),
    };
    eprintln!("bundle {} from {source_url}", bundle.as_ref().map_or(0, |body| body.len()));

    let source = match std::env::var("DD_SCRIPT") {
        Ok(path) => match std::fs::read_to_string(&path) {
            Ok(found) => found,
            Err(_) => {
                eprintln!("no script at {path}");
                return;
            }
        },
        Err(_) => match bundle.as_deref().map(deob::deobfuscate) {
            Some(Ok(found)) => found,
            _ => {
                eprintln!("no bundle to deobfuscate");
                return;
            }
        },
    };
    let Some(spec) = plv2::spec(&source) else {
        eprintln!("no encoder spec");
        return;
    };
    eprintln!("build {} mixer {} second {}", spec.build, spec.mixer, spec.second);

    let listed = plv2::fields(&source);
    if std::env::var("DD_FIELDS").is_ok() {
        for field in &listed {
            println!(
                "{}\t{}\t{}",
                if field.deferred { "late" } else { "sync" },
                field.key,
                field.value.replace('\n', " ")
            );
        }
        return;
    }

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |gone| gone.as_millis() as u64);
    let mut seed = now | 1;
    let mut roll = || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        (seed >> 11) as f64 / (1u64 << 53) as f64
    };
    let mut draws = [0.0f64; 9];
    for slot in draws.iter_mut() {
        *slot = ((roll() * 15.0) * 100.0).round() / 100.0;
    }

    let trace = bundle.as_deref().and_then(|found| plv2::stack(found, &source_url));
    let session = plv2::Session {
        heap: [4_294_705_152, 19_000_000, 11_500_000],
        seed: String::new(),
        seed_env: String::new(),
        spent: 0.7,
        elapsed: 1284.5,
        pressure: 0.5,
        seconds: (now / 1000) as i64,
        built: 1,
        script: bundle.as_deref().map(|found| plv2::sha256(found.as_bytes())),
        draws,
        quota: 0,
        usage: 0,
        downlink: 10.0,
        rtt: 50,
        timing: [0.0; 19],
        protocol: "h2".to_string(),
    };

    let checks = plv2::checks(&source);
    let (fields, open) =
        plv2::build(profile, &listed, &session, checks.as_ref(), trace.as_deref(), &tagged(&source), &[], Some(&spec));
    eprintln!("mapped {} open {:?}", fields.len(), &open[..open.len().min(40)]);

    let key = std::env::var("DD_KEY")
        .ok()
        .or_else(|| client(&page))
        .unwrap_or_else(|| "8C7191D8AA1BF5FBB1B84DC7268196".to_string());
    eprintln!("client key {key}");
    let made = plv2::encode(&spec, key.as_str(), ".keep", &fields, now as f64);
    eprintln!("payload {made}");
    match plv2::decode(&spec, key.as_str(), ".keep", &made) {
        Some(back) => eprintln!("decoded {back}"),
        None => eprintln!("decode failed"),
    }

    let counters = plv2::Counters {
        mousemove: 0,
        pointermove: 0,
        click: 0,
        scroll: 0,
        touchstart: 0,
        touchend: 0,
        touchmove: 0,
        keydown: 0,
        keyup: 0,
    };
    let sent = plv2::body(
        &made,
        &counters,
        "ch",
        ".keep",
        key.as_str(),
        target,
        "/",
        "origin",
        &std::env::var("DD_VERSION").unwrap_or_else(|_| release(&source_url)),
    );
    if std::env::var("DD_POST").is_err() {
        return;
    }
    let origin = std::env::var("DD_ORIGIN").unwrap_or_else(|_| root(target));
    match fetch::post(profile, "https://api-js.datadome.co/js/", &origin, &sent, None) {
        Ok(reply) => {
            eprintln!("post {} {}", reply.status, reply.text());
            let text = reply.text();
            if let Some(at) = text.find("datadome=") {
                let value = text[at + 9..].split(';').next().unwrap_or("");
                match fetch::document(profile, target, Some(&format!("datadome={value}"))) {
                    Ok(again) => eprintln!("recheck {} {} bytes", again.status, again.body.len()),
                    Err(error) => eprintln!("recheck {error}"),
                }
            }
        }
        Err(error) => eprintln!("post {error}"),
    }
}
