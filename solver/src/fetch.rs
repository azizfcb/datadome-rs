use std::process::Command;

use crate::profile::Profile;

pub struct Reply {
    pub status: u16,
    pub headers: Vec<(String, String)>,
    pub body: Vec<u8>,
}

impl Reply {
    pub fn text(&self) -> String {
        String::from_utf8_lossy(&self.body).to_string()
    }

    pub fn cookie(&self, name: &str) -> Option<String> {
        for (header, value) in &self.headers {
            if header != "set-cookie" {
                continue;
            }
            let (pair, _) = value.split_once(';').unwrap_or((value.as_str(), ""));
            if let Some((found, content)) = pair.split_once('=') {
                if found.trim() == name {
                    return Some(content.trim().to_string());
                }
            }
        }
        None
    }
}

pub fn document(profile: &Profile, url: &str, cookie: Option<&str>) -> Result<Reply, String> {
    let mut headers = vec![
        ("sec-ch-ua".to_string(), profile.brands()),
        ("sec-ch-ua-mobile".to_string(), "?0".to_string()),
        ("sec-ch-ua-platform".to_string(), format!("\"{}\"", profile.platform)),
        ("upgrade-insecure-requests".to_string(), "1".to_string()),
        ("user-agent".to_string(), profile.agent()),
        (
            "accept".to_string(),
            "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8"
                .to_string(),
        ),
        ("sec-fetch-site".to_string(), "none".to_string()),
        ("sec-fetch-mode".to_string(), "navigate".to_string()),
        ("sec-fetch-user".to_string(), "?1".to_string()),
        ("sec-fetch-dest".to_string(), "document".to_string()),
        ("accept-encoding".to_string(), "gzip, deflate, br".to_string()),
        ("accept-language".to_string(), profile.accept_language()),
    ];
    if let Some(value) = cookie {
        headers.push(("cookie".to_string(), value.to_string()));
    }
    send(profile, "GET", url, &headers, None)
}

pub fn script(profile: &Profile, url: &str, referer: &str) -> Result<Reply, String> {
    let headers = vec![
        ("sec-ch-ua".to_string(), profile.brands()),
        ("sec-ch-ua-mobile".to_string(), "?0".to_string()),
        ("sec-ch-ua-platform".to_string(), format!("\"{}\"", profile.platform)),
        ("user-agent".to_string(), profile.agent()),
        ("accept".to_string(), "*/*".to_string()),
        ("sec-fetch-site".to_string(), "cross-site".to_string()),
        ("sec-fetch-mode".to_string(), "no-cors".to_string()),
        ("sec-fetch-dest".to_string(), "script".to_string()),
        ("referer".to_string(), referer.to_string()),
        ("accept-encoding".to_string(), "gzip, deflate, br".to_string()),
        ("accept-language".to_string(), profile.accept_language()),
    ];
    send(profile, "GET", url, &headers, None)
}

pub fn xhr(profile: &Profile, url: &str, origin: &str, referer: &str) -> Result<Reply, String> {
    let headers = vec![
        ("sec-ch-ua".to_string(), profile.brands()),
        (
            "content-type".to_string(),
            "application/x-www-form-urlencoded; charset=UTF-8".to_string(),
        ),
        ("sec-ch-ua-mobile".to_string(), "?0".to_string()),
        ("user-agent".to_string(), profile.agent()),
        ("sec-ch-ua-platform".to_string(), format!("\"{}\"", profile.platform)),
        ("accept".to_string(), "*/*".to_string()),
        ("origin".to_string(), origin.to_string()),
        ("sec-fetch-site".to_string(), "same-origin".to_string()),
        ("sec-fetch-mode".to_string(), "cors".to_string()),
        ("sec-fetch-dest".to_string(), "empty".to_string()),
        ("referer".to_string(), referer.to_string()),
        ("accept-encoding".to_string(), "gzip, deflate, br".to_string()),
        ("accept-language".to_string(), profile.accept_language()),
    ];
    send(profile, "GET", url, &headers, None)
}

pub fn submit(
    profile: &Profile,
    url: &str,
    origin: &str,
    referer: &str,
    body: &str,
) -> Result<Reply, String> {
    let headers = vec![
        (
            "content-type".to_string(),
            "application/x-www-form-urlencoded; charset=UTF-8".to_string(),
        ),
        ("sec-ch-ua".to_string(), profile.brands()),
        ("sec-ch-ua-mobile".to_string(), "?0".to_string()),
        ("user-agent".to_string(), profile.agent()),
        ("sec-ch-ua-platform".to_string(), format!("\"{}\"", profile.platform)),
        ("accept".to_string(), "*/*".to_string()),
        ("origin".to_string(), origin.to_string()),
        ("sec-fetch-site".to_string(), "same-origin".to_string()),
        ("sec-fetch-mode".to_string(), "cors".to_string()),
        ("sec-fetch-dest".to_string(), "empty".to_string()),
        ("referer".to_string(), referer.to_string()),
        ("accept-encoding".to_string(), "gzip, deflate, br".to_string()),
        ("accept-language".to_string(), profile.accept_language()),
    ];
    send(profile, "POST", url, &headers, Some(body))
}

pub fn post(
    profile: &Profile,
    url: &str,
    origin: &str,
    body: &str,
    cookie: Option<&str>,
) -> Result<Reply, String> {
    let mut headers = vec![
        ("content-type".to_string(), "application/x-www-form-urlencoded".to_string()),
        ("sec-ch-ua".to_string(), profile.brands()),
        ("sec-ch-ua-mobile".to_string(), "?0".to_string()),
        ("user-agent".to_string(), profile.agent()),
        ("sec-ch-ua-platform".to_string(), format!("\"{}\"", profile.platform)),
        ("accept".to_string(), "*/*".to_string()),
        ("origin".to_string(), origin.to_string()),
        ("sec-fetch-site".to_string(), "cross-site".to_string()),
        ("sec-fetch-mode".to_string(), "cors".to_string()),
        ("sec-fetch-dest".to_string(), "empty".to_string()),
        ("referer".to_string(), format!("{origin}/")),
        ("accept-encoding".to_string(), "gzip, deflate, br".to_string()),
        ("accept-language".to_string(), profile.accept_language()),
    ];
    if let Some(value) = cookie {
        headers.push(("cookie".to_string(), value.to_string()));
    }
    send(profile, "POST", url, &headers, Some(body))
}

fn send(
    profile: &Profile,
    method: &str,
    url: &str,
    headers: &[(String, String)],
    body: Option<&str>,
) -> Result<Reply, String> {
    let tool = std::env::var("DD_FETCH").unwrap_or_else(|_| "/root/tlsclient/tlsfetch".to_string());
    let mut command = Command::new(tool);
    command.arg("-identity").arg(profile.identity).arg("-method").arg(method);
    if let Some(exit) = route() {
        command.arg("-proxy").arg(exit);
    }
    if let Some(payload) = body {
        command.arg("-body").arg(payload);
    }
    for (name, value) in headers {
        command.arg("-H").arg(format!("{name}: {value}"));
    }
    command.arg(url);

    let done = command.output().map_err(|error| error.to_string())?;
    let notes = String::from_utf8_lossy(&done.stderr);
    let mut status = 0;
    let mut collected = Vec::new();
    for line in notes.lines() {
        if let Some(code) = line.strip_prefix("status ") {
            status = code.trim().parse().unwrap_or(0);
            continue;
        }
        if let Some((name, value)) = line.split_once(": ") {
            collected.push((name.to_ascii_lowercase(), value.to_string()));
        }
    }
    if status == 0 {
        return Err(notes.trim().to_string());
    }
    Ok(Reply { status, headers: collected, body: done.stdout })
}

pub fn route() -> Option<String> {
    if let Ok(single) = std::env::var("DD_PROXY") {
        if !single.is_empty() {
            return Some(single);
        }
    }
    let path = std::env::var("DD_PROXIES").ok()?;
    let body = std::fs::read_to_string(path).ok()?;
    let list: Vec<&str> = body
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .collect();
    if list.is_empty() {
        return None;
    }
    let turn = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |gone| gone.as_nanos() as usize);
    Some(shape(list[turn % list.len()]))
}

fn shape(entry: &str) -> String {
    if entry.contains("://") {
        return entry.to_string();
    }
    let parts: Vec<&str> = entry.split(':').collect();
    match parts.len() {
        4 => format!("http://{}:{}@{}:{}", parts[2], parts[3], parts[0], parts[1]),
        2 => format!("http://{}:{}", parts[0], parts[1]),
        _ => entry.to_string(),
    }
}
