mod fetch;
mod image;
mod mouse;
mod plv2;
mod profile;
mod versions;

fn main() {
    let mut args = std::env::args().skip(1);
    let number: u32 = args.next().and_then(|value| value.parse().ok()).unwrap_or(1);
    let kind = args.next().unwrap_or_else(|| "tags".to_string());
    let target = args.next().unwrap_or_else(|| "https://www.vinted.fr/".to_string());

    let profile = profile::load(number);
    eprintln!("{} {} {}", profile.identity, profile.agent(), profile.platform);
    let missing = profile.missing();
    if !missing.is_empty() {
        eprintln!("machine constants missing: {}", missing.join(" "));
    }

    if kind == "image" {
        let Ok(body) = std::fs::read(&target) else {
            eprintln!("no image at {target}");
            return;
        };
        match image::decode(&body) {
            Some(picture) => {
                eprintln!("image {}x{}", picture.width, picture.height);
                let mut out = format!("P3\n{} {}\n255\n", picture.width, picture.height);
                for y in 0..picture.height {
                    for x in 0..picture.width {
                        let (r, g, b) = picture.at(x, y);
                        out.push_str(&format!("{r} {g} {b} "));
                    }
                    out.push('\n');
                }
                std::fs::write("run/c/puzzle.ppm", out).ok();
                let energy = image::profile(&picture);
                let mut ranked: Vec<(usize, f64)> =
                    energy.iter().copied().enumerate().collect();
                ranked.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
                eprintln!("top columns {:?}", &ranked[..12.min(ranked.len())]);
                eprintln!("notch {:?}", image::notch(&picture, 55));
            }
            None => eprintln!("decode failed"),
        }
        return;
    }

    let runs: usize = std::env::var("DD_RUNS").ok().and_then(|v| v.parse().ok()).unwrap_or(1);
    if runs <= 1 {
        solve(profile, &kind, &target);
        return;
    }
    let mut crew = Vec::new();
    for worker in 0..runs {
        let kind = kind.clone();
        let target = target.clone();
        let picked = profile::load(if number == 0 { (worker as u32 % 5) + 1 } else { number });
        crew.push(std::thread::spawn(move || {
            eprintln!("worker {worker} {}", picked.identity);
            solve(picked, &kind, &target);
        }));
    }
    for hand in crew {
        hand.join().ok();
    }
}

fn solve(profile: &'static profile::Profile, kind: &str, target: &str) {
    match kind {
        "slider" | "captcha" => versions::slider::payload::run(profile, target),
        "interstitial" => versions::interstitial::payload::run(profile, target),
        _ => versions::tags::payload::run(profile, target),
    }
}
