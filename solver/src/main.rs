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
        let rounds: usize = std::env::var("DD_ROUNDS")
            .ok()
            .and_then(|value| value.parse().ok())
            .unwrap_or(2000);
        let Some(picture) = image::decode(&body) else {
            eprintln!("decode failed");
            return;
        };
        let seal = picture
            .plane
            .iter()
            .fold(2166136261u32, |sum, byte| (sum ^ u32::from(*byte)).wrapping_mul(16777619));
        eprintln!("image {}x{} {} bytes plane {seal:08x}", picture.width, picture.height, body.len());
        eprintln!("notch {:?}", image::locate(&picture));
        let mut taken = Vec::with_capacity(rounds);
        for round in 0..rounds + rounds / 10 {
            let start = std::time::Instant::now();
            let found = image::decode(&body);
            let spent = start.elapsed();
            std::hint::black_box(&found);
            if round >= rounds / 10 {
                taken.push(spent.as_nanos() as u64);
            }
        }
        report("decode", &mut taken);

        taken.clear();
        for round in 0..rounds + rounds / 10 {
            let start = std::time::Instant::now();
            let found = image::locate(&picture);
            let spent = start.elapsed();
            std::hint::black_box(&found);
            if round >= rounds / 10 {
                taken.push(spent.as_nanos() as u64);
            }
        }
        report("detect", &mut taken);
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

fn report(name: &str, taken: &mut Vec<u64>) {
    taken.sort_unstable();
    let pick = |share: f64| taken[((taken.len() - 1) as f64 * share) as usize] as f64 / 1000.0;
    eprintln!(
        "{name} n={} min {:.1} p50 {:.1} p90 {:.1} p99 {:.1} max {:.1} us",
        taken.len(),
        taken[0] as f64 / 1000.0,
        pick(0.50),
        pick(0.90),
        pick(0.99),
        taken[taken.len() - 1] as f64 / 1000.0
    );
}

fn solve(profile: &'static profile::Profile, kind: &str, target: &str) {
    match kind {
        "slider" | "captcha" => versions::slider::payload::run(profile, target),
        "interstitial" => versions::interstitial::payload::run(profile, target),
        _ => versions::tags::payload::run(profile, target),
    }
}
