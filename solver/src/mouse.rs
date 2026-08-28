use crate::profile::Profile;

pub struct Point {
    pub x: f64,
    pub y: f64,
    pub at: f64,
}

pub fn wander(profile: &Profile, now: f64) -> Vec<Point> {
    let mut seed = (now as u64) ^ 0x9e3779b97f4a7c15 | 1;
    let mut roll = || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        (seed >> 11) as f64 / (1u64 << 53) as f64
    };
    let wide = f64::from(profile.inner_width());
    let tall = f64::from(profile.inner_height());
    let mut x = wide * (0.2 + roll() * 0.6);
    let mut y = tall * (0.2 + roll() * 0.6);
    let mut at = 300.0 + roll() * 900.0;
    let mut trail = Vec::new();
    let legs = 3 + (roll() * 4.0) as usize;
    for _ in 0..legs {
        let goal_x = wide * (0.08 + roll() * 0.84);
        let goal_y = tall * (0.08 + roll() * 0.84);
        let steps = 12 + (roll() * 26.0) as usize;
        let span = 220.0 + roll() * 520.0;
        let from = (x, y);
        for step in 1..=steps {
            let share = step as f64 / steps as f64;
            let eased = ease(share);
            let sway = (share * std::f64::consts::PI).sin() * (roll() * 6.0 - 3.0);
            x = from.0 + (goal_x - from.0) * eased;
            y = from.1 + (goal_y - from.1) * eased + sway;
            at += span / steps as f64;
            trail.push(Point { x: x.round(), y: y.round(), at: at.round() });
        }
        at += 60.0 + roll() * 400.0;
    }
    trail
}

pub fn drag(profile: &Profile, distance: f64, now: f64) -> Vec<Point> {
    let mut seed = (now as u64) | 1;
    let mut roll = || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        (seed >> 11) as f64 / (1u64 << 53) as f64
    };
    let start = f64::from(profile.inner_width()) / 2.0 - distance / 2.0;
    let base = f64::from(profile.inner_height()) / 2.0 + 60.0;
    let steps = 28 + (roll() * 14.0) as usize;
    let span = 620.0 + roll() * 480.0;
    let mut trail = Vec::with_capacity(steps + 1);
    for step in 0..=steps {
        let share = step as f64 / steps as f64;
        let eased = ease(share);
        let jitter = (roll() - 0.5) * 1.6;
        let sway = (share * std::f64::consts::TAU).sin() * 3.4
            + (share * std::f64::consts::PI * 3.0).sin() * 1.1;
        trail.push(Point {
            x: (start + distance * eased + jitter).round(),
            y: (base + sway + (roll() - 0.5) * 1.2).round(),
            at: (span * pace(share)).round(),
        });
    }
    trail
}

fn ease(share: f64) -> f64 {
    if share < 0.5 {
        4.0 * share * share * share
    } else {
        let back = -2.0 * share + 2.0;
        1.0 - back * back * back / 2.0
    }
}

fn pace(share: f64) -> f64 {
    share.powf(0.86)
}

pub struct Signals {
    pub segments: usize,
    pub left: f64,
    pub right: f64,
    pub up: f64,
    pub down: f64,
    pub y_avg: f64,
    pub y_sd: f64,
    pub speed_avg: f64,
    pub speed_sd: f64,
    pub x_speed_avg: f64,
    pub x_speed_sd: f64,
    pub y_speed_avg: f64,
    pub y_speed_sd: f64,
    pub straight: f64,
    pub below: f64,
    pub above: f64,
    pub lower: f64,
    pub upper: f64,
    pub count: usize,
}

pub fn segments(trail: &[Point]) -> usize {
    if trail.is_empty() {
        return 0;
    }
    let mut count = 1usize;
    let mut last = trail[0].at;
    for point in trail.iter().skip(1) {
        if point.at - last > 750.0 {
            count += 1;
        }
        last = point.at;
    }
    count
}

pub fn signals(trail: &[Point], initial: &[Point]) -> Option<Signals> {
    if trail.len() < 3 {
        return None;
    }
    let mut left = 0.0;
    let mut right = 0.0;
    let mut up = 0.0;
    let mut down = 0.0;
    for pair in trail.windows(2) {
        let (first, next) = (&pair[0], &pair[1]);
        let dx = (first.x - next.x).abs();
        let dy = (first.y - next.y).abs();
        if next.x < first.x {
            left += dx;
        } else {
            right += dx;
        }
        if next.y < first.y {
            up += dy;
        } else {
            down += dy;
        }
    }
    let ys: Vec<f64> = trail.iter().map(|point| point.y).collect();
    let width = trail.len().min(5);
    let mut speeds = Vec::new();
    let mut x_speeds = Vec::new();
    let mut y_speeds = Vec::new();
    for window in trail.windows(width) {
        let first = &window[0];
        let last = &window[window.len() - 1];
        let seconds = (last.at - first.at) / 1000.0;
        if seconds == 0.0 {
            continue;
        }
        let dx = last.x - first.x;
        let dy = last.y - first.y;
        speeds.push((dx * dx + dy * dy).sqrt() / seconds);
        x_speeds.push(dx.abs() / seconds);
        y_speeds.push(dy.abs() / seconds);
    }
    let first = &trail[0];
    let last = &trail[trail.len() - 1];
    let direct = span(first, last);
    let walked: f64 = trail.windows(2).map(|pair| span(&pair[0], &pair[1])).sum();
    let slope = (last.y - first.y) / (last.x - first.x);
    let base = first.y - slope * first.x;
    let mut below = Vec::new();
    let mut above = Vec::new();
    for point in trail {
        let gap = ((last.x - first.x) * (first.y - point.y)
            - (first.x - point.x) * (last.y - first.y))
            .abs()
            / ((last.x - first.x).powi(2) + (last.y - first.y).powi(2)).sqrt();
        let side = point.y - (slope * point.x + base);
        if side >= 0.0 {
            below.push(gap);
        }
        if side <= 0.0 {
            above.push(gap);
        }
    }
    let (lower, upper) = areas(&buckets(trail, 30));
    Some(Signals {
        segments: segments(initial),
        left,
        right,
        up,
        down,
        y_avg: mean(&ys),
        y_sd: spread(&ys),
        speed_avg: mean(&speeds),
        speed_sd: spread(&speeds),
        x_speed_avg: mean(&x_speeds),
        x_speed_sd: spread(&x_speeds),
        y_speed_avg: mean(&y_speeds),
        y_speed_sd: spread(&y_speeds),
        straight: if walked == 0.0 { 0.0 } else { direct / walked },
        below: peak(&below),
        above: peak(&above),
        lower,
        upper,
        count: trail.len(),
    })
}

fn span(first: &Point, next: &Point) -> f64 {
    let dx = next.x - first.x;
    let dy = next.y - first.y;
    (dx * dx + dy * dy).sqrt()
}

fn mean(list: &[f64]) -> f64 {
    if list.is_empty() {
        return 0.0;
    }
    list.iter().sum::<f64>() / list.len() as f64
}

fn spread(list: &[f64]) -> f64 {
    if list.is_empty() {
        return 0.0;
    }
    let middle = mean(list);
    let total: f64 = list.iter().map(|value| (middle - value).powi(2)).sum();
    (total / list.len() as f64).sqrt()
}

fn peak(list: &[f64]) -> f64 {
    list.iter().copied().fold(f64::NEG_INFINITY, f64::max)
}

fn buckets(trail: &[Point], count: usize) -> Vec<(f64, f64)> {
    let mut low = &trail[0];
    let mut high = &trail[0];
    for point in trail.iter().skip(1) {
        if point.x < low.x || (point.x == low.x && point.y > low.y) {
            low = point;
        }
        if point.x > high.x || (point.x == high.x && point.y < high.y) {
            high = point;
        }
    }
    let step = (high.x - low.x) / count as f64;
    let mut bounds: Vec<f64> = (0..count).map(|at| low.x + at as f64 * step).collect();
    bounds.push(high.x);
    let mut sorted: Vec<Vec<f64>> = vec![Vec::new(); count];
    for point in trail {
        for slot in 0..count {
            if point.x <= bounds[slot + 1] {
                sorted[slot].push(point.y);
                break;
            }
        }
    }
    let mut out = Vec::new();
    for slot in 0..count {
        if !sorted[slot].is_empty() {
            out.push((bounds[slot], mean(&sorted[slot])));
        }
    }
    out
}

fn areas(marks: &[(f64, f64)]) -> (f64, f64) {
    if marks.len() < 2 {
        return (0.0, 0.0);
    }
    let first = marks[0];
    let last = marks[marks.len() - 1];
    let slope = (last.1 - first.1) / (last.0 - first.0);
    let base = first.1 - slope * first.0;
    let mut lower = 0.0;
    let mut upper = 0.0;
    for pair in marks.windows(2) {
        let (one, two) = (pair[0], pair[1]);
        let line_one = slope * one.0 + base;
        let line_two = slope * two.0 + base;
        let area =
            (two.0 - one.0) * ((line_one - one.1).abs() + (line_two - two.1).abs()) / 2.0;
        if (one.1 + two.1) / 2.0 < slope * (one.0 + two.0) / 2.0 + base {
            upper += area;
        } else {
            lower += area;
        }
    }
    (lower, upper)
}

pub fn mapped(emit: &crate::plv2::Emit, found: &Signals) -> Option<String> {
    let body = emit.argument.as_str();
    let cut = |value: f64| format!("{}", (value * 1e10).round() / 1e10);
    let tail = body.rsplit('.').next().unwrap_or("");
    let value = match tail {
        "left" if body.starts_with(|c: char| c.is_alphabetic()) && body.contains('.') => {
            Some(found.left)
        }
        "right" => Some(found.right),
        "up" => Some(found.up),
        "down" => Some(found.down),
        "yAvg" => Some(found.y_avg),
        "ySD" => Some(found.y_sd),
        "lower" => Some(found.lower),
        "upper" => Some(found.upper),
        "length" if body.contains("_coordsList") => Some(found.count as f64),
        _ => None,
    };
    if let Some(number) = value {
        return Some(cut(number));
    }
    if body.contains("_getStraigthness") {
        return Some(cut(found.straight));
    }
    if body.contains("_untrustedEventsCount") || body.contains("_coalescedEventsCount") {
        return Some("0".to_string());
    }
    if body.contains(".length") && body.contains("segments") {
        return Some(found.segments.to_string());
    }
    let stat = |mean: f64, sd: f64| -> Option<String> {
        if body.starts_with("C(") {
            Some(cut(mean))
        } else if body.starts_with("P(") {
            Some(cut(sd))
        } else {
            None
        }
    };
    if body.contains(".xSpeeds") {
        return stat(found.x_speed_avg, found.x_speed_sd);
    }
    if body.contains(".ySpeeds") {
        return stat(found.y_speed_avg, found.y_speed_sd);
    }
    if body.contains(".speeds") {
        return stat(found.speed_avg, found.speed_sd);
    }
    if body.contains(".below") {
        return Some(cut(found.below));
    }
    if body.contains(".above") {
        return Some(cut(found.above));
    }
    None
}

