pub struct Profile {
    pub identity: &'static str,
    pub major: u32,
    pub full: &'static str,
    pub system: &'static str,
    pub platform: &'static str,
    pub navigator: &'static str,
    pub version: &'static str,
    pub architecture: &'static str,
    pub bitness: &'static str,
    pub model: &'static str,
    pub cores: u32,
    pub memory: u32,
    pub touch: u32,
    pub width: u32,
    pub height: u32,
    pub ratio: f64,
    pub depth: u32,
    pub top: u32,
    pub bottom: u32,
    pub outer_width: u32,
    pub outer_height: u32,
    pub chrome_height: u32,
    pub vendor: &'static str,
    pub renderer: &'static str,
    pub language: &'static str,
    pub languages: &'static [&'static str],
    pub timezone: &'static str,
    pub offset: i32,
    pub gamut: &'static str,
    pub range: &'static str,
    pub devices: &'static str,
    pub layout: u32,
    pub keys: &'static str,
    pub fonts: &'static str,
    pub families: &'static str,
    pub voices: &'static str,
    pub canvas: &'static str,
    pub globals: u32,
    pub latency: f64,
    pub rate: u32,
    pub buffer: u64,
    pub binding: u64,
    pub features: &'static str,
}

pub const MACBOOK_PRO_M3: Profile = Profile {
    identity: "chrome_146_PSK",
    major: 146,
    full: "146.0.7680.180",
    system: "Macintosh; Intel Mac OS X 10_15_7",
    platform: "macOS",
    navigator: "MacIntel",
    version: "26.6.2",
    architecture: "arm",
    bitness: "64",
    model: "",
    cores: 12,
    memory: 8,
    touch: 0,
    width: 1512,
    height: 982,
    ratio: 2.0,
    depth: 24,
    top: 37,
    bottom: 0,
    outer_width: 1512,
    outer_height: 945,
    chrome_height: 87,
    vendor: "Google Inc. (Apple)",
    renderer: "ANGLE (Apple, ANGLE Metal Renderer: Apple M3 Pro, Unspecified Version)",
    language: "en-US",
    languages: &["en-US", "en"],
    timezone: "Europe/Berlin",
    offset: -120,
    gamut: "p3",
    range: "high",
    devices: "k:ai,ao,vi",
    layout: 49,
    keys: "",
    fonts: "",
    families: "",
    voices: "",
    canvas: "",
    globals: 0,
    latency: 0.005333333333333333,
    rate: 48000,
    buffer: 4294967296,
    binding: 2147483644,
    features: "depth-clip-control,depth32float-stencil8,texture-compression-bc,texture-compression-bc-sliced-3d,texture-compression-etc2,texture-compression-astc,texture-compression-astc-sliced-3d,indirect-first-instance,shader-f16,rg11b10ufloat-renderable,bgra8unorm-storage,float32-filterable,float32-blendable,clip-distances,dual-source-blending,subgroups",
};

pub const WINDOWS_DESKTOP: Profile = Profile {
    identity: "chrome_146_PSK",
    major: 146,
    full: "146.0.7680.180",
    system: "Windows NT 10.0; Win64; x64",
    platform: "Windows",
    navigator: "Win32",
    version: "19.0.0",
    architecture: "x86",
    bitness: "64",
    model: "",
    cores: 16,
    memory: 8,
    touch: 0,
    width: 1920,
    height: 1080,
    ratio: 1.0,
    depth: 24,
    top: 0,
    bottom: 48,
    outer_width: 1920,
    outer_height: 1032,
    chrome_height: 122,
    vendor: "Google Inc. (Intel)",
    renderer: "ANGLE (Intel, Intel(R) UHD Graphics 630 (0x00003E9B) Direct3D11 vs_5_0 ps_5_0, D3D11)",
    language: "en-US",
    languages: &["en-US", "en"],
    timezone: "Europe/Berlin",
    offset: -120,
    gamut: "srgb",
    range: "standard",
    devices: "k:ai,ao,vi",
    layout: 49,
    keys: "",
    fonts: "",
    families: "",
    voices: "",
    canvas: "",
    globals: 0,
    latency: 0.01,
    rate: 48000,
    buffer: 2147483648,
    binding: 2147483644,
    features: "depth-clip-control,depth32float-stencil8,texture-compression-bc,texture-compression-bc-sliced-3d,indirect-first-instance,shader-f16,rg11b10ufloat-renderable,bgra8unorm-storage,float32-filterable,float32-blendable,dual-source-blending,subgroups",
};

pub const MACBOOK_PRO_M3_112: Profile = Profile {
    identity: "chrome_112",
    major: 112,
    full: "112.0.5615.137",
    ..MACBOOK_PRO_M3
};

pub const WINDOWS_DESKTOP_112: Profile = Profile {
    identity: "chrome_112",
    major: 112,
    full: "112.0.5615.137",
    ..WINDOWS_DESKTOP
};

pub const MACBOOK_PRO_M3_141: Profile = Profile {
    identity: "chrome_133",
    major: 141,
    full: "141.0.7390.37",
    ..MACBOOK_PRO_M3
};

pub fn load(number: u32) -> &'static Profile {
    let base = pick(number);
    let Ok(path) = std::env::var("DD_MACHINE") else { return base };
    let Ok(body) = std::fs::read_to_string(path) else { return base };
    let mut made = Profile { ..*base };
    for line in body.lines() {
        let Some((name, value)) = line.split_once('=') else { continue };
        let value: &'static str = Box::leak(value.trim().to_string().into_boxed_str());
        match name.trim() {
            "fonts" => made.fonts = value,
            "families" => made.families = value,
            "voices" => made.voices = value,
            "canvas" => made.canvas = value,
            "keys" => made.keys = value,
            "devices" => made.devices = value,
            "renderer" => made.renderer = value,
            "vendor" => made.vendor = value,
            "timezone" => made.timezone = value,
            "language" => made.language = value,
            "globals" => made.globals = value.parse().unwrap_or(0),
            "layout" => made.layout = value.parse().unwrap_or(made.layout),
            "cores" => made.cores = value.parse().unwrap_or(made.cores),
            "memory" => made.memory = value.parse().unwrap_or(made.memory),
            "offset" => made.offset = value.parse().unwrap_or(made.offset),
            _ => {}
        }
    }
    Box::leak(Box::new(made))
}

pub fn pick(number: u32) -> &'static Profile {
    match number {
        2 => &WINDOWS_DESKTOP,
        3 => &MACBOOK_PRO_M3_112,
        4 => &WINDOWS_DESKTOP_112,
        5 => &MACBOOK_PRO_M3_141,
        _ => &MACBOOK_PRO_M3,
    }
}

impl Profile {
    pub fn agent(&self) -> String {
        format!(
            "Mozilla/5.0 ({}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/{}.0.0.0 Safari/537.36",
            self.system, self.major
        )
    }

    pub fn brands(&self) -> String {
        format!(
            "\"Not;A=Brand\";v=\"99\", \"Google Chrome\";v=\"{}\", \"Chromium\";v=\"{}\"",
            self.major, self.major
        )
    }

    pub fn accept_language(&self) -> String {
        let mut out = String::new();
        for (at, tag) in self.languages.iter().enumerate() {
            if at > 0 {
                out.push(',');
            }
            out.push_str(tag);
            if at > 0 {
                out.push_str(&format!(";q=0.{}", 10 - at.min(9)));
            }
        }
        out
    }

    pub fn language_list(&self) -> String {
        let quoted: Vec<String> = self.languages.iter().map(|tag| format!("\"{tag}\"")).collect();
        format!("[{}]", quoted.join(","))
    }

    pub fn avail_width(&self) -> u32 {
        self.width
    }

    pub fn avail_height(&self) -> u32 {
        self.height.saturating_sub(self.top + self.bottom)
    }

    pub fn inner_width(&self) -> u32 {
        self.outer_width
    }

    pub fn inner_height(&self) -> u32 {
        self.outer_height.saturating_sub(self.chrome_height)
    }
}

impl Profile {
    pub fn host(&self, origin: &str, page: &str, document: String, now: f64) -> vm::host::Host {
        vm::host::Host {
            agent: self.agent(),
            platform: self.navigator.to_string(),
            language: self.language.to_string(),
            languages: self.languages.iter().map(|tag| (*tag).to_string()).collect(),
            timezone: self.timezone.to_string(),
            offset: f64::from(self.offset),
            cores: f64::from(self.cores),
            memory: f64::from(self.memory),
            touch: f64::from(self.touch),
            width: f64::from(self.width),
            height: f64::from(self.height),
            avail_width: f64::from(self.avail_width()),
            avail_height: f64::from(self.avail_height()),
            inner_width: f64::from(self.inner_width()),
            inner_height: f64::from(self.inner_height()),
            outer_width: f64::from(self.outer_width),
            outer_height: f64::from(self.outer_height),
            ratio: self.ratio,
            depth: f64::from(self.depth),
            vendor: self.vendor.to_string(),
            renderer: self.renderer.to_string(),
            origin: origin.to_string(),
            page: page.to_string(),
            now,
            elapsed: 812.5,
            seed: now as u64 ^ 0x9e3779b97f4a7c15,
            document,
        }
    }
}

impl Profile {
    pub fn missing(&self) -> Vec<&'static str> {
        let mut out = Vec::new();
        if self.fonts.is_empty() {
            out.push("fonts");
        }
        if self.families.is_empty() {
            out.push("families");
        }
        if self.voices.is_empty() {
            out.push("voices");
        }
        if self.keys.is_empty() {
            out.push("keys");
        }
        if self.canvas.is_empty() {
            out.push("canvas");
        }
        if self.globals == 0 {
            out.push("globals");
        }
        out
    }
}
