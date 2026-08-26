pub mod core;
pub mod passes;
pub mod module_split;
pub mod extract;
pub mod vm;
pub mod vm_db;
pub mod pipeline;

pub mod parse { pub use crate::core::parse_js; }
pub mod hex_cleanup { pub use crate::passes::clean::{hex_decode as run, cleanup_codegen_hex as cleanup_codegen}; }
pub mod bracket { pub use crate::passes::clean::{unwrap_double_brackets, normalize_to_dot}; }
pub mod inline_settimeout { pub use crate::passes::clean::inline_settimeout_zero as run; }
pub mod pure_calls { pub use crate::passes::decoders::pure_calls as run; }
pub mod string_decoders { pub use crate::passes::decoders::string_decoders as run; }
pub mod window_methods { pub use crate::passes::decoders::window_methods as run; }
pub mod tmatrix { pub use crate::passes::decoders::tmatrix as run; }
pub mod opaque { pub use crate::passes::simplify::opaque as run; }
pub mod simplify {
    pub use crate::passes::simplify::{fold_expressions, fold_if_statements};
}
pub mod cff { pub use crate::passes::cff::run; }
