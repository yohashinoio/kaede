pub fn kaede_dir() -> String {
    std::env::var("KAEDE_DIR").unwrap_or(concat!(env!("HOME"), "/.kaede").to_string())
}
