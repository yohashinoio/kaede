use std::{collections::HashMap, fmt::Display, hash::Hash};

use once_cell::sync::Lazy;

static mut SYMBOLS: Lazy<HashMap<u64, String>> = Lazy::new(Default::default);

// Do not derive PartialEq, Hash, etc!
// We need to compare symbols(string) as well as id
#[derive(Debug, Clone, Copy)]
pub struct Symbol {
    id: u64,
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        unsafe { SYMBOLS[&self.id].as_str() }
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        let id = unsafe {
            let id = SYMBOLS.len() as u64;
            if SYMBOLS.insert(id, value).is_some() {
                unreachable!();
            }
            id
        };

        Self { id }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id || self.as_str() == other.as_str()
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // It is important not to include id in hash!
        unsafe { SYMBOLS[&self.id].hash(state) };
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
