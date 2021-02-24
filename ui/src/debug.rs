// #[cfg(debug_assertions)]
use lazy_static::lazy_static;
// #[cfg(debug_assertions)]
use parking_lot::Mutex;

// #[cfg(debug_assertions)]
lazy_static! {
    pub static ref FRAME_DEBUG_INFO: Mutex<String> = Mutex::new(String::new());
}

#[allow(unused_macros)]
macro_rules! printlnd {
    () => {
        // #[cfg(debug_assertions)]
        crate::debug::FRAME_DEBUG_INFO.lock().push('\n');
    };
    ($($arg:tt)+) => {
        // #[cfg(debug_assertions)]
        crate::debug::FRAME_DEBUG_INFO.lock().push_str(&format!($($arg)+));
        printlnd!();
    };
}
