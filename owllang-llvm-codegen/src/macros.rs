/// Utility to create *const i8 strings from &str
#[macro_export]
macro_rules! c_str {
    ($s:expr) => {
        format!("{}\0", $s).as_ptr() as *const i8
    };
}
