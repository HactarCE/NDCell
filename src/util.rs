macro_rules! __lazy_static_mutex_internal {
    () => {};
}

macro_rules! lazy_static_mutex {
    ($(#[$attr:meta])* static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
        __lazy_static_mutex_internal!($(#[$attr])* () static ref $N : std::sync::Mutex<$T> = std::sync::Mutex::new($e); $($t)*)
        __lazy_static_internal!($(#[$attr])* () static ref $N : $T = $e; $($t)*);
    };
    ($(#[$attr:meta])* pub static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
        __lazy_static_internal!($(#[$attr])* (pub) static ref $N : $T = $e; $($t)*);
    };
    ($(#[$attr:meta])* pub ($($vis:tt)+) static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
        __lazy_static_internal!($(#[$attr])* (pub ($($vis)+)) static ref $N : $T = $e; $($t)*);
    };
    ($(#[$attr:meta])* static ref mut $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
        __lazy_static_internal!($(#[$attr])* () static ref $N : $T = $e; $($t)*);
    };
    ($(#[$attr:meta])* pub static ref mut $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
        __lazy_static_internal!($(#[$attr])* (pub) static ref $N : $T = $e; $($t)*);
    };
    ($(#[$attr:meta])* pub ($($vis:tt)+) static ref mut $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
        __lazy_static_internal!($(#[$attr])* (pub ($($vis)+)) static ref $N : $T = $e; $($t)*);
    };
    () => ();
}
