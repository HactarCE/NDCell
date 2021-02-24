#![allow(unused_macros)]

macro_rules! load_shader_static {
    { srgb: $srgb:expr, vert: $vert_name:expr, frag: $frag_name:expr, } => {
        compile_shader(
            $srgb,
            $vert_name,
            $frag_name,
            include_str!(concat!($vert_name, ".vert")),
            include_str!(concat!($frag_name, ".frag")),
        )
        .unwrap()
    };
}

macro_rules! load_shader_dynamic {
    { srgb: $srgb:expr, vert: $vert_name:expr, frag: $frag_name:expr, } => {
        DynamicWrappedShader {
            vert_name: $vert_name,
            frag_name: $frag_name,
            vert_filename: concat!("ui/src/gridview/render/shaders/", $vert_name, ".vert"),
            frag_filename: concat!("ui/src/gridview/render/shaders/", $frag_name, ".frag"),
            srgb: $srgb,
            program: parking_lot::Mutex::new(Some(load_shader_static! {
                srgb: $srgb,
                vert: $vert_name,
                frag: $frag_name,
            })),
        };
    };
}

#[cfg(debug_assertions)]
macro_rules! load_shader {
    ($($tok:tt)*) => {
        load_shader_dynamic!($($tok)*);
    };
}

#[cfg(not(debug_assertions))]
macro_rules! load_shader {
    ($($tok:tt)*) => {
        StaticWrappedShader(load_shader_static!($($tok)*));
    };
}

/// Uses `lazy_static!()` to define a shader that is lazily compiled from files.
macro_rules! shader {
    ($name:ident = { srgb: $srgb:expr, vert: $vert_name:expr, frag: $frag_name:expr }) => {
        lazy_static! {
            pub static ref $name: SendWrapper<WrappedShader> = SendWrapper::new(load_shader! {
                srgb: $srgb,
                vert: $vert_name,
                frag: $frag_name,
            });
        }
    };
    ($name:ident = { srgb: $srgb:expr, $filename:expr }) => {
        shader!($name = { srgb: $srgb, vert: $filename, frag: $filename });
    };
}
