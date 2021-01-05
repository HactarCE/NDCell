#![allow(unused_macros)]

macro_rules! load_shader_static {
    { $glsl_version:expr, srgb: $srgb:expr, vert: $vert_name:expr, frag: $frag_name:expr, } => {
        compile_shader(
            $glsl_version,
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
    { $glsl_version:tt, srgb: $srgb:expr, vert: $vert_name:expr, frag: $frag_name:expr, } => {
        DynamicWrappedShader {
            glsl_version: $glsl_version,
            vert_name: $vert_name,
            frag_name: $frag_name,
            vert_filename: concat!("ui/src/gridview/render/shaders/", $vert_name, ".vert"),
            frag_filename: concat!("ui/src/gridview/render/shaders/", $frag_name, ".frag"),
            srgb: $srgb,
            program: parking_lot::Mutex::new(Some(load_shader_static! {
                $glsl_version,
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
    ($name:ident = { $glsl_version:tt, srgb: $srgb:expr, vert: $vert_name:expr, frag: $frag_name:expr }) => {
        lazy_static! {
            pub static ref $name: SendWrapper<WrappedShader> = SendWrapper::new(load_shader! {
                $glsl_version,
                srgb: $srgb,
                vert: $vert_name,
                frag: $frag_name,
            });
        }
    };
    ($name:ident = { $glsl_version:expr, srgb: $srgb:expr, $filename:expr }) => {
        shader!($name = { $glsl_version, srgb: $srgb, vert: $filename, frag: $filename });
    };
}
