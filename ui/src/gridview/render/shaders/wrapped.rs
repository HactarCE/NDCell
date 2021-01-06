use anyhow::Context;
use glium::Program;
use log::error;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};

pub struct StaticWrappedShader(pub Program);
impl StaticWrappedShader {
    pub fn load(&self) -> &Program {
        &self.0
    }
}

pub struct DynamicWrappedShader {
    pub vert_name: &'static str,
    pub frag_name: &'static str,
    pub vert_filename: &'static str,
    pub frag_filename: &'static str,
    pub srgb: bool,
    pub program: Mutex<Option<Program>>,
}
impl DynamicWrappedShader {
    fn _load(&self) -> Option<Program> {
        let vert_contents = std::fs::read_to_string(self.vert_filename)
            .with_context(|| format!("Reading {:?} vertex shader file", self.vert_name))
            .map_err(|e| error!("{}", e))
            .ok()?;
        let frag_contents = std::fs::read_to_string(self.frag_filename)
            .with_context(|| format!("Reading {:?} fragex shader file", self.frag_name))
            .map_err(|e| error!("{}", e))
            .ok()?;
        super::compile_shader(
            self.srgb,
            self.vert_name,
            self.frag_name,
            &vert_contents,
            &frag_contents,
        )
    }

    pub fn try_reload(&self) {
        if let Some(reloaded_program) = self._load() {
            *self.program.lock() = Some(reloaded_program);
        } else {
            error!("Falling back to statically linked shader");
        }
    }
    pub fn load<'a>(&'a self) -> MappedMutexGuard<'a, Program> {
        MutexGuard::map(self.program.lock(), |program| {
            program.get_or_insert_with(|| self._load().unwrap())
        })
    }
}
