use crate::config::MouseDisplay;

#[derive(Debug, Clone)]
pub enum MouseClickBinding {}
impl MouseClickBinding {
    pub fn display(&self) -> MouseDisplay {
        match self.clone() {}
    }
}
