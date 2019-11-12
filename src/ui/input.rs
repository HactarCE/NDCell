use glium::glutin::*;

use super::render;

pub fn handle_event(state: &mut super::State, ev: &Event) {
    match ev {
        // Handle WindowEvents.
        Event::WindowEvent { event, .. } => match &mut state.grid_view {
            // Handle input on 2D grid view.
            render::GridView::Grid2D(grid) => match event {
                WindowEvent::KeyboardInput { input, .. } => match input {
                    // Handle key press.
                    KeyboardInput {
                        state: ElementState::Pressed,
                        virtual_keycode,
                        ..
                    } => match virtual_keycode {
                        // Handle spacebar press.
                        Some(VirtualKeyCode::Space) => grid.step(),
                        // Handle minus key press.
                        Some(VirtualKeyCode::Subtract) => grid.zoom(-1.0),
                        // Handle plus key press.
                        Some(VirtualKeyCode::Equals) => grid.zoom(1.0),
                        _ => (),
                    },

                    // Handle key release.
                    _ => (),
                },
                WindowEvent::MouseWheel { delta, .. } => match delta {
                    MouseScrollDelta::LineDelta(dx, dy) => {
                        // grid.zoom(*dy / 2.0);
                        // println!("line delta");
                        grid.scroll(*dx * 50.0, *dy * 50.0);
                    }
                    MouseScrollDelta::PixelDelta(dpi::LogicalPosition { x: dx, y: dy }) => {
                        // grid.zoom(*dy as f32 / 2.0);
                        // println!("pixel delta");
                        grid.scroll(*dx as f32 * 50.0, *dy as f32 * 50.0);
                    }
                },
                _ => (),
            },

            // Handle input on 3D grid view.
            render::GridView::Grid3D(grid) => match event {
                _ => (),
            },
        },
        // Ignore non-WindowEvents.
        _ => (),
    }
}
