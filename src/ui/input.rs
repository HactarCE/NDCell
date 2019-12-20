use glium::glutin::*;

use super::gridview;

pub fn handle_event(state: &mut super::State, ev: &Event) {
    match ev {
        // Handle WindowEvents.
        Event::WindowEvent { event, .. } => match &mut state.grid_view {
            // Handle input on 2D grid view.
            gridview::GridView::View2D(grid) => match event {
                WindowEvent::KeyboardInput { input, .. } => match input {
                    // Handle key press.
                    KeyboardInput {
                        state: ElementState::Pressed,
                        virtual_keycode,
                        modifiers,
                        ..
                    } => {
                        if modifiers.ctrl {
                            match virtual_keycode {
                                // Handle undo and redo.
                                Some(VirtualKeyCode::Z) => {
                                    if modifiers.shift {
                                        // TODO redo
                                    } else {
                                        // TODO undo
                                    }
                                }
                                // Handle reset.
                                Some(VirtualKeyCode::R) => {
                                    // TODO reset
                                }
                                _ => (),
                            }
                        } else {
                            match virtual_keycode {
                                // Handle spacebar press.
                                Some(VirtualKeyCode::Space) => {
                                    // TODO single step with history
                                }
                                // Handle tab key press.
                                Some(VirtualKeyCode::Tab) => {
                                    // TODO step with history
                                }
                                // Handle zooming out.
                                Some(VirtualKeyCode::Subtract)
                                | Some(VirtualKeyCode::Z)
                                | Some(VirtualKeyCode::PageDown) => {
                                    // TODO zoom out 2x
                                }
                                // Handle zooming in.
                                Some(VirtualKeyCode::Equals)
                                | Some(VirtualKeyCode::Q)
                                | Some(VirtualKeyCode::PageUp) => {
                                    // TODO zoom in 2x
                                }
                                // Handle WASD or arrow keys.
                                Some(VirtualKeyCode::W) | Some(VirtualKeyCode::Up) => {
                                    // TODO scroll 64px up
                                    // grid.scroll(0.0, 64.0)
                                }
                                Some(VirtualKeyCode::A) | Some(VirtualKeyCode::Left) => {
                                    // TODO scroll 64px left
                                    // grid.scroll(-64.0, 0.0)
                                }
                                Some(VirtualKeyCode::S) | Some(VirtualKeyCode::Down) => {
                                    // TODO scroll 64px down
                                    // grid.scroll(0.0, -64.0)
                                }
                                Some(VirtualKeyCode::D) | Some(VirtualKeyCode::Right) => {
                                    // TODO scroll 64px right
                                    // grid.scroll(64.0, 0.0)
                                }
                                _ => (),
                            }
                        }
                    }

                    // Handle key release.
                    _ => (),
                },
                WindowEvent::MouseWheel { delta, .. } => match delta {
                    MouseScrollDelta::LineDelta(dx, dy) => {
                        // TODO scroll 50x
                        // grid.scroll(*dx * 50.0, *dy * 50.0);
                    }
                    MouseScrollDelta::PixelDelta(dpi::LogicalPosition { x: dx, y: dy }) => {
                        // TODO scroll 50x
                        // grid.scroll(*dx as f32 * 50.0, *dy as f32 * 50.0);
                    }
                },
                _ => (),
            },

            // Handle input on 3D grid view.
            gridview::GridView::View3D(_grid) => match event {
                _ => (),
            },
        },
        // Ignore non-WindowEvents.
        _ => (),
    }
}
