use glium::glutin::*;

use super::render;
use crate::automaton::NdSimulate;

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
                        modifiers,
                        ..
                    } => {
                        if modifiers.ctrl {
                            match virtual_keycode {
                                // Handle undo and redo.
                                Some(VirtualKeyCode::Z) => {
                                    if modifiers.shift {
                                        grid.automaton.redo();
                                    } else {
                                        grid.automaton.undo();
                                    }
                                }
                                // Handle reset.
                                Some(VirtualKeyCode::R) => {
                                    grid.automaton.reset();
                                }
                                _ => (),
                            }
                        } else {
                            match virtual_keycode {
                                // Handle spacebar press.
                                Some(VirtualKeyCode::Space) => {
                                    grid.automaton.step_single();
                                    state.grid_view.push_to_history();
                                }
                                // Handle tab key press.
                                Some(VirtualKeyCode::Tab) => {
                                    grid.automaton.step();
                                    state.grid_view.push_to_history();
                                }
                                // Handle zooming out.
                                Some(VirtualKeyCode::Subtract)
                                | Some(VirtualKeyCode::Z)
                                | Some(VirtualKeyCode::PageDown) => grid.zoom(-1.0),
                                // Handle zooming in.
                                Some(VirtualKeyCode::Equals)
                                | Some(VirtualKeyCode::Q)
                                | Some(VirtualKeyCode::PageUp) => grid.zoom(1.0),
                                // Handle WASD or arrow keys.
                                Some(VirtualKeyCode::W) | Some(VirtualKeyCode::Up) => {
                                    grid.scroll(0.0, 64.0)
                                }
                                Some(VirtualKeyCode::A) | Some(VirtualKeyCode::Left) => {
                                    grid.scroll(-64.0, 0.0)
                                }
                                Some(VirtualKeyCode::S) | Some(VirtualKeyCode::Down) => {
                                    grid.scroll(0.0, -64.0)
                                }
                                Some(VirtualKeyCode::D) | Some(VirtualKeyCode::Right) => {
                                    grid.scroll(64.0, 0.0)
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
            render::GridView::Grid3D(_grid) => match event {
                _ => (),
            },
        },
        // Ignore non-WindowEvents.
        _ => (),
    }
}
