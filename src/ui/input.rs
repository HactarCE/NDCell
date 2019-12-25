use glium::glutin::*;

use super::gridview;

pub fn handle_event(state: &mut super::State, ev: &Event) {
    match ev {
        // Handle WindowEvents.
        Event::WindowEvent { event, .. } => match &mut state.grid_view {
            // Handle input on 2D grid view.
            gridview::GridView::View2D(view2d) => match event {
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
                                    // Zoom out by a factor of 2.
                                    view2d.viewport.zoom_by(2.0f32.powf(-1f32));
                                }
                                // Handle zooming in.
                                Some(VirtualKeyCode::Equals)
                                | Some(VirtualKeyCode::Q)
                                | Some(VirtualKeyCode::PageUp) => {
                                    // Zoom in by a factor of 2.
                                    view2d.viewport.zoom_by(2.0f32.powf(1f32));
                                }
                                // Handle WASD or arrow keys.
                                Some(VirtualKeyCode::W) | Some(VirtualKeyCode::Up) => {
                                    // Scroll 64px up.
                                    view2d.viewport.scroll_pixels(0.0, 64.0);
                                }
                                Some(VirtualKeyCode::A) | Some(VirtualKeyCode::Left) => {
                                    // Scroll 64px left.
                                    view2d.viewport.scroll_pixels(-64.0, 0.0);
                                }
                                Some(VirtualKeyCode::S) | Some(VirtualKeyCode::Down) => {
                                    // Scroll 64px down.
                                    view2d.viewport.scroll_pixels(0.0, -64.0);
                                }
                                Some(VirtualKeyCode::D) | Some(VirtualKeyCode::Right) => {
                                    // Scroll 64px right.
                                    view2d.viewport.scroll_pixels(64.0, 0.0);
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
                        // Scroll 64x
                        view2d.viewport.scroll_pixels(*dx * 64.0, *dy * 64.0);
                    }
                    MouseScrollDelta::PixelDelta(dpi::LogicalPosition { x: dx, y: dy }) => {
                        // Scroll 64x
                        view2d
                            .viewport
                            .scroll_pixels(*dx as f32 * 64.0, *dy as f32 * 64.0);
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
