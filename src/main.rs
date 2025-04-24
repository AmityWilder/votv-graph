use std::collections::VecDeque;
use std::num::NonZeroU128;
use std::time::{Duration, Instant};
use console::{console_write, pop_word, Cmd, Console, ConsoleLineCategory, ConsoleLineRef, EnrichEx, Tempo};
use graph::WeightedGraph;
use raylib::prelude::*;
use route::{VertexClass, Visit};

mod serialization;
mod console;
mod graph;
mod route;

pub trait MeasureTextEx {
    #[inline]
    #[must_use]
    fn measure_text_ex(&self, font: impl AsRef<ffi::Font>, text: &str, font_size: f32, spacing: f32) -> Vector2 {
        let c_text = std::ffi::CString::new(text).unwrap();
        unsafe { ffi::MeasureTextEx(*font.as_ref(), c_text.as_ptr(), font_size, spacing) }.into()
    }
}
impl MeasureTextEx for RaylibHandle {}

const VERTEX_RADIUS: f32 = 8.0;
const CAMERA_POSITION_DEFAULT: Vector3 = Vector3::new(0.0, 1300.0, 0.0);

fn main() {
    let mut is_giving_command = true;
    let mut command_history = VecDeque::new();
    let mut command_history_offset = 0;
    let mut console: Console = Console::new();
    let mut is_debugging = false;

    let mut graph = WeightedGraph::load_from_memory(include_str!("resources/votv.graph"))
        .unwrap_or_else(|e| {
            console_write!(console, Error, "error loading hard-coded map: {e}");
            WeightedGraph::new(Vec::new(), Vec::new())
        });

    let mut route = None;

    let mut camera = Camera3D::perspective(
        CAMERA_POSITION_DEFAULT,
        Vector3::zero(),
        Vector3::new(0.0, 0.0, -1.0),
        45.0,
    );

    let mut tempo = Tempo::new();

    let mut is_cursor_shown = false;
    let mut cursor_last_toggled = Instant::now();

    let mut backspace_pressed = None;

    let mut interactive_targets = Vec::new();
    let mut is_giving_interactive_targets = false;

    let mut mouse_tracking;

    let (mut rl, thread) = init()
        .title("VotV Route Tool")
        .resizable()
        .msaa_4x()
        .build();

    rl.hide_cursor();
    rl.maximize_window();
    rl.set_target_fps(120);
    rl.set_exit_key(None);

    let font = rl.load_font_from_memory(&thread, ".ttf", include_bytes!("resources/ShareTechMono-Regular.ttf"), 16, None).unwrap();

    mouse_tracking = rl.get_mouse_position();

    'window: while !rl.window_should_close() {
        let hovered_vert = graph.verts_iter()
            .map(|(v, vert)| (v, get_ray_collision_sphere(rl.get_screen_to_world_ray(rl.get_mouse_position(), camera), vert.pos, VERTEX_RADIUS)))
            .filter(|(_, c)| c.hit)
            .min_by(|(_, c1), (_, c2)| c1.distance.partial_cmp(&c2.distance).expect("vertices should not have `NaN` distance"))
            .map(|(v, _)| v);

        let mouse_snapped =
            if let &Some(v) = &hovered_vert {
                rl.get_world_to_screen(graph.vert(v).pos, camera)
            } else {
                rl.get_mouse_position()
            };

        mouse_tracking = mouse_tracking.lerp(mouse_snapped, 1.0);

        if rl.is_key_pressed(KeyboardKey::KEY_ESCAPE) {
            is_giving_command = false;
            // console.command.clear();
        }
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            if !is_giving_command {
                // begin giving command
                is_giving_command = true;
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now();
                // console.command.clear();
            } else {
                // finish giving command
                if !console.command.is_empty() {
                    command_history.push_front(std::mem::take(&mut console.command));
                    command_history_offset = command_history.len();
                    let mut args = command_history.front().unwrap().split(' ');

                    console.reply.clear();
                    if let Some(cmd) = args.next() {
                        match Cmd::try_from_str(cmd) {
                            Ok(cmd) => {
                                let result = match cmd {
                                    Cmd::Help => Ok(Cmd::run_help(&mut console)),
                                    Cmd::SvRoute => match Cmd::run_sv_route(&graph, &mut route, std::mem::take(&mut interactive_targets), &mut console, args) {
                                        Ok(is_ready) => {
                                            is_giving_interactive_targets = !is_ready;
                                            if is_giving_interactive_targets {
                                                is_giving_command = false;
                                            }
                                            Ok(())
                                        },
                                        Err(e) => {
                                            is_giving_interactive_targets = false;
                                            Err(e)
                                        }
                                    }
                                    Cmd::SvRouteAdd => match Cmd::run_sv_route_add(&graph, &mut route, std::mem::take(&mut interactive_targets), &mut console, args) {
                                        Ok(is_ready) => {
                                            is_giving_interactive_targets = !is_ready;
                                            if is_giving_interactive_targets {
                                                is_giving_command = false;
                                            }
                                            Ok(())
                                        }
                                        Err(e) => {
                                            is_giving_interactive_targets = false;
                                            Err(e)
                                        }
                                    }
                                    Cmd::SvRouteClear => {
                                        route = None;
                                        Ok(console_write!(console, Info, "route cleared"))
                                    },
                                    Cmd::SvNew => Cmd::run_sv_new(&mut graph, &mut route, camera, &mut console, args),
                                    Cmd::SvEdge => Cmd::run_sv_edge(&mut graph, &mut route, &mut console, args),
                                    Cmd::SvLoad => Cmd::run_sv_load(&mut graph, &mut route, &mut console, args),
                                    Cmd::SvSave => Cmd::run_sv_save(&mut graph, &mut console, args),
                                    Cmd::Tempo => Cmd::run_tempo(&mut console, args, &mut tempo),
                                    Cmd::Dbg => {
                                        is_debugging = !is_debugging;
                                        Ok(console_write!(console, Info, "debugging is now {}", if is_debugging { "on" } else { "off" }))
                                    }
                                    Cmd::Focus => Cmd::run_focus(&graph, &mut camera, &mut console, args),
                                    Cmd::Close => break 'window,
                                };
                                if let Err(e) = result {
                                    use std::error::Error;
                                    use std::fmt::Write;
                                    let mut indent = 0;
                                    let mut e: &dyn Error = &e;
                                    let mut msg = e.to_string();
                                    while let Some(src) = e.source() {
                                        indent += 1;
                                        write!(msg, "\n{:1$}{src}", "", 2*indent).unwrap();
                                        e = src;
                                    }
                                    console_write!(console, Error, "{msg}");
                                }
                            }

                            Err(cmd) => {
                                console_write!(console, Error, "no such command: `{cmd}`");
                            }
                        }
                    }
                }
            }
        }

        if rl.is_key_released(KeyboardKey::KEY_BACKSPACE) {
            backspace_pressed = None;
        }

        if is_giving_command {
            let mut force_blink = true;
            if let Some(ch) = rl.get_char_pressed() {
                console.command.push(ch);
            } else if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
                backspace_pressed = Some(Instant::now());
                if rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL) {
                    pop_word(&mut console.command);
                } else {
                    console.command.pop();
                }
            } else if let Some(pressed_time) = &mut backspace_pressed {
                const DELAY: Duration = Duration::from_millis(550);
                const REP: Duration = Duration::from_millis(33);
                if pressed_time.elapsed() >= DELAY {
                    *pressed_time = Instant::now() - DELAY + REP;
                    if rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL) {
                        pop_word(&mut console.command);
                    } else {
                        console.command.pop();
                    }
                }
            } else if rl.is_key_pressed(KeyboardKey::KEY_UP) {
                command_history_offset = if command_history_offset + 1 < command_history.len() + 1 { command_history_offset + 1 } else { 0 };
                console.command = command_history.get(command_history_offset).cloned().unwrap_or_default();
            } else if rl.is_key_pressed(KeyboardKey::KEY_DOWN) {
                command_history_offset = command_history_offset.checked_sub(1).unwrap_or(command_history.len() + 1 - 1);
                console.command = command_history.get(command_history_offset).cloned().unwrap_or_default();
            } else if rl.is_key_pressed(KeyboardKey::KEY_V) && (rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL)) {
                if let Ok(clipboard) = rl.get_clipboard_text() {
                    console.command.push_str(&clipboard);
                }
            } else {
                force_blink = false;
            }

            const BLINK_TIME: Duration = Duration::from_millis(500);
            if force_blink {
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now() - BLINK_TIME / 2;
            } else if cursor_last_toggled.elapsed() >= BLINK_TIME {
                is_cursor_shown = !is_cursor_shown;
                cursor_last_toggled = Instant::now();
            }
        } else {
            let speed = 4.0*camera.position.distance_to(camera.target)/CAMERA_POSITION_DEFAULT.y;
            let north = (rl.is_key_down(KeyboardKey::KEY_W) as i8 - rl.is_key_down(KeyboardKey::KEY_S) as i8) as f32*speed;
            let east  = (rl.is_key_down(KeyboardKey::KEY_D) as i8 - rl.is_key_down(KeyboardKey::KEY_A) as i8) as f32*speed;
            let up    = (rl.is_key_down(KeyboardKey::KEY_Q) as i8 - rl.is_key_down(KeyboardKey::KEY_E) as i8) as f32*speed;
            let pitch = (rl.is_key_down(KeyboardKey::KEY_R) as i8 - rl.is_key_down(KeyboardKey::KEY_F) as i8) as f32*speed;
            let yaw   = (rl.is_key_down(KeyboardKey::KEY_X) as i8 - rl.is_key_down(KeyboardKey::KEY_Z) as i8) as f32*speed;
            let pan = Vector3::new(east, 0.0, -north);
            let zoom = Vector3::new(yaw, up, -pitch);
            camera.position += pan + zoom;
            camera.target += pan;
        }

        if is_giving_interactive_targets && rl.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {
            if let Some(v) = hovered_vert {
                if let Some(p) = interactive_targets.iter().position(|x| x == &v) {
                    console_write!(console, Info, "removing vertex {} from route", graph.vert(v).id);
                    interactive_targets.remove(p);
                } else {
                    console_write!(console, Info, "adding vertex {} to route", graph.vert(v).id);
                    interactive_targets.push(v);
                }
            }
        }

        if let Some(route) = &mut route {
            match &tempo {
                Tempo::Sync => {
                    if !route.is_finished() {
                        route.step(&mut console, &graph);
                    }
                }

                Tempo::Sprint => {
                    let target_duration = Duration::from_secs_f32(0.5/rl.get_fps() as f32);
                    let start = Instant::now();
                    while !route.is_finished() && start.elapsed() < target_duration {
                        route.step(&mut console, &graph);
                    }
                }

                Tempo::Instant => {
                    while !route.is_finished() {
                        route.step(&mut console, &graph);
                    }
                }

                Tempo::Paused => {}

                Tempo::Exact { ticks, ms } => {
                    let ms_elapsed = route.last_step_elapsed().as_millis();
                    let ticks = ticks.get() as u128*ms_elapsed/(NonZeroU128::from(*ms));
                    for _ in 0..ticks {
                        if route.is_finished() { break; }
                        route.step(&mut console, &graph);
                    }
                }
            }
        }

        let route = &route;

        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::new(8, 0, 0, 255));

        {
            const SCALE_FACTOR: f32 = 1.0/10.0;
            let mut d = d.begin_mode3D({
                let mut camera = camera;
                camera.target *= SCALE_FACTOR;
                camera.position *= SCALE_FACTOR;
                camera
            });

            for edge in graph.edges() {
                let [i, j] = edge.adj;
                let p0 = graph.vert(i).pos;
                let p1 = graph.vert(j).pos;
                d.draw_capsule(p0*SCALE_FACTOR, p1*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::RED.alpha(0.25));
            }

            for (v, vert) in graph.verts_iter() {
                let distance_from_target = vert.pos - camera.target;
                let mut color = route.as_ref().and_then(|route| route.classify(&graph, v)).map_or(
                    Color::RED,
                    |c| match c {
                        VertexClass::Current => Color::SKYBLUE,
                        VertexClass::Adjacent => Color::ORANGE,
                        VertexClass::Root => Color::BLUE,
                        VertexClass::Result => Color::BLUEVIOLET,
                        VertexClass::Target => Color::GREEN,
                    }
                );

                if is_giving_interactive_targets {
                    let is_hovered = get_ray_collision_sphere(d.get_screen_to_world_ray(d.get_mouse_position(), camera), vert.pos, VERTEX_RADIUS).hit;
                    if is_hovered {
                        color = color.brightness(0.45);
                    }
                    let is_targeted = interactive_targets.contains(&v);
                    if is_targeted {
                        color = color.brightness(0.35);
                    }
                } else {
                    let is_focused = distance_from_target.dot(distance_from_target) <= VERTEX_RADIUS*VERTEX_RADIUS;
                    if is_focused {
                        color = color.brightness(0.45);
                    }
                }

                let resolution = lerp(24.0, 8.0, (camera.position.distance_to(vert.pos)/1000.0).clamp(0.0, 1.0)).round() as i32; // LOD
                d.draw_sphere_ex(vert.pos*SCALE_FACTOR, VERTEX_RADIUS*SCALE_FACTOR, resolution, resolution, color);
                if let Some(route) = &route {
                    if let Some(Visit { parent: Some(p), .. }) = route.get_visit(v) {
                        d.draw_capsule(graph.vert(p).pos*SCALE_FACTOR, vert.pos*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::ORANGERED);
                    }
                }
            }

            if let Some(route) = &route {
                for pair in route.result().windows(2) {
                    let [a, b] = pair else { panic!("window(2) should always create 2 elements") };
                    d.draw_capsule(graph.vert(*a).pos*SCALE_FACTOR, graph.vert(*b).pos*SCALE_FACTOR, 2.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
                }
            }

            d.draw_capsule((camera.target + Vector3::new(-5.0, 0.0,  0.0))*SCALE_FACTOR, (camera.target + Vector3::new(5.0, 0.0, 0.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
            d.draw_capsule((camera.target + Vector3::new( 0.0, 0.0, -5.0))*SCALE_FACTOR, (camera.target + Vector3::new(0.0, 0.0, 5.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);

            // d.draw_grid(10, 100.0*SCALE_FACTOR);
        }

        d.draw_rectangle_rec(Rectangle::new(mouse_tracking.x - 1.5, 0.0, 3.0, d.get_render_height() as f32), Color::new(255, 255, 255, 32));
        d.draw_rectangle_rec(Rectangle::new(0.0, mouse_tracking.y - 1.5, d.get_render_width() as f32, 3.0), Color::new(255, 255, 255, 32));

        for (v, vert) in graph.verts_iter() {
            let pos = d.get_world_to_screen(vert.pos, camera);
            let text = vert.alias.as_str();
            let text_size = d.measure_text_ex(&font, text, font.baseSize as f32, 0.0);
            d.draw_text_ex(&font, text, pos - rvec2(text_size.x*0.5, font.baseSize/2), font.baseSize as f32, 0.0, Color::WHITE);
            if let Some(route) = &route {
                if let Some(Visit { distance, parent }) = route.get_visit(v) {
                    let parent_text = parent.map_or("-", |p| &graph.vert(p).alias);
                    let text = format!("{} ({parent_text})", distance.ceil());
                    d.draw_text_ex(&font, &text, pos + rvec2(text_size.x*0.5 + 3.0, 3), font.baseSize as f32, 0.0, Color::GRAY);
                }
            }
        }

        // Console

        if !is_giving_command {
            d.draw_text_ex(
                &font,
                "use W/A/S/D to pan, Q/E to zoom, and R/F/Z/X to orbit",
                rvec2(0, d.get_render_height() - font.baseSize*3),
                font.baseSize as f32,
                0.0,
                Color::GREENYELLOW,
            );
        }

        d.draw_text_ex(
            &font,
            if is_giving_command {
                "press ENTER to run the command, or ESCAPE to cancel"
            } else {
                "press ENTER to begin typing a command"
            },
            rvec2(0, d.get_render_height() - font.baseSize*2),
            font.baseSize as f32,
            0.0,
            Color::GREENYELLOW,
        );

        d.draw_text_ex(
            &font,
            &format!("x:{}/y:{}", camera.target.x, camera.target.z),
            rvec2(0, d.get_render_height() - font.baseSize),
            font.baseSize as f32,
            0.0,
            Color::GREENYELLOW,
        );

        {
            let mut line_idx = 0;

            let console_iter =
                command_history.iter()
                    .take(4)
                    .rev()
                    .map(|item| ConsoleLineRef::ghost(item))
                .chain(console.reply.iter().map(|item| item.as_line_ref()))
                .chain(std::iter::once_with(|| ConsoleLineRef::command(&console.command)))
                .chain(is_debugging.then(|| console.debug.iter().map(|item| item.as_line_ref())).into_iter().flatten());

            for item in console_iter {
                use ConsoleLineCategory::*;
                let (color, prefix, suffix) = match item.cat {
                    Route => (Color::RAYWHITE,  "route: ", ""),
                    Ghost => (Color::LIGHTBLUE.alpha(0.5), ">", ""),
                    Command => (Color::LIGHTBLUE, ">", if is_giving_command && is_cursor_shown { "_" } else { "" }),
                    TargetList => (Color::LIME, "targets: ", ""),
                    Trace => (Color::DARKGRAY, "trace: ", ""),
                    Debug if is_debugging => (Color::MAGENTA, "debug: ", ""),
                    Debug => continue,
                    Info => (Color::LIGHTGRAY, "", ""),
                    Warning => (Color::GOLD, "warning: ", ""),
                    Error => (Color::RED, "err: ", ""),
                    Fatal => (Color::SALMON, "fatal: ", ""),
                };
                let font_size = font.baseSize as f32;
                let spacing = 0.0;
                let char_width = d.measure_text_ex(&font, "M", font_size, spacing).x;
                for line in format!("{prefix}{}{suffix}", item.msg).lines() {
                    let y = font.baseSize*line_idx;
                    for (x, text, color) in line.enrich(char_width, spacing, color) {
                        d.draw_text_ex(&font, text, rvec2(x, y), font_size, spacing, color);
                    }
                    line_idx += 1;
                }
            }
        }
    }
}
