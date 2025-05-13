#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![feature(
    substr_range,           // used in serialization::Source
    str_split_remainder,
    str_split_whitespace_remainder,
    assert_matches,
    iter_next_chunk,
    never_type,
    type_alias_impl_trait,
    impl_trait_in_assoc_type,
    associated_type_defaults,
    let_chains,
    iter_intersperse,
)]

use std::num::NonZeroU128;
use std::time::{Duration, Instant};
use camera::Orbiter;
use console::input::ConsoleIn;
use console::output::ConsoleOut;
use console::{console_log, enrich::EnrichEx, output::ConsoleLineCategory};
use command::{Cmd, CmdRunner, Command, ProgramData, Routine};
use graph::{VertexID, WeightedGraph};
use raylib::prelude::*;
use route::{RouteGenerator, VertexClass, Visit};
use types::Tempo;

mod camera;
mod types;
mod serialization;
mod console;
mod command;
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
const CAMERA_LENGTH_DEFAULT: f32 = 1300.0;
const CAMERA_FOVY: f32 = 45.0;
const SAFE_ZONE: f32 = 20.0;
const UPSCALE: f32 = 2.0;

fn main() {
    let mut cin = ConsoleIn::new();
    let mut cout = ConsoleOut::new();
    let mut console_buf = String::with_capacity(4 * 1024);

    let mut data = ProgramData {
        is_debugging: false,
        graph: WeightedGraph::new(Vec::new(), Vec::new()),
        route: None,
        orbit: Orbiter::new(Vector3::zero(), CAMERA_LENGTH_DEFAULT, 0.0, 0.0),
        tempo: Tempo::new(),
        interactive_targets: Vec::new(),
        is_giving_interactive_targets: false,
        should_close: false,
        verts_color: Color::new(255, 0, 0, 255),
        edges_color: Color::new(255, 0, 0, 63),
        background_color: Color::new(4, 0, 0, 255),
    };

    // ongoing simultaneous routines
    let mut routines = Vec::new();

    let mut is_cursor_shown = false;
    let mut cursor_last_toggled = Instant::now();

    {
        let default_graph = WeightedGraph::load_from_memory(include_str!("resources/votv.graph"));
        match default_graph {
            Ok(g) => data.graph = g,
            Err(e) => console_log!(cout, Error, "error loading hard-coded map: {e}"),
        }
    }

    let (mut rl, thread) = init()
        .title("VotV Route Tool")
        .resizable()
        .msaa_4x()
        .vsync()
        .build();

    rl.hide_cursor();
    rl.maximize_window();
    rl.set_target_fps(60);
    rl.set_exit_key(None);

    let font = rl.load_font_from_memory(&thread, ".ttf", include_bytes!("resources/ShareTechMono-Regular.ttf"), 16, None).unwrap();
    let mut framebuffer = rl.load_render_texture(
        &thread,
        rl.get_screen_width().try_into().unwrap(),
        rl.get_screen_height().try_into().unwrap(),
    ).unwrap();
    let mut shader = rl.load_shader_from_memory(&thread, None, Some(include_str!("resources/screen.frag")));
    let shader_render_width_loc = shader.get_shader_location("renderWidth");
    let shader_render_height_loc = shader.get_shader_location("renderHeight");

    'window: while !rl.window_should_close() {
        if rl.is_window_resized() {
            let width = u32::try_from(rl.get_screen_width()).unwrap()*UPSCALE as u32;
            let height = u32::try_from(rl.get_screen_height()).unwrap()*UPSCALE as u32;
            framebuffer = rl.load_render_texture(&thread, width, height).unwrap();
            shader.set_shader_value(shader_render_width_loc, width as f32);
            shader.set_shader_value(shader_render_height_loc, height as f32);
        }

        if rl.is_key_pressed(KeyboardKey::KEY_ESCAPE) {
            cin.unfocus();
        }
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            if cin.is_focused() {
                // finish giving command
                if let Some(command) = cin.submit_cmd(&mut cout) {
                    routines.push(Routine::new(command));
                }
            } else {
                // begin giving command
                cin.focus();
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now();
            }
        }

        // tick routines
        {
            routines.retain_mut(|routine| {
                match routine.step(&mut cout, &mut cin, &mut data) {
                    std::ops::ControlFlow::Continue(()) => true,
                    std::ops::ControlFlow::Break(result) => {
                        match result {
                            Ok(x) => CmdRunner::disp(&x, &mut cout, &data),
                            Err(e) => {
                                let mut err = Some(&e as &dyn std::error::Error);
                                while let Some(e) = err {
                                    console_log!(cout, Error, "{e}");
                                    err = e.source();
                                }
                            }
                        }
                        false
                    }
                }
            });

            if data.should_close {
                break 'window;
            }
        }

        let is_input_changed = cin.update_input(&mut rl);

        if cin.is_focused() {
            const BLINK_TIME: Duration = Duration::from_millis(500);
            if is_input_changed {
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now() - BLINK_TIME / 2;
            } else if cursor_last_toggled.elapsed() >= BLINK_TIME {
                is_cursor_shown = !is_cursor_shown;
                cursor_last_toggled = Instant::now();
            }
        } else {
            const ROTATE_SPEED: f32 = 1.5; // radians per second
            const PAN_SPEED: f32 = 0.25; // "screen heights" per second

            let dt = rl.get_frame_time();
            let screen_height = data.orbit.length*(CAMERA_FOVY.to_radians()*0.5).tan()*2.0;
            let pan_speed = PAN_SPEED*screen_height*dt;
            let rot_speed = ROTATE_SPEED*dt;

            let north = (rl.is_key_down(KeyboardKey::KEY_W) as i8 - rl.is_key_down(KeyboardKey::KEY_S) as i8) as f32;
            let east  = (rl.is_key_down(KeyboardKey::KEY_D) as i8 - rl.is_key_down(KeyboardKey::KEY_A) as i8) as f32;
            let zoom  = (rl.is_key_down(KeyboardKey::KEY_E) as i8 - rl.is_key_down(KeyboardKey::KEY_Q) as i8) as f32;
            let pitch = (rl.is_key_down(KeyboardKey::KEY_R) as i8 - rl.is_key_down(KeyboardKey::KEY_F) as i8) as f32;
            let yaw   = (rl.is_key_down(KeyboardKey::KEY_X) as i8 - rl.is_key_down(KeyboardKey::KEY_Z) as i8) as f32;

            let pan = Vector3::new(east, -north, 0.0);
            data.orbit.target += pan   * pan_speed;
            data.orbit.length -= zoom  * pan_speed;
            data.orbit.pitch  -= pitch * rot_speed;
            data.orbit.yaw    -= yaw   * rot_speed;
        }

        let camera = Camera3D::perspective(
            data.orbit.position(),
            data.orbit.target,
            Vector3::new(0.0, -1.0, 0.0),
            CAMERA_FOVY,
        );

        let hovered_vert = data.graph.verts_iter()
            .map(|(v, vert)| (v, get_ray_collision_sphere(rl.get_screen_to_world_ray(rl.get_mouse_position(), camera), vert.pos, VERTEX_RADIUS)))
            .filter(|(_, c)| c.hit)
            .min_by(|(_, c1), (_, c2)| c1.distance.partial_cmp(&c2.distance).expect("vertices should not have `NaN` distance"))
            .map(|(v, _)| v);

        let mouse_snapped =
            if let &Some(v) = &hovered_vert {
                rl.get_world_to_screen(data.graph.vert(v).pos, camera)
            } else {
                rl.get_mouse_position()
            };

        if data.is_giving_interactive_targets && rl.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {
            if let Some(v) = hovered_vert {
                if let Some(p) = data.interactive_targets.iter().position(|x| x == &v) {
                    console_log!(cout, Info, "removing vertex {} from route", data.graph.vert(v).id);
                    data.interactive_targets.remove(p);
                } else {
                    console_log!(cout, Info, "adding vertex {} to route", data.graph.vert(v).id);
                    data.interactive_targets.push(v);
                }
            }
        }

        if let Some(route) = &mut data.route {
            match &data.tempo {
                Tempo::Sync => {
                    if !route.is_finished() {
                        route.step(&mut cout, &data.graph);
                    }
                }

                Tempo::Sprint => {
                    let target_duration = Duration::from_secs_f32(0.5/rl.get_fps() as f32);
                    let start = Instant::now();
                    while !route.is_finished() && start.elapsed() < target_duration {
                        route.step(&mut cout, &data.graph);
                    }
                }

                Tempo::Instant => {
                    while !route.is_finished() {
                        route.step(&mut cout, &data.graph);
                    }
                }

                Tempo::Pause => {}

                Tempo::Exact { ticks, ms } => {
                    let ms_elapsed = route.last_step_elapsed().as_millis();
                    let ticks = ticks.get() as u128*ms_elapsed/(NonZeroU128::from(*ms));
                    for _ in 0..ticks {
                        if route.is_finished() { break; }
                        route.step(&mut cout, &data.graph);
                    }
                }
            }
        }

        let route = &data.route;

        {
            let mut d = rl.begin_texture_mode(&thread, &mut framebuffer);
            d.clear_background(data.background_color);

            {
                const SCALE_FACTOR: f32 = 1.0/(10.0*UPSCALE);
                let mut d = d.begin_mode3D({
                    let mut camera = camera;
                    camera.target *= SCALE_FACTOR;
                    camera.position *= SCALE_FACTOR;
                    camera
                });

                for edge in data.graph.edges() {
                    let [i, j] = edge.adj;
                    let p0 = data.graph.vert(i).pos;
                    let p1 = data.graph.vert(j).pos;
                    d.draw_capsule(p0*SCALE_FACTOR, p1*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, data.edges_color);
                }

                for (v, vert) in data.graph.verts_iter() {
                    let distance_from_target = vert.pos - data.orbit.target;
                    let mut color = route.as_ref().and_then(|route| route.classify(&data.graph, v)).map_or(
                        data.verts_color,
                        |c| match c {
                            VertexClass::Current => Color::SKYBLUE,
                            VertexClass::Adjacent => Color::ORANGE,
                            VertexClass::Root => Color::BLUE,
                            VertexClass::Result => Color::BLUEVIOLET,
                            VertexClass::Target => Color::GREEN,
                        }
                    );

                    if data.is_giving_interactive_targets {
                        let is_hovered = get_ray_collision_sphere(d.get_screen_to_world_ray(d.get_mouse_position(), camera), vert.pos, VERTEX_RADIUS).hit;
                        if is_hovered {
                            color = color.brightness(0.45);
                        }
                        let is_targeted = data.interactive_targets.contains(&v);
                        if is_targeted {
                            color = color.brightness(0.35);
                        }
                    } else {
                        let is_focused = distance_from_target.dot(distance_from_target) <= VERTEX_RADIUS*VERTEX_RADIUS;
                        if is_focused {
                            color = color.brightness(0.45);
                        }
                    }

                    // let resolution = lerp(24.0, 8.0, (camera.position.distance_to(vert.pos)/1000.0).clamp(0.0, 1.0)).round() as i32; // LOD
                    d.draw_sphere_ex(vert.pos*SCALE_FACTOR, VERTEX_RADIUS*SCALE_FACTOR, 10, 10, color);
                    if let Some(route) = &route {
                        if let Some(Visit { parent: Some(p), .. }) = route.get_visit(v) {
                            d.draw_capsule(data.graph.vert(p).pos*SCALE_FACTOR, vert.pos*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::ORANGERED);
                        }
                    }
                }

                if let Some(route) = &route {
                    for pair in route.result().windows(2) {
                        let [a, b] = pair else { panic!("window(2) should always create 2 elements") };
                        d.draw_capsule(data.graph.vert(*a).pos*SCALE_FACTOR, data.graph.vert(*b).pos*SCALE_FACTOR, 2.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
                    }
                }

                d.draw_capsule((data.orbit.target + Vector3::new(-5.0, 0.0,  0.0))*SCALE_FACTOR, (data.orbit.target + Vector3::new(5.0, 0.0, 0.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
                d.draw_capsule((data.orbit.target + Vector3::new( 0.0, -5.0, 0.0))*SCALE_FACTOR, (data.orbit.target + Vector3::new(0.0, 5.0, 0.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
            }

            d.draw_rectangle_rec(Rectangle::new((mouse_snapped.x - 1.5)*UPSCALE, 0.0, 3.0*UPSCALE, d.get_screen_height() as f32*UPSCALE), Color::new(255, 255, 255, 32));
            d.draw_rectangle_rec(Rectangle::new(0.0, (mouse_snapped.y - 1.5)*UPSCALE, d.get_screen_width() as f32*UPSCALE, 3.0*UPSCALE), Color::new(255, 255, 255, 32));

            for (v, vert) in data.graph.verts_iter() {
                let pos = d.get_world_to_screen(vert.pos, camera);
                let text = vert.alias.as_str();
                let text_size = d.measure_text_ex(&font, text, font.baseSize as f32, 0.0);
                d.draw_text_ex(&font, text, (pos - rvec2(text_size.x*0.5, font.baseSize/2))*UPSCALE, font.baseSize as f32*UPSCALE, 0.0, Color::WHITE);
                if let Some(route) = &route {
                    if let Some(Visit { distance, parent }) = route.get_visit(v) {
                        let parent_text = parent.map_or("-", |p| &data.graph.vert(p).alias);
                        let text = format!("{} ({parent_text})", distance.ceil());
                        d.draw_text_ex(&font, &text, (pos + rvec2(text_size.x*0.5 + 3.0, 3))*UPSCALE, font.baseSize as f32*UPSCALE, 0.0, Color::GRAY);
                    }
                }
            }
        }

        let mut d = rl.begin_drawing(&thread);
        d.clear_background(data.background_color);

        // Render
        {
            let mut d = d.begin_shader_mode(&mut shader);
            let src = Rectangle::new(0.0, 0.0, framebuffer.width() as f32, -framebuffer.height() as f32);
            let dst = Rectangle::new(0.0, 0.0, d.get_screen_width() as f32, d.get_screen_height() as f32);
            d.draw_texture_pro(&framebuffer, src, dst, Vector2::zero(), 0.0, Color::WHITE);
        }

        // Console
        {
            if !cin.is_focused() {
                d.draw_text_ex(
                    &font,
                    "use W/A/S/D to pan, Q/E to zoom, and R/F/Z/X to orbit",
                    Vector2::new(SAFE_ZONE, (d.get_screen_height() - font.baseSize*3) as f32 - SAFE_ZONE),
                    font.baseSize as f32,
                    0.0,
                    Color::GREENYELLOW,
                );
            }

            d.draw_text_ex(
                &font,
                if cin.is_focused() {
                    "press ENTER to run the command, or ESCAPE to cancel"
                } else {
                    "press ENTER to begin typing a command"
                },
                Vector2::new(SAFE_ZONE, (d.get_screen_height() - font.baseSize*2) as f32 - SAFE_ZONE),
                font.baseSize as f32,
                0.0,
                Color::GREENYELLOW,
            );

            d.draw_text_ex(
                &font,
                &format!("x:{}/y:{}", data.orbit.target.x, data.orbit.target.z),
                Vector2::new(SAFE_ZONE, (d.get_screen_height() - font.baseSize) as f32 - SAFE_ZONE),
                font.baseSize as f32,
                0.0,
                Color::GREENYELLOW,
            );

            {
                let font_size = font.baseSize as f32;
                let spacing = 0.0;
                let char_width = d.measure_text_ex(&font, "M", font_size, spacing).x;
                let max_history_lines = ((d.get_screen_height() as f32 - SAFE_ZONE*2.0)/font.baseSize as f32 - 5.0).floor().max(0.0) as usize;

                let (c_out, c_in) = (&mut cout, &mut cin);

                let display_cursor = c_in.is_focused() && is_cursor_shown;
                let (selection_range, selection_tail) = (c_in.selection_range(), c_in.selection_tail());

                if c_out.is_dirty() || c_in.is_dirty() {
                    let (Color { r, g, b, a }, pre) = ConsoleLineCategory::Command.color_prefix();
                    let cmd_string = format!("<color=rgba({r},{g},{b},{a})>{pre}{}</color>", c_in.current());

                    let history = c_out.log_history()
                        .flat_map(|msg| msg
                            .lines()
                            .enumerate()
                        )
                        .chain(data.is_debugging
                            .then(|| c_out.dbg_history()
                                .flat_map(|msg| msg
                                    .lines()
                                    .enumerate()
                                )
                            )
                            .into_iter()
                            .flatten()
                        );

                    let mut skip = history
                        .clone()
                        .count()
                        .saturating_sub(max_history_lines);

                    skip -= history.clone()
                        .map(|(n, _)| n)
                        .nth(skip)
                        .unwrap_or_default();

                    let history = history
                        .map(|(_, s)| s)
                        .skip(skip);

                    let it = history.chain(std::iter::once(cmd_string.as_str()));

                    console_buf.clear();
                    console_buf.reserve(it.clone().map(|s| s.len() + 1).sum::<usize>());
                    for s in it {
                        console_buf.push_str(s);
                        console_buf.push('\n');
                    }

                    c_in.mark_clean();
                    c_out.mark_clean();
                }

                let (init, sample) = console_buf.split_at(
                    console_buf.match_indices('\n')
                        .map(|(n, _)| n)
                        .nth_back(max_history_lines)
                        .unwrap_or_default()
                );

                let mut point = Vector2::new(SAFE_ZONE, SAFE_ZONE);
                let char_step = Vector2::new(char_width + spacing, font_size);
                d.set_text_line_spacing(0);
                for (text, color) in sample.enrich(Color::WHITE, init) {
                    // let wrap_width = 128;
                    let mut lines_it = text.split('\n');
                    if let Some(line) = lines_it.next() {
                        let mut last_line = line;
                        d.draw_text_ex(&font, line, point, font_size, spacing, color);
                        for line in lines_it {
                            point.x = SAFE_ZONE;
                            point.y += font_size;
                            d.draw_text_ex(&font, line, point, font_size, spacing, color);
                            last_line = line;
                        }

                        let cols = last_line
                            .enrich(Color::WHITE, "")
                            .flat_map(|(s, _)| s.chars())
                            .count();

                        point.x += cols as f32*char_step.x;
                    }
                }

                {
                    const COLOR: Color = ConsoleLineCategory::Command.color_prefix().0;
                    const PREFIX_LEN: usize = ConsoleLineCategory::Command.color_prefix().1.len();
                    let row = sample.lines().count().saturating_sub(1);
                    let selection_y = SAFE_ZONE + row as f32*font_size;
                    let selection_rec = Rectangle::new(
                        SAFE_ZONE + (PREFIX_LEN + selection_range.start) as f32*char_step.x,
                        selection_y,
                        (selection_range.len() as f32*char_step.x - spacing).max(0.0),
                        font_size,
                    );
                    if selection_range.len() > 0 {
                        d.draw_rectangle_rec(selection_rec, Color::LIGHTBLUE.alpha(0.25));
                    } else if !cin.current().is_empty() && selection_tail == cin.current().len() {
                        // let it = Cmd::predict_cmd(cin.current());
                        // if !it.clone().any(|(_, x)| x.is_empty()) {
                        //     for (n, (_, s)) in it.enumerate() {
                        //         d.draw_text_ex(&font, s, Vector2::new(selection_rec.x, selection_rec.y + font_size*n as f32), font_size, spacing, COLOR.alpha(0.5));
                        //     }
                        // }
                    }
                    if display_cursor {
                        let cursor_rec = Rectangle::new(
                            (SAFE_ZONE + (PREFIX_LEN + selection_tail) as f32*char_step.x).floor(),
                            selection_y + font_size - 2.0,
                            char_width + 2.0*spacing,
                            2.0,
                        );
                        d.draw_rectangle_rec(cursor_rec, Color::LIGHTBLUE);
                    }
                }

                d.draw_fps(0, 0);
            }
        }
    }
}
