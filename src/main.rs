use std::collections::VecDeque;
use std::time::{Duration, Instant};
use console::{console_write, Cmd, Console, ConsoleLineCategory, ConsoleLineRef, EnrichEx};
use graph::{define_edges, define_verts, WeightedGraph};
use raylib::prelude::*;
use route::{Phase, Visit};

pub mod console;
pub mod graph;
pub mod route;

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

fn pop_word(s: &mut String) {
    let st = s.trim_end();
    if let Some(last_char) = st.chars().last() {
        let new_len = if last_char.is_alphanumeric() || last_char == '_' {
            st.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_').len()
        } else if last_char == ']' {
            let trimmed = st.trim_end_matches(']');
            let len = trimmed.len();
            if trimmed.ends_with('[') { len - 1 } else { len }
        } else if last_char == ')' {
            let trimmed = st.trim_end_matches(')');
            let len = trimmed.len();
            if trimmed.ends_with('{') { len - 1 } else { len }
        } else if last_char == '}' {
            let trimmed = st.trim_end_matches('}');
            let len = trimmed.len();
            if trimmed.ends_with('{') { len - 1 } else { len }
        } else {
            st.trim_end_matches(last_char).len()
        };

        while s.len() > new_len {
            s.pop();
        }
    }
}

fn main() {
    define_verts!{
        verts:
        // Satellites
        Alpha    (A) = (     0.0, 0.0,      0.0);
        Bravo    (B) = (-  100.0, 0.0, -  200.0);
        Charlie  (C) = (     0.0, 0.0, -  200.0);
        Delta    (D) = (   100.0, 0.0, -  200.0);
        Echo     (E) = (   200.0, 0.0, -  100.0);
        Foxtrot  (F) = (   200.0, 0.0,      0.0);
        Golf     (G) = (   200.0, 0.0,    100.0);
        Hotel    (H) = (   100.0, 0.0,    200.0);
        India    (I) = (     0.0, 0.0,    200.0);
        Juliett  (J) = (-  100.0, 0.0,    200.0);
        Kilo     (K) = (-  200.0, 0.0,    100.0);
        Lima     (L) = (-  200.0, 0.0,      0.0);
        Mike     (M) = (-  200.0, 0.0, -  100.0);
        November (N) = (-  300.0, 0.0, -  300.0);
        Oscar    (O) = (   300.0, 0.0, -  300.0);
        Papa     (P) = (   300.0, 0.0,    300.0);
        Quebec   (Q) = (-  300.0, 0.0,    300.0);
        Romeo    (R) = (-  500.0, 0.0, -  500.0);
        Sierra   (S) = (     0.0, 0.0, -  500.0);
        Tango    (T) = (   500.0, 0.0, -  500.0);
        Uniform  (U) = (   500.0, 0.0,      0.0);
        Victor   (V) = (   500.0, 0.0,    500.0);
        Whiskey  (W) = (     0.0, 0.0,    500.0);
        Xray     (X) = (-  500.0, 0.0,    500.0);
        Yankee   (Y) = (-  500.0, 0.0,      0.0);
        Zulu     (Z) = ( 10000.0, 0.0,  10000.0);

        // Transformers
        TR_1 (TR1) = ( 400.0, 0.0,  200.0); // Transformer 1
        TR_2 (TR2) = (-540.0, 0.0,  232.0); // Transformer 2
        TR_3 (TR3) = (-400.0, 0.0, -475.0); // Transformer 3

        // Bridges
        Bridge_Alpha_East  (brAe) = ( -76.9, 0.0,   11.7); // Bridge Alpha East
        Bridge_Alpha_West  (brAw) = ( -51.0, 0.0,   11.8); // Bridge Alpha West
        Bridge_Quebec_East (brQe) = (-223.2, 0.0,  285.1); // Bridge Quebec East
        Bridge_Quebec_West (brQw) = (-197.3, 0.0,  282.8); // Bridge Quebec West
        Bridge_Xray_East   (brXe) = (-400.2, 0.0,  497.7); // Bridge Xray East
        Bridge_Xray_West   (brXw) = (-376.6, 0.0,  508.7); // Bridge Xray West
        Bridge_Echo_East   (brEe) = ( 116.2, 0.0, -121.9); // Bridge Echo East
        Bridge_Echo_West   (brEw) = ( 120.7, 0.0,  -96.3); // Bridge Echo West
        Bridge_Oscar_East  (brOe) = ( 274.2, 0.0, -360.0); // Bridge Oscar East
        Bridge_Oscar_West  (brOw) = ( 295.5, 0.0, -345.0); // Bridge Oscar West
    }

    define_edges!{
        edges:
        A -- E,   A -- F,   A -- G,   A -- H,   A -- I,   A -- J,   A -- P,
        E -- F,   E -- H,   E -- I,   E -- J,   E -- O,   E -- P,   E -- T,   E -- U,   E -- V,   E -- TR1,
        F -- G,   F -- H,   F -- I,   F -- J,   F -- O,   F -- P,   F -- W,   F -- T,   F -- U,   F -- V,   F -- TR1,
        G -- H,   G -- I,   G -- J,   G -- O,   G -- P,   G -- W,   G -- T,   G -- U,   G -- V,   G -- TR1,
        H -- I,   H -- O,   H -- P,   H -- W,   H -- T,   H -- U,   H -- V,   H -- TR1,
        I -- J,   I -- O,   I -- P,   I -- W,   I -- T,   I -- U,   I -- V,
        J -- O,   J -- P,   J -- W,   J -- T,   J -- V,
        O -- P,   O -- W,   O -- T,   O -- U,   O -- V,   O -- TR1,
        P -- W,   P -- T,   P -- U,   P -- V,   P -- TR1,
        W -- U,   W -- V,   W -- TR1,
        T -- U,   T -- TR1,
        U -- V,   U -- TR1,
        V -- TR1,
        B -- C,   B -- K,   B -- L,   B -- M,   B -- N,   B -- Q,   B -- R,   B -- S,   B -- X,   B -- Y,   B -- TR2, B -- TR3,
        C -- D,   C -- K,   C -- L,   C -- M,   C -- N,   C -- Q,   C -- R,   C -- S,   C -- X,   C -- Y,   C -- TR2, C -- TR3,
        D -- K,   D -- L,   D -- M,   D -- N,   D -- R,   D -- S,   D -- X,   D -- TR2, D -- TR3,
        K -- L,   K -- N,   K -- Q,   K -- R,   K -- X,   K -- Y,   K -- TR2, K -- TR3,
        L -- M,   L -- N,   L -- Q,   L -- R,   L -- S,   L -- X,   L -- Y,   L -- TR2, L -- TR3,
        M -- N,   M -- Q,   M -- R,   M -- S,   M -- X,   M -- Y,   M -- TR2, M -- TR3,
        N -- Q,   N -- R,   N -- S,   N -- X,   N -- Y,   N -- TR2, N -- TR3,
        Q -- R,   Q -- X,   Q -- Y,   Q -- TR2, Q -- TR3,
        R -- S,   R -- Y,   R -- TR2, R -- TR3,
        S -- Y,   S -- TR2, S -- TR3,
        X -- Y,   X -- TR2, X -- TR3,
        Y -- TR2, Y -- TR3,
        TR2 -- TR3,
        brXe -- brQe,
        brQe -- brAe,
        brAe -- brEe,
        brEe -- brOe,
        brXw -- brQw,
        brQw -- brAw,
        brAw -- brEw,
        brEw -- brOw,
        A -- brAw, A -- brEw, A -- brQw,
        E -- brAw, E -- brEw, E -- brOw, E -- brQw, E -- brXw,
        F -- brAw, F -- brEw, F -- brOw, F -- brQw, F -- brXw,
        G -- brAw, G -- brEw, G -- brOw, G -- brQw, G -- brXw,
        H -- brAw, H -- brEw, H -- brOw, H -- brQw, H -- brXw,
        I -- brAw, I -- brEw, I -- brOw, I -- brQw, I -- brXw,
        J -- brAw, J -- brEw, J -- brQw, J -- brXw,
        O -- brEw, O -- brOw, O -- brQw, O -- brXw,
        P -- brAw, P -- brEw, P -- brOw, P -- brQw, P -- brXw,
        W -- brAw, W -- brEw, W -- brOw, W -- brQw, W -- brXw,
        T -- brEw, T -- brOw, T -- brQw, T -- brXw,
        U -- brAw, U -- brEw, U -- brOw, U -- brQw, U -- brXw,
        V -- brAw, V -- brEw, V -- brOw, V -- brQw, V -- brXw,
        TR1 -- brAw, TR1 -- brEw, TR1 -- brOw, TR1 -- brQw, TR1 -- brXw,
        B -- brAe, B -- brEe, B -- brOe, B -- brQe, B -- brXe,
        C -- brAe, C -- brEe, C -- brOe, C -- brXe,
        D -- brAe, D -- brEe, D -- brOe,
        K -- brAe, K -- brOe, K -- brQe,
        L -- brAe, L -- brEe, L -- brOe, L -- brQe, L -- brXe,
        M -- brAe, M -- brEe, M -- brOe, M -- brQe, M -- brXe,
        N -- brAe, N -- brEe, N -- brOe, N -- brQe, N -- brXe,
        Q -- brAe, Q -- brQe, Q -- brXe,
        R -- brAe, R -- brEe, R -- brOe, R -- brQe, R -- brXe,
        S -- brAe, S -- brEe, S -- brOe, S -- brQe,
        X -- brAe, X -- brQe, X -- brXe,
        Y -- brAe, Y -- brEe, Y -- brOe, Y -- brQe, Y -- brXe,
        TR2 -- brAe, TR2 -- brEe, TR2 -- brOe, TR2 -- brQe, TR2 -- brXe,
        TR3 -- brAe, TR3 -- brEe, TR3 -- brOe, TR3 -- brQe, TR3 -- brXe,
        brXe -- brXw,
        brQe -- brQw,
        brAe -- brAw,
        brEe -- brEw,
        brOe -- brOw,
    }

    let mut graph = WeightedGraph::new(verts, edges);

    let mut route = None;

    let mut camera = Camera3D::perspective(
        CAMERA_POSITION_DEFAULT,
        Vector3::zero(),
        Vector3::new(0.0, 0.0, -1.0),
        45.0,
    );

    let mut is_giving_command = false;
    let mut command_history = VecDeque::new();
    let mut command_history_offset = 0;
    let mut console: Console = Console::new();
    let mut is_debugging = false;

    let mut tempo_ticks = 1;
    let mut tempo_ms = 16;

    let mut is_cursor_shown = false;
    let mut cursor_last_toggled = Instant::now();

    let mut backspace_pressed = None;

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

    'window: while !rl.window_should_close() {
        if rl.is_key_pressed(KeyboardKey::KEY_ESCAPE) {
            is_giving_command = false;
            console.command.clear();
        }
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            is_giving_command = !is_giving_command;
            if is_giving_command {
                // begin giving command
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now();
                console.command.clear();
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
                                    Cmd::SvRoute => Cmd::run_sv_route(&graph, &mut route, &mut console, args),
                                    Cmd::SvRouteAdd => Cmd::run_sv_route_add(&graph, &mut route, &mut console, args),
                                    Cmd::SvNew => Cmd::run_sv_new(&mut graph, &mut route, camera, &mut console, args),
                                    Cmd::SvEdge => Cmd::run_sv_edge(&mut graph, &mut route, &mut console, args),
                                    Cmd::Tempo => Cmd::run_tempo(&mut console, args, &mut tempo_ticks, &mut tempo_ms),
                                    Cmd::Dbg => {
                                        is_debugging = !is_debugging;
                                        Ok(console_write!(console, Info, "debugging is now {}", if is_debugging { "on" } else { "off" }))
                                    }
                                    Cmd::Focus => Cmd::run_focus(&graph, &mut camera, &mut console, args),
                                    Cmd::Close => break 'window,
                                };
                                if let Err(e) = result {
                                    console_write!(console, Error, "{e}");
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
                command_history_offset = dbg!(if command_history_offset + 1 < command_history.len() + 1 { command_history_offset + 1 } else { 0 });
                console.command = dbg!(command_history.get(dbg!(command_history_offset)).cloned().unwrap_or_default());
            } else if rl.is_key_pressed(KeyboardKey::KEY_DOWN) {
                command_history_offset = dbg!(command_history_offset.checked_sub(1).unwrap_or(command_history.len() + 1 - 1));
                console.command = dbg!(command_history.get(dbg!(command_history_offset)).cloned().unwrap_or_default());
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

        if let Some((route, last_step)) = &mut route {
            if !route.is_finished {
                if let Some(tempo_ms) = std::num::NonZeroU128::new(tempo_ms) {
                    let ms_elapsed = last_step.elapsed().as_millis();
                    let ticks = tempo_ticks * ms_elapsed/tempo_ms;
                    println!("tempo_ticks: {tempo_ticks} tempo_ms: {tempo_ms} ms_elapsed: {ms_elapsed} ticks: {ticks}");
                    for _ in 0..ticks {
                        if route.is_finished { break; }
                        route.step(&mut console, &graph);
                        *last_step = Instant::now();
                    }
                } else {
                    while !route.is_finished {
                        route.step(&mut console, &graph);
                        *last_step = Instant::now();
                    }
                };
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

            for edge in &graph.edges {
                let [i, j] = edge.adj.map(usize::from);
                let p0 = graph.verts[i].pos;
                let p1 = graph.verts[j].pos;
                d.draw_capsule(p0*SCALE_FACTOR, p1*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::RED.alpha(0.25));
            }

            for (v, vert) in graph.verts_iter() {
                let distance_from_target = vert.pos - camera.target;
                let color = loop {
                    if let Some((route, _)) = &route {
                        if let Phase::Edge { current, i } = &route.phase {
                            if &v == current {
                                break Color::SKYBLUE;
                            } else if graph.adjacent[*current as usize].get(*i).is_some_and(|x| &v == &x.vertex) {
                                break Color::ORANGE;
                            }
                        }
                        if !route.is_finished && v == route.root {
                            break Color::BLUE;
                        } else if route.result.contains(&v) {
                            break Color::BLUEVIOLET;
                        } else if route.targets.contains(&v) {
                            break Color::GREEN;
                        }
                    }
                    let dist_sqr = distance_from_target.dot(distance_from_target);
                    if dist_sqr <= VERTEX_RADIUS*VERTEX_RADIUS {
                        break Color::new(255, 128, 128, 255);
                    } else {
                        break Color::RED;
                    }
                };
                let resolution = lerp(24.0, 8.0, (camera.position.distance_to(vert.pos)/1000.0).clamp(0.0, 1.0)).round() as i32; // LOD
                d.draw_sphere_ex(vert.pos*SCALE_FACTOR, VERTEX_RADIUS*SCALE_FACTOR, resolution, resolution, color);
                if let Some((route, _)) = &route {
                    if let Some(Visit { parent: Some(p), .. }) = route.visited[v as usize] {
                        d.draw_capsule(graph.verts[p as usize].pos*SCALE_FACTOR, vert.pos*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::ORANGERED);
                    }
                }
            }

            if let Some((route, _)) = &route {
                for pair in route.result.windows(2) {
                    let [a, b] = pair else { panic!("window(2) should always create 2 elements") };
                    d.draw_capsule(graph.verts[*a as usize].pos*SCALE_FACTOR, graph.verts[*b as usize].pos*SCALE_FACTOR, 2.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
                }
            }

            d.draw_capsule((camera.target + Vector3::new(-5.0, 0.0,  0.0))*SCALE_FACTOR, (camera.target + Vector3::new(5.0, 0.0, 0.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
            d.draw_capsule((camera.target + Vector3::new( 0.0, 0.0, -5.0))*SCALE_FACTOR, (camera.target + Vector3::new(0.0, 0.0, 5.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);

            // d.draw_grid(10, 100.0*SCALE_FACTOR);
        }

        {
            let mouse_pos = d.get_mouse_position();
            d.draw_rectangle_rec(Rectangle::new(mouse_pos.x, 0.0, 3.0, d.get_render_height() as f32), Color::new(255, 255, 255, 32));
            d.draw_rectangle_rec(Rectangle::new(0.0, mouse_pos.y, d.get_render_width() as f32, 3.0), Color::new(255, 255, 255, 32));
        }

        for (v, vert) in graph.verts_iter() {
            let pos = d.get_world_to_screen(vert.pos, camera);
            let text = vert.alias.as_str();
            let text_size = d.measure_text_ex(&font, text, font.baseSize as f32, 0.0);
            d.draw_text_ex(&font, text, pos - rvec2(text_size.x*0.5, font.baseSize/2), font.baseSize as f32, 0.0, Color::WHITE);
            if let Some((route, _)) = &route {
                if let Some(Visit { distance, parent }) = route.visited[v as usize] {
                    let parent_text = parent.map_or("-", |p| &graph.verts[p as usize].alias);
                    let text = format!("{} ({parent_text})", distance.ceil());
                    d.draw_text_ex(&font, &text, pos + rvec2(text_size.x*0.5 + 3.0, 3), font.baseSize as f32, 0.0, Color::GRAY);
                }
            }
        }

        // Console

        if !is_giving_command {
            d.draw_text_ex(
                &font,
                "use WASD to pan and RFZX to orbit",
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
