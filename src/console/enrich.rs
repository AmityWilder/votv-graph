use raylib::prelude::*;

#[derive(Clone)]
pub struct Enrich<'a> {
    text: &'a str,
    root_color: Color,
    color_stack: Vec<Color>,
}
impl<'a> Enrich<'a> {
    pub fn new(
        text: &'a str,
        root_color: Color,
        initial: &str,
    ) -> Self {
        let color_stack = if !initial.is_empty() {
            let mut init = Enrich::new(initial, root_color, "");
            while let Some(_) = init.next() {
                // ignore
            }
            init.color_stack
        } else {
            Vec::new()
        };
        Self {
            text,
            root_color,
            color_stack,
        }
    }
}

pub trait EnrichEx {
    fn enrich(
        &self,
        root_color: Color,
        initial: &str,
    ) -> Enrich<'_>;
}
impl EnrichEx for str {
    fn enrich(
        &self,
        root_color: Color,
        initial: &str,
    ) -> Enrich<'_> {
        Enrich::new(self, root_color, initial)
    }
}

impl<'a> Iterator for Enrich<'a> {
    type Item = (&'a str, Color);

    fn next(&mut self) -> Option<Self::Item> {
        while !self.text.is_empty() {
            let segment_color = *self.color_stack.last().unwrap_or(&self.root_color);

            let result;
            (result, self.text) = self.text
                .match_indices('<')
                .filter_map(|(i, _)| {
                    let (pre, post) = self.text.split_at(i);
                    post.find('>')
                        .map(|pos| {
                            let (mid, post) = post.split_at(pos + 1);
                            (pre, mid, post)
                        })
                })
                .find(|&(pre, ext, post)| {
                    if ext == "</color>" {
                        if cfg!(debug_assertions) && self.color_stack.is_empty() {
                            println!("built-in messages should not contain excessive color stack pops\n  pre: {pre:?}\n  post: {post:?}");
                        }
                        _ = self.color_stack.pop();
                        true
                    } else if let Some(color) = ext
                        .strip_prefix("<color")
                        .and_then(|s| s
                            .trim_start_matches(' ')
                            .strip_prefix('=')
                        )
                        .map(|s| s
                            .strip_suffix('>').expect("should be guarded by filter_map")
                            .trim_matches(' ')
                        )
                        .and_then(|color_str| color_str
                            .parse()
                            .map(|RichColor(color)| color)
                            .ok()
                        )
                    {
                        self.color_stack.push(color);
                        true
                    } else {
                        // if cfg!(debug_assertions) && ext.starts_with("<color") {
                        //     println!("possibly broken ext: {ext:?}\npre: {pre:?} post: {post:?}");
                        // }
                        false
                    }
                })
                .map_or(
                    (self.text, ""),
                    |(pre, _, post)| (pre, post),
                );

            if !result.is_empty() {
                return Some((result, segment_color));
            }
        }
        None
    }
}

#[derive(Clone, Copy)]
struct RichColor(Color);

#[derive(Debug)]
enum FromColorError {
    NonAscii,
    UnknownSyntax,
    UnknownName,
    BadInt(std::num::ParseIntError),
    BadConversion(std::num::TryFromIntError),
    BadFloat(std::num::ParseFloatError),
    BadComponentCount,
}
impl std::fmt::Display for FromColorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NonAscii => f.write_str("a color string must be ASCII, not UTF"),
            Self::UnknownSyntax => f.write_str("the color format does not match any known syntax"),
            Self::UnknownName => f.write_str("the color appears to be named but does not match a recognized color name"),
            Self::BadInt(_) => f.write_str("failed to parse an integer"),
            Self::BadConversion(_) => f.write_str("failed to convert an integer"),
            Self::BadFloat(_) => f.write_str("failed to parse a float"),
            Self::BadComponentCount => f.write_str("an invalid number of color components were provided"),
        }
    }
}
impl std::error::Error for FromColorError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            | Self::NonAscii
            | Self::UnknownSyntax
            | Self::UnknownName
            | Self::BadComponentCount
                => None,

            Self::BadInt(e) => Some(e),
            Self::BadConversion(e) => Some(e),
            Self::BadFloat(e) => Some(e),
        }
    }
}

impl std::str::FromStr for RichColor {
    type Err = FromColorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.is_ascii() {
            return Err(FromColorError::NonAscii);
        }

        if let Some(s) = s.strip_prefix('#') {
            let chunk_size = match s.len() {
                3|4 => 1,
                6|8 => 2,
                _ => return Err(FromColorError::BadComponentCount),
            };

            let mut it = s
                .as_bytes()
                .chunks_exact(chunk_size)
                .map(|x|
                    str::from_utf8(x)
                        .expect("should be guarded by s.is_ascii()")
                )
                .map(|item|
                    u8::from_str_radix(item, 0x10)
                        .map_err(|e| FromColorError::BadInt(e))
                );

            let [r, g, b] = it.next_chunk().expect("should be guarded by chunk_size");
            let a = it.next().unwrap_or(Ok(255));
            Ok(RichColor(Color::new(r?, g?, b?, a?)))
        } else if let Some(s) = s.strip_prefix("rgb(").and_then(|s| s.strip_suffix(')')) {
            if s.matches(',').count() == 2 {
                let [r, g, b] = s
                    .split(',')
                    .map(|x| x.trim_matches(' '))
                    .map(|item|
                        u8::from_str_radix(item, 10)
                            .map_err(|e| FromColorError::BadInt(e))
                    )
                    .next_chunk()
                        .map_err(|_| FromColorError::BadComponentCount)?;

                Ok(RichColor(Color::new(r?, g?, b?, 255)))
            } else {
                Err(FromColorError::BadComponentCount)
            }
        } else if let Some(s) = s.strip_prefix("rgba(").and_then(|x| x.strip_suffix(')')) {
            if s.matches(',').count() == 3 {
                let [r, g, b, a] = s
                    .split(',')
                    .map(|x| x.trim_matches(' '))
                    .enumerate()
                    .map(|(n, item)|
                        if n < 3 {
                            u8::from_str_radix(item, 10)
                                .map_err(|e| FromColorError::BadInt(e))
                        } else {
                            u8::from_str_radix(item, 10)
                                .or_else(|_|
                                    item.parse::<f32>()
                                        .map_err(|e| FromColorError::BadFloat(e))
                                        .and_then(|a|
                                            u8::try_from((a * 255.0) as i32)
                                                .map_err(|e| FromColorError::BadConversion(e))
                                        )
                                )

                        }
                    )
                    .next_chunk()
                        .map_err(|_| FromColorError::BadComponentCount)?;

                Ok(RichColor(Color::new(r?, g?, b?, a?)))
            } else {
                Err(FromColorError::BadComponentCount)
            }
        } else if matches!(s.len(), 3..=20) && s.chars().all(char::is_alphabetic) {
            const NAMED_COLORS: [(&str, Color); 145] = [
                ("INDIANRED",            Color::INDIANRED),
                ("LIGHTCORAL",           Color::LIGHTCORAL),
                ("SALMON",               Color::SALMON),
                ("DARKSALMON",           Color::DARKSALMON),
                ("LIGHTSALMON",          Color::LIGHTSALMON),
                ("CRIMSON",              Color::CRIMSON),
                ("RED",                  Color::RED),
                ("FIREBRICK",            Color::FIREBRICK),
                ("DARKRED",              Color::DARKRED),
                ("PINK",                 Color::PINK),
                ("LIGHTPINK",            Color::LIGHTPINK),
                ("HOTPINK",              Color::HOTPINK),
                ("DEEPPINK",             Color::DEEPPINK),
                ("MEDIUMVIOLETRED",      Color::MEDIUMVIOLETRED),
                ("PALEVIOLETRED",        Color::PALEVIOLETRED),
                ("CORAL",                Color::CORAL),
                ("TOMATO",               Color::TOMATO),
                ("ORANGERED",            Color::ORANGERED),
                ("DARKORANGE",           Color::DARKORANGE),
                ("ORANGE",               Color::ORANGE),
                ("GOLD",                 Color::GOLD),
                ("YELLOW",               Color::YELLOW),
                ("LIGHTYELLOW",          Color::LIGHTYELLOW),
                ("LEMONCHIFFON",         Color::LEMONCHIFFON),
                ("LIGHTGOLDENRODYELLOW", Color::LIGHTGOLDENRODYELLOW),
                ("PAPAYAWHIP",           Color::PAPAYAWHIP),
                ("MOCCASIN",             Color::MOCCASIN),
                ("PEACHPUFF",            Color::PEACHPUFF),
                ("PALEGOLDENROD",        Color::PALEGOLDENROD),
                ("KHAKI",                Color::KHAKI),
                ("DARKKHAKI",            Color::DARKKHAKI),
                ("LAVENDER",             Color::LAVENDER),
                ("THISTLE",              Color::THISTLE),
                ("PLUM",                 Color::PLUM),
                ("VIOLET",               Color::VIOLET),
                ("ORCHID",               Color::ORCHID),
                ("FUCHSIA",              Color::FUCHSIA),
                ("MAGENTA",              Color::MAGENTA),
                ("MEDIUMORCHID",         Color::MEDIUMORCHID),
                ("MEDIUMPURPLE",         Color::MEDIUMPURPLE),
                ("REBECCAPURPLE",        Color::REBECCAPURPLE),
                ("BLUEVIOLET",           Color::BLUEVIOLET),
                ("DARKVIOLET",           Color::DARKVIOLET),
                ("DARKORCHID",           Color::DARKORCHID),
                ("DARKMAGENTA",          Color::DARKMAGENTA),
                ("PURPLE",               Color::PURPLE),
                ("DARKPURPLE",           Color::DARKPURPLE),
                ("INDIGO",               Color::INDIGO),
                ("SLATEBLUE",            Color::SLATEBLUE),
                ("DARKSLATEBLUE",        Color::DARKSLATEBLUE),
                ("MEDIUMSLATEBLUE",      Color::MEDIUMSLATEBLUE),
                ("GREENYELLOW",          Color::GREENYELLOW),
                ("CHARTREUSE",           Color::CHARTREUSE),
                ("LAWNGREEN",            Color::LAWNGREEN),
                ("LIME",                 Color::LIME),
                ("LIMEGREEN",            Color::LIMEGREEN),
                ("PALEGREEN",            Color::PALEGREEN),
                ("LIGHTGREEN",           Color::LIGHTGREEN),
                ("MEDIUMSPRINGGREEN",    Color::MEDIUMSPRINGGREEN),
                ("SPRINGGREEN",          Color::SPRINGGREEN),
                ("MEDIUMSEAGREEN",       Color::MEDIUMSEAGREEN),
                ("SEAGREEN",             Color::SEAGREEN),
                ("FORESTGREEN",          Color::FORESTGREEN),
                ("GREEN",                Color::GREEN),
                ("DARKGREEN",            Color::DARKGREEN),
                ("YELLOWGREEN",          Color::YELLOWGREEN),
                ("OLIVEDRAB",            Color::OLIVEDRAB),
                ("OLIVE",                Color::OLIVE),
                ("DARKOLIVEGREEN",       Color::DARKOLIVEGREEN),
                ("MEDIUMAQUAMARINE",     Color::MEDIUMAQUAMARINE),
                ("DARKSEAGREEN",         Color::DARKSEAGREEN),
                ("LIGHTSEAGREEN",        Color::LIGHTSEAGREEN),
                ("DARKCYAN",             Color::DARKCYAN),
                ("TEAL",                 Color::TEAL),
                ("AQUA",                 Color::AQUA),
                ("CYAN",                 Color::CYAN),
                ("LIGHTCYAN",            Color::LIGHTCYAN),
                ("PALETURQUOISE",        Color::PALETURQUOISE),
                ("AQUAMARINE",           Color::AQUAMARINE),
                ("TURQUOISE",            Color::TURQUOISE),
                ("MEDIUMTURQUOISE",      Color::MEDIUMTURQUOISE),
                ("DARKTURQUOISE",        Color::DARKTURQUOISE),
                ("CADETBLUE",            Color::CADETBLUE),
                ("STEELBLUE",            Color::STEELBLUE),
                ("LIGHTSTEELBLUE",       Color::LIGHTSTEELBLUE),
                ("POWDERBLUE",           Color::POWDERBLUE),
                ("LIGHTBLUE",            Color::LIGHTBLUE),
                ("SKYBLUE",              Color::SKYBLUE),
                ("LIGHTSKYBLUE",         Color::LIGHTSKYBLUE),
                ("DEEPSKYBLUE",          Color::DEEPSKYBLUE),
                ("DODGERBLUE",           Color::DODGERBLUE),
                ("CORNFLOWERBLUE",       Color::CORNFLOWERBLUE),
                ("ROYALBLUE",            Color::ROYALBLUE),
                ("BLUE",                 Color::BLUE),
                ("MEDIUMBLUE",           Color::MEDIUMBLUE),
                ("DARKBLUE",             Color::DARKBLUE),
                ("NAVY",                 Color::NAVY),
                ("MIDNIGHTBLUE",         Color::MIDNIGHTBLUE),
                ("CORNSILK",             Color::CORNSILK),
                ("BLANCHEDALMOND",       Color::BLANCHEDALMOND),
                ("BISQUE",               Color::BISQUE),
                ("NAVAJOWHITE",          Color::NAVAJOWHITE),
                ("WHEAT",                Color::WHEAT),
                ("BURLYWOOD",            Color::BURLYWOOD),
                ("TAN",                  Color::TAN),
                ("ROSYBROWN",            Color::ROSYBROWN),
                ("SANDYBROWN",           Color::SANDYBROWN),
                ("GOLDENROD",            Color::GOLDENROD),
                ("DARKGOLDENROD",        Color::DARKGOLDENROD),
                ("PERU",                 Color::PERU),
                ("CHOCOLATE",            Color::CHOCOLATE),
                ("SADDLEBROWN",          Color::SADDLEBROWN),
                ("SIENNA",               Color::SIENNA),
                ("BROWN",                Color::BROWN),
                ("DARKBROWN",            Color::DARKBROWN),
                ("MAROON",               Color::MAROON),
                ("WHITE",                Color::WHITE),
                ("SNOW",                 Color::SNOW),
                ("HONEYDEW",             Color::HONEYDEW),
                ("MINTCREAM",            Color::MINTCREAM),
                ("AZURE",                Color::AZURE),
                ("ALICEBLUE",            Color::ALICEBLUE),
                ("GHOSTWHITE",           Color::GHOSTWHITE),
                ("WHITESMOKE",           Color::WHITESMOKE),
                ("SEASHELL",             Color::SEASHELL),
                ("BEIGE",                Color::BEIGE),
                ("OLDLACE",              Color::OLDLACE),
                ("FLORALWHITE",          Color::FLORALWHITE),
                ("IVORY",                Color::IVORY),
                ("ANTIQUEWHITE",         Color::ANTIQUEWHITE),
                ("LINEN",                Color::LINEN),
                ("LAVENDERBLUSH",        Color::LAVENDERBLUSH),
                ("MISTYROSE",            Color::MISTYROSE),
                ("GAINSBORO",            Color::GAINSBORO),
                ("LIGHTGRAY",            Color::LIGHTGRAY),
                ("SILVER",               Color::SILVER),
                ("DARKGRAY",             Color::DARKGRAY),
                ("GRAY",                 Color::GRAY),
                ("DIMGRAY",              Color::DIMGRAY),
                ("LIGHTSLATEGRAY",       Color::LIGHTSLATEGRAY),
                ("SLATEGRAY",            Color::SLATEGRAY),
                ("DARKSLATEGRAY",        Color::DARKSLATEGRAY),
                ("BLACK",                Color::BLACK),
                ("BLANK",                Color::BLANK),
                ("RAYWHITE",             Color::RAYWHITE),
            ];

            NAMED_COLORS.iter()
                .copied()
                .find_map(|(name, value)|
                    s.eq_ignore_ascii_case(name)
                        .then_some(RichColor(value))
                )
                .ok_or(FromColorError::UnknownName)
        } else {
            Err(FromColorError::UnknownSyntax)
        }
    }
}
