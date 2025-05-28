use std::{error::Error, fmt::{self, Display, Formatter}, num::{NonZeroU32, ParseFloatError, ParseIntError}, str::FromStr};
use raylib::prelude::*;

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct NonNaNF32(f32);

impl Ord for NonNaNF32 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl Eq for NonNaNF32 {}

#[allow(unused)]
impl NonNaNF32 {
    #[inline]
    pub const fn new(value: f32) -> Option<Self> {
        if value.is_nan() { None } else { Some(Self(value)) }
    }

    #[inline]
    pub const fn new_unchecked(value: f32) -> Self {
        assert!(!value.is_nan(), "NonNaNF32 cannot be NaN");
        Self(value)
    }

    #[inline]
    pub const fn get(self) -> f32 {
        self.0
    }

    #[inline]
    pub const fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }

    #[inline]
    pub const fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }

    #[inline]
    pub const fn mul(self, other: Self) -> Self {
        Self(self.0 * other.0)
    }

    #[inline]
    pub const fn div_checked(self, other: Self) -> Option<Self> {
        let new_value = self.0 / other.0;
        if new_value.is_nan() { None } else { Some(Self(new_value)) }
    }

    #[inline]
    pub fn sqrt_checked(self) -> Option<Self> {
        let new_value = self.0.sqrt();
        if new_value.is_nan() { None } else { Some(Self(new_value)) }
    }
}

#[derive(Debug)]
pub enum ParseCoordsError {
    Invalid,
    ParseFloat(ParseFloatError),
}

impl Display for ParseCoordsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid => f.write_str("expected `x:<???>/y:<???>[/z:<???>]`"),
            Self::ParseFloat(_) => f.write_str("failed to read number"),
        }
    }
}

impl Error for ParseCoordsError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Invalid => None,
            Self::ParseFloat(e) => Some(e),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Coords(pub Vector3);

impl FromStr for Coords {
    type Err = ParseCoordsError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split('/')
            .zip(["x:", "y:", "z:"].into_iter())
            .map(|(s, pre)| s
                .strip_prefix(pre)
                .ok_or(ParseCoordsError::Invalid)
                .and_then(|n| n
                    .parse::<f32>()
                    .map_err(|e| ParseCoordsError::ParseFloat(e))
                )
            );

        let x = it.next().ok_or(ParseCoordsError::Invalid).and_then(|x| x)?;
        let y = it.next().ok_or(ParseCoordsError::Invalid).and_then(|x| x)?;
        let z = if let Some(x) = it.next() { x? } else { 0.0 };
        Ok(Self(Vector3 { x, y, z }))
    }
}

impl Display for Coords {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(Vector3 { x, y, z }) = self;
        write!(f, "x:{x}/y:{y}/z:{z}")
    }
}

#[derive(Debug)]
pub enum ParseTempoError {
    Invalid,
    ParseInt(ParseIntError),
}

impl Display for ParseTempoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid => f.write_str("expected `reset|sync|sprint|instant|pause|ticks:<???>/ms:<???>`"),
            Self::ParseInt(_) => f.write_str("failed to read number"),
        }
    }
}

impl Error for ParseTempoError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Invalid => None,
            Self::ParseInt(e) => Some(e),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tempo {
    Sync,
    Sprint,
    Instant,
    Pause,
    Exact { ticks: NonZeroU32, ms: NonZeroU32 },
}

impl FromStr for Tempo {
    type Err = ParseTempoError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "reset" => Self::new(),
            "sync" => Self::Sync,
            "sprint" => Self::Sprint,
            "instant" => Self::Instant,
            "pause" => Self::Pause,
            _ => {
                let mut it = s.split('/')
                    .zip(["ticks:", "ms:"].into_iter())
                    .map(|(s, pre)| s
                        .strip_prefix(pre)
                        .ok_or(ParseTempoError::Invalid)
                        .and_then(|n| n
                            .parse::<u32>()
                            .map_err(|e| ParseTempoError::ParseInt(e))
                        )
                    );

                let ticks = it.next().ok_or(ParseTempoError::Invalid).and_then(|x| x)?;
                let ms    = it.next().ok_or(ParseTempoError::Invalid).and_then(|x| x)?;
                match (NonZeroU32::new(ticks), NonZeroU32::new(ms)) {
                    (None, _) => Self::Pause,
                    (Some(_), None) => Self::Instant,
                    (Some(ticks), Some(ms)) => Self::Exact { ticks, ms },
                }
            }
        })
    }
}

impl Default for Tempo {
    fn default() -> Self {
        Self::new()
    }
}

impl Tempo {
    pub const fn new() -> Self {
        Self::Exact {
            ticks: unsafe { NonZeroU32::new_unchecked(1) },
            ms:    unsafe { NonZeroU32::new_unchecked(1) },
        }
    }
}

impl Display for Tempo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Tempo::Sync => f.write_str("FPS sync"),
            Tempo::Sprint => f.write_str("max responsive"),
            Tempo::Instant => f.write_str("run in one frame"),
            Tempo::Pause => f.write_str("paused"),
            Tempo::Exact { ticks, ms } => write!(f, "{ticks} steps every {ms}ms"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RichColor(pub Color);

impl RichColor {
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
}

#[derive(Debug)]
pub enum ParseColorError {
    NonAscii,
    UnknownSyntax,
    UnknownName,
    BadInt(std::num::ParseIntError),
    BadConversion(std::num::TryFromIntError),
    BadFloat(std::num::ParseFloatError),
    BadComponentCount,
}
impl std::fmt::Display for ParseColorError {
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
impl std::error::Error for ParseColorError {
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
    type Err = ParseColorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.is_ascii() {
            return Err(ParseColorError::NonAscii);
        }

        if let Some(s) = s.strip_prefix('#') {
            let chunk_size = match s.len() {
                3|4 => 1,
                6|8 => 2,
                _ => return Err(ParseColorError::BadComponentCount),
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
                        .map_err(|e| ParseColorError::BadInt(e))
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
                            .map_err(|e| ParseColorError::BadInt(e))
                    )
                    .next_chunk()
                        .map_err(|_| ParseColorError::BadComponentCount)?;

                Ok(RichColor(Color::new(r?, g?, b?, 255)))
            } else {
                Err(ParseColorError::BadComponentCount)
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
                                .map_err(|e| ParseColorError::BadInt(e))
                        } else {
                            u8::from_str_radix(item, 10)
                                .or_else(|_|
                                    item.parse::<f32>()
                                        .map_err(|e| ParseColorError::BadFloat(e))
                                        .and_then(|a|
                                            u8::try_from((a * 255.0) as i32)
                                                .map_err(|e| ParseColorError::BadConversion(e))
                                        )
                                )

                        }
                    )
                    .next_chunk()
                        .map_err(|_| ParseColorError::BadComponentCount)?;

                Ok(RichColor(Color::new(r?, g?, b?, a?)))
            } else {
                Err(ParseColorError::BadComponentCount)
            }
        } else if matches!(s.len(), 3..=20) && s.chars().all(char::is_alphabetic) {
            Self::NAMED_COLORS.iter()
                .copied()
                .find_map(|(name, value)|
                    s.eq_ignore_ascii_case(name)
                        .then_some(RichColor(value))
                )
                .ok_or(ParseColorError::UnknownName)
        } else {
            Err(ParseColorError::UnknownSyntax)
        }
    }
}

impl Display for RichColor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(Color { r, g, b, a }) = *self;
        if a == 255 {
            write!(f, "rgb({r},{g},{b})")
        } else {
            write!(f, "rgba({r},{g},{b},{a})")
        }
    }
}
