use std::{str::FromStr, ops::Range};
use crate::graph::{Edge, Vertex, VertexID, WeightedGraph};
use raylib::prelude::*;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version {
    major: u8,
    minor: u8,
    patch: u16,
}
impl Version {
    pub const fn new(major: u8, minor: u8, patch: u16) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }
}
impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { major, minor, patch } = self;
        write!(f, "{major}.{minor}.{patch}")
    }
}
impl FromStr for Version {
    type Err = std::num::ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (major, (minor, patch)) = s.split_once('.')
            .map_or(
                (s, ("0", "0")),
                |(major, rest)| (
                    major,
                    rest.split_once('.')
                        .unwrap_or((rest, "0"))
                )
            );
        Ok(Self::new(major.parse()?, minor.parse()?, patch.parse()?))
    }
}
static CURRENT_VERSION: Version = Version::new(0, 0, 1);

#[derive(Debug)]
pub struct Source<S> {
    line: usize,
    range: Range<usize>,
    code: S,
}
impl<'a, 'b: 'a> Source<&'b str> {
    fn new(line: usize, substr: &'a str, code: &'b str) -> Self {
        Source {
            line,
            range: code.substr_range(substr)
                .expect("`substr` should be a slice within `code`"),
            code: code.into(),
        }
    }
}
impl From<Source<&str>> for Source<String> {
    fn from(Source { line, range, code }: Source<&str>) -> Self {
        Self {
            line,
            range,
            code: code.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum LoadGraphErrorKind {
    UnexpectedEOF,
    MissingVersion,
    UnknownVersion(Version),
    UnknownVertex,
    DuplicateName(Source<String>),
    EmptyName,
    IncompletePosition,
    Unexpected,
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    UsizeToVertexID(std::num::TryFromIntError),
}
use LoadGraphErrorKind::*;

#[derive(Debug)]
pub struct LoadGraphError {
    src: Source<String>,
    kind: LoadGraphErrorKind,
}
impl LoadGraphError {
    fn new<S>(src: Source<S>, kind: LoadGraphErrorKind) -> Self
    where
        Source<S>: Into<Source<String>>,
    {
        Self { src: src.into(), kind }
    }
}
impl std::fmt::Display for LoadGraphError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at line {}: ", self.src.line + 1)?;
        match &self.kind {
            UnexpectedEOF => f.write_str("unexpected end of file"),
            MissingVersion => f.write_str("missing version number"),
            UnknownVersion(v) => write!(f, "incompatible version number: {v} (current: {CURRENT_VERSION})"),
            UnknownVertex => write!(f, "edge references an unknown vertex"),
            DuplicateName(prev) => write!(f, "the name (alias or ID) `{}` appears on multiple vertices (first appearance on line {})",
                &prev.code[prev.range.clone()],
                prev.line + 1,
            ),
            EmptyName => f.write_str("a name (alias or ID) is missing"),
            IncompletePosition => f.write_str("position is missing one or more coordinates"),
            Unexpected => f.write_str("unexpected text"),
            ParseInt(_) => f.write_str("error while trying to parse an integer"),
            ParseFloat(_) => f.write_str("error while trying to parse a float"),
            UsizeToVertexID(_) => write!(f, "failed to convert integer to ID, make sure you have {} vertices or fewer", VertexID::MAX),
        }?;
        let line_msg = match &self.kind {
            UnexpectedEOF => "",
            MissingVersion => "expected a version",
            UnknownVersion(_) => "incompatible version",
            UnknownVertex => "vertex is not defined at this scope",
            DuplicateName(_) => "name is already in use",
            EmptyName => "expected a name",
            IncompletePosition => "expected x, y, z coordinates",
            Unexpected => "didn't expect any more arguments",
            ParseInt(_) => "expected an integer",
            ParseFloat(_) => "expected a float",
            UsizeToVertexID(_) => unimplemented!(),
        };
        let mut snippet = format!("<color=rgb(40,140,250)>{:>4} |</color>    <color=rgb(200,200,200)>{}</color>\
                                  \n     <color=rgb(40,140,250)>|</color>    {:<space$}{:^<squig$} {line_msg}",
            self.src.line + 1,
            self.src.code,
            "",
            "",
            space=self.src.range.start,
            squig=self.src.range.len().max(1),
        );
        if let DuplicateName(prev) = &self.kind {
            let line_msg = "first ocurrance here";
            snippet = format!("<color=rgb(40,140,250)>{:>4} |</color>    <color=rgb(200,200,200)>{}</color>\
                              \n     <color=rgb(40,140,250)>|</color>    {:<space$}<color=rgb(40,140,250)>{:-<squig$} {line_msg}</color>\
                              \n    <color=rgb(40,140,250)>...</color>\
                              \n{snippet}",
                prev.line + 1,
                prev.code,
                "",
                "",
                space=prev.range.start,
                squig=prev.range.len().max(1),
            )
        }
        write!(f, "\ncode:\n{snippet}")?;
        Ok(())
    }
}
impl std::error::Error for LoadGraphError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            | UnexpectedEOF
            | MissingVersion
            | UnknownVersion(_)
            | UnknownVertex
            | DuplicateName(_)
            | EmptyName
            | IncompletePosition
            | Unexpected
                => None,

            ParseInt(e) => Some(e),
            ParseFloat(e) => Some(e),
            UsizeToVertexID(e) => Some(e),
        }
    }
}
impl From<LoadGraphError> for std::io::Error {
    fn from(value: LoadGraphError) -> Self {
        std::io::Error::other(value)
    }
}

impl WeightedGraph {
    pub fn load_from_memory<C: AsRef<str>>(bytes: C) -> Result<Self, LoadGraphError> {
        let bytes = bytes.as_ref();
        let mut line_iter = bytes.lines().enumerate();

        // Check version
        {
            let (version_line, version_code) = line_iter.next()
                .ok_or(LoadGraphError::new(Source::new(0, &bytes[0..0], bytes), UnexpectedEOF))?;

            if !version_code.starts_with('v') {
                return Err(LoadGraphError::new(Source::new(version_line, &version_code[..1], version_code), MissingVersion));
            }

            let version_str = &version_code[1..];

            let version = version_str.parse::<Version>()
                .map_err(|e| LoadGraphError::new(Source::new(version_line, version_str, version_code), ParseInt(e)))?;

            if version > CURRENT_VERSION {
                return Err(LoadGraphError::new(Source::new(version_line, version_str, version_code), UnknownVersion(version)));
            }
        }

        let (vert_count_est, edge_count_est) = bytes
            .lines()
            .fold((0, 0), |(v, e), mut line| {
                if let Some(comment_pos) = line.find("//") {
                    line = &line[..comment_pos];
                }
                if line.contains('=') { (v + 1, e) }
                else if line.contains("--") { (v, e + 1) }
                else { (v, e) }
            });

        struct VertCreation<'a> {
            line: usize,
            id: &'a str,
            alias: Option<&'a str>,
            code: &'a str,
        }
        let mut vert_creation: Vec<VertCreation> = Vec::with_capacity(vert_count_est);
        let mut verts = Vec::with_capacity(vert_count_est);
        let mut edges = Vec::with_capacity(edge_count_est);
        for (line, code) in line_iter {
            let code = code.find("//").map_or(code, |comment_pos| &code[..comment_pos]);

            if code.trim().is_empty() { continue; }

            if let Some((a, mut b)) = code.split_once("--") {
                // edge
                let mut weight_str;
                (b, weight_str) = b.split_once(':').unwrap_or((b, ""));

                fn find_vert(verts: &[Vertex], line: usize, code: &str, s: &str) -> Result<VertexID, LoadGraphError> {
                    verts.iter()
                        .position(|v| v.alias.as_str() == s || v.id.as_str() == s)
                            .ok_or_else(|| LoadGraphError::new(Source::new(line, s, code), UnknownVertex))?
                        .try_into()
                            .map_err(|e| LoadGraphError::new(Source::new(line, s, code), UsizeToVertexID(e)))
                }

                let adj = [
                    find_vert(&verts, line, code, a.trim())?,
                    find_vert(&verts, line, code, b.trim())?,
                ];

                weight_str = weight_str.trim();
                let weight_start = weight_str.chars().next();
                let weight =
                    if weight_start.is_none_or(|ch| matches!(ch, '*'|'+')) {
                        let distance = verts[adj[0] as usize].pos.distance_to(verts[adj[1] as usize].pos);
                        if let Some(ch) = weight_start {
                            weight_str = weight_str[1..].trim_start();
                            let weight_aug = weight_str.parse::<f32>()
                                .map_err(|e| LoadGraphError::new(Source::new(line, weight_str, code), ParseFloat(e)))?;
                            match ch {
                                '*' => distance*weight_aug,
                                '+' => distance + weight_aug,
                                _ => unreachable!(),
                            }
                        } else {
                            distance
                        }
                    } else {
                        weight_str.parse()
                            .map_err(|e| LoadGraphError::new(Source::new(line, weight_str, code), ParseFloat(e)))?
                    };

                edges.push(Edge { id: None, adj, weight });
            } else if let Some((name, pos_str)) = code.split_once('=') {
                // vertex
                let (id, alias, creation) =
                    if let Some((mut id, mut alias)) = name.split_once(':') {
                        (id, alias) = (id.trim(), alias.trim());
                        (id, alias, VertCreation { line, id, alias: Some(alias), code })
                    } else {
                        let id = name.trim();
                        (id, id, VertCreation { line, id, alias: None, code })
                    };

                // check for empty
                for name in [id, alias] {
                    if name.is_empty() {
                        return Err(LoadGraphError::new(Source::new(line, name, code), EmptyName))
                    }
                }

                // check for duplicates
                {
                    let dupe = [id, alias].into_iter()
                        .find_map(|name|
                            vert_creation.iter()
                                .find_map(|prev_creation|
                                    std::iter::once(prev_creation.id).chain(prev_creation.alias.into_iter())
                                        .find_map(|prev_name| name.eq_ignore_ascii_case(prev_name)
                                            .then(|| (
                                                Source::new(prev_creation.line, prev_name, prev_creation.code),
                                                Source::new(line, name, code),
                                            )))
                                )
                        );

                    if let Some((prev, curr)) = dupe {
                        debug_assert_ne!(prev.line, curr.line, "alias == id should not be considered a duplicate");

                        return Err(LoadGraphError::new(curr, DuplicateName(prev.into())))
                    }
                }

                let (id, alias) = (id.to_string(), alias.to_string());

                fn pos_comp(line: usize, code: &str, iter: &mut std::str::Split<'_, char>) -> Result<f32, LoadGraphError> {
                    let comp = iter.next()
                            .ok_or_else(|| LoadGraphError::new(Source::new(line, iter.remainder().unwrap_or_else(|| &code[code.len() - 1..]), code), IncompletePosition))?
                        .trim();

                    comp.parse()
                        .map_err(|e| LoadGraphError::new(Source::new(line, comp, code), ParseFloat(e)))
                }

                let mut pos_iter = pos_str.split(',');
                let x = pos_comp(line, code, &mut pos_iter)?;
                let y = pos_comp(line, code, &mut pos_iter)?;
                let z = pos_comp(line, code, &mut pos_iter)?;

                if let Some(unexpected) = pos_iter.next() {
                    return Err(LoadGraphError::new(Source::new(line, unexpected, code), Unexpected));
                }
                let pos = Vector3::new(x, y, z);

                vert_creation.push(creation);
                verts.push(Vertex { id, alias, pos });
            } else {
                return Err(LoadGraphError::new(Source::new(line, code, code), Unexpected));
            }
        }
        Ok(Self::new(verts, edges))
    }

    pub fn save_to_memory(&self) -> String {
        std::iter::once(format!("v{CURRENT_VERSION}"))
            .chain(self.verts().into_iter().map(|v| format!("{}:{}={},{},{}", &v.id, &v.alias, v.pos.x, v.pos.y, v.pos.z)))
            .chain(self.edges().into_iter().map(|e| {
                fn shorter<'a>(str1: &'a str, str2: &'a str) -> &'a str {
                    if str1.len() < str2.len() { str1 } else { str2 }
                }
                let [a, b] = e.adj.map(|v| self.vert(v));
                format!("{}--{}:{}", shorter(&a.id, &a.alias), shorter(&b.id, &b.alias), e.weight)
            }))
            .collect::<Vec<_>>()
            .join("\n")
    }
}
