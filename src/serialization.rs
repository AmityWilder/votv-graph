use std::str::FromStr;
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
pub enum LoadGraphErrorKind {
    UnexpectedEOF,
    MissingVersion,
    UnknownVersion(Version),
    UnknownVertex(String),
    IncompletePosition,
    Unexpected,
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    TryFromInt(std::num::TryFromIntError),
}
use LoadGraphErrorKind::*;
#[derive(Debug)]
pub struct LoadGraphError {
    line: usize,
    code: Option<String>,
    kind: LoadGraphErrorKind,
}
impl LoadGraphError {
    pub const fn new(line: usize, kind: LoadGraphErrorKind) -> Self {
        Self {
            line,
            code: None,
            kind,
        }
    }
    pub fn new_with_code(line: usize, code: impl ToString, kind: LoadGraphErrorKind) -> Self {
        Self {
            line,
            code: Some(code.to_string()),
            kind,
        }
    }
}
impl std::fmt::Display for LoadGraphError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at line {}: ", self.line + 1)?;
        match &self.kind {
            UnexpectedEOF => f.write_str("unexpected end of file"),
            MissingVersion => f.write_str("missing version number"),
            UnknownVersion(v) => write!(f, "unknown version number: {v:?} (current: {CURRENT_VERSION:?})"),
            UnknownVertex(id) => write!(f, "edge references an unknown vertex: id `{id}`"),
            IncompletePosition => f.write_str("position is missing one or more coordinates"),
            Unexpected => f.write_str("unexpected text"),
            ParseInt(_) => f.write_str("error while trying to parse an integer"),
            ParseFloat(_) => f.write_str("error while trying to parse a float"),
            TryFromInt(_) => f.write_str("failed to convert integer"),
        }?;
        if let Some(code) = &self.code {
            write!(f, "\ncode: \"{code}\"")?;
        }
        Ok(())
    }
}
impl std::error::Error for LoadGraphError {}
impl From<LoadGraphError> for std::io::Error {
    fn from(value: LoadGraphError) -> Self {
        std::io::Error::other(value)
    }
}

impl WeightedGraph {
    pub fn load_from_memory<C: AsRef<str>>(bytes: C) -> Result<Self, LoadGraphError> {
        let mut line_iter = bytes.as_ref().lines().enumerate();

        // Check version
        {
            let (version_line, mut version_code) = line_iter.next()
                .ok_or(LoadGraphError::new(0, UnexpectedEOF))?;

            if !version_code.starts_with('v') {
                return Err(LoadGraphError::new_with_code(version_line, version_code, MissingVersion));
            }

            version_code = &version_code[1..];

            let version = version_code.parse::<Version>()
                .map_err(|e| LoadGraphError::new_with_code(version_line, version_code, ParseInt(e)))?;

            if version > CURRENT_VERSION {
                return Err(LoadGraphError::new_with_code(version_line, version_code, UnknownVersion(version)));
            }
        }

        let mut verts: Vec<Vertex> = Vec::new();
        let mut edges: Vec<Edge> = Vec::new();
        for (line, mut code) in line_iter {
            if let Some(comment_start) = code.find("//") {
                code = &code[..comment_start];
            }

            if code.trim().is_empty() { continue; }

            if let Some((a, mut b)) = code.split_once("--") {
                // edge
                let mut weight_str;
                (b, weight_str) = b.split_once(':').unwrap_or((b, ""));

                let find_vert = |s: &str| -> Result<VertexID, LoadGraphError> {
                    verts.iter()
                        .position(|v| v.alias.as_str() == s || v.id.as_str() == s)
                            .ok_or_else(|| LoadGraphError::new_with_code(line, code, UnknownVertex(s.to_string())))?
                        .try_into()
                            .map_err(|e| LoadGraphError::new_with_code(line, code, TryFromInt(e)))
                };

                let adj = [find_vert(a.trim())?, find_vert(b.trim())?];

                weight_str = weight_str.trim();
                let weight_start = weight_str.chars().next();
                let weight =
                    if weight_start.is_none_or(|ch| matches!(ch, '*'|'+')) {
                        let distance = verts[adj[0] as usize].pos.distance_to(verts[adj[1] as usize].pos);
                        if let Some(ch) = weight_start {
                            let weight_aug = weight_str[1..]
                                .trim_start()
                                .parse::<f32>()
                                    .map_err(|e| LoadGraphError::new_with_code(line, code, ParseFloat(e)))?;
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
                            .map_err(|e| LoadGraphError::new_with_code(line, code, ParseFloat(e)))?
                    };

                edges.push(Edge { id: None, adj, weight });
            } else if let Some((name, pos_str)) = code.split_once('=') {
                // vertex
                let (id, alias) =
                    if let Some((id, alias)) = name.split_once(':') {
                        (id.trim(), alias.trim())
                    } else {
                        let id = name.trim();
                        (id, id)
                    };

                let (id, alias) = (id.to_string(), alias.to_string());

                let pos_comp = |iter: &mut std::str::Split<'_, char>| -> Result<f32, LoadGraphError> {
                    iter.next()
                            .ok_or_else(|| LoadGraphError::new_with_code(line, code, IncompletePosition))?
                        .trim()
                        .parse()
                            .map_err(|e| LoadGraphError::new_with_code(line, code, ParseFloat(e)))
                };

                let mut pos_iter = pos_str.split(',');
                let x = pos_comp(&mut pos_iter)?;
                let y = pos_comp(&mut pos_iter)?;
                let z = pos_comp(&mut pos_iter)?;

                if let Some(code) = pos_iter.next() {
                    return Err(LoadGraphError::new_with_code(line, code, Unexpected));
                }
                let pos = Vector3::new(x, y, z);

                verts.push(Vertex { id, alias, pos });
            } else {
                return Err(LoadGraphError::new_with_code(line, code, Unexpected));
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
