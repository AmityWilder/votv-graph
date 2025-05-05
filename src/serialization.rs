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
        Self { major, minor, patch }
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Source<S> {
    line: usize,
    range: Range<usize>,
    code: S,
}
impl<'a, 'src: 'a> Source<&'src str> {
    fn new(line: usize, substr: &'a str, code: &'src str) -> Self {
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
    IncompatibleVersion(Version),
    UnknownVertex,
    DuplicateName(Source<String>),
    EmptyName,
    InvalidName,
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
    fn new(src: Source<&str>, kind: LoadGraphErrorKind) -> Self {
        Self { src: src.into(), kind }
    }

    fn unexpected_eof<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), UnexpectedEOF)
    }
    fn missing_version<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), MissingVersion)
    }
    fn unknown_version<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str, version: Version) -> Self {
        Self::new(Source::new(line, substr, code), IncompatibleVersion(version))
    }
    fn unknown_vertex<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), UnknownVertex)
    }
    fn duplicate_name<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str, prev: Source<&str>) -> Self {
        Self::new(Source::new(line, substr, code), DuplicateName(prev.into()))
    }
    fn empty_name<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), EmptyName)
    }
    fn invalid_name<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), InvalidName)
    }
    fn incomplete_position<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), IncompletePosition)
    }
    fn unexpected<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str) -> Self {
        Self::new(Source::new(line, substr, code), Unexpected)
    }
    fn parse_int<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str, e: std::num::ParseIntError) -> Self {
        Self::new(Source::new(line, substr, code), ParseInt(e))
    }
    fn parse_float<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str, e: std::num::ParseFloatError) -> Self {
        Self::new(Source::new(line, substr, code), ParseFloat(e))
    }
    fn usize_to_vertex_id<'a, 'src: 'a>(line: usize, substr: &'a str, code: &'src str, e: std::num::TryFromIntError) -> Self {
        Self::new(Source::new(line, substr, code), UsizeToVertexID(e))
    }
}
impl std::fmt::Display for LoadGraphError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at line {}: ", self.src.line + 1)?;
        match &self.kind {
            UnexpectedEOF => f.write_str("unexpected end of file"),
            MissingVersion => f.write_str("missing version number"),
            IncompatibleVersion(v) => write!(f, "incompatible version number: {v} (current: {CURRENT_VERSION})"),
            UnknownVertex => write!(f, "edge references an unknown vertex"),
            DuplicateName(prev) => write!(f, "the name (alias or ID) `{}` appears on multiple vertices (first appearance on line {})",
                &prev.code[prev.range.clone()],
                prev.line + 1,
            ),
            EmptyName => f.write_str("a name (alias or ID) is missing"),
            InvalidName => f.write_str("a name (alias or ID) contains whitespace"),
            IncompletePosition => f.write_str("position is missing one or more coordinates"),
            Unexpected => f.write_str("unexpected text"),
            ParseInt(_) => f.write_str("error while trying to parse an integer"),
            ParseFloat(_) => f.write_str("error while trying to parse a float"),
            UsizeToVertexID(_) => write!(f, "failed to convert integer to ID, make sure you have {} vertices or fewer", VertexID::MAX),
        }?;
        let line_msg = match &self.kind {
            UnexpectedEOF => "",
            MissingVersion => "expected a version",
            IncompatibleVersion(_) => "incompatible version",
            UnknownVertex => "vertex is not defined at this scope",
            DuplicateName(_) => "name is already in use",
            EmptyName => "expected a name",
            InvalidName => "name cannot contain separator characters",
            IncompletePosition => "expected x, y, z coordinates",
            Unexpected => "didn't expect any more arguments",
            ParseInt(_) => "expected an integer",
            ParseFloat(_) => "expected a float",
            UsizeToVertexID(_) => unimplemented!(),
        };
        let mut snippet = format!("<color = #288cfa>{:>4} |</color>    <color = #c8c8c8>{}</color>\
                                  \n     <color = #288cfa>|</color>    {:<space$}{:^<squig$} {line_msg}",
            self.src.line + 1,
            self.src.code,
            "",
            "",
            space=self.src.range.start,
            squig=self.src.range.len().max(1),
        );
        if let DuplicateName(prev) = &self.kind {
            let line_msg = "first ocurrance here";
            snippet = format!("<color = #288cfa>{:>4} |</color>    <color = #c8c8c8>{}</color>\
                              \n     <color = #288cfa>|</color>    {:<space$}<color = #288cfa>{:-<squig$} {line_msg}</color>\
                              \n    <color = #288cfa>...</color>\
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
            | IncompatibleVersion(_)
            | UnknownVertex
            | DuplicateName(_)
            | EmptyName
            | InvalidName
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

fn find_vert<'a, 'src: 'a>(
    verts: &[Vertex],
    line: usize,
    code: &'src str,
    s: &'a str,
) -> Result<VertexID, LoadGraphError> {
    verts.iter()
        .position(|v| v.alias.as_str() == s || v.id.as_str() == s)
            .ok_or_else(|| LoadGraphError::unknown_vertex(line, s, code))?
        .try_into()
            .map_err(|e| LoadGraphError::usize_to_vertex_id(line, s, code, e))
}

fn pos_comp<'a, 'src: 'a>(
    line: usize,
    code: &'src str,
    iter: &mut std::str::Split<'a, char>,
) -> Result<f32, LoadGraphError> {
    let comp = iter.next()
            .ok_or_else(|| LoadGraphError::incomplete_position(line, iter.remainder().unwrap_or_else(|| &code[code.len() - 1..]), code))?
        .trim();

    comp.parse()
        .map_err(|e| LoadGraphError::parse_float(line, comp, code, e))
}

fn parse_edge<'a, 'src: 'a>(
    line: usize,
    code: &'src str,
    (a, mut b): (&'a str, &'a str),
    verts: &[Vertex],
) -> Result<Edge, LoadGraphError> {
    let mut weight_str;
    (b, weight_str) = b.split_once(':').unwrap_or((b, ""));

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
                    .map_err(|e| LoadGraphError::parse_float(line, weight_str, code, e))?;
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
                .map_err(|e| LoadGraphError::parse_float(line, weight_str, code, e))?
        };

    Ok(Edge { adj, weight })
}

struct VertCreation<'a, 'src: 'a> {
    line: usize,
    id: &'a str,
    alias: Option<&'a str>,
    code: &'src str,
}

fn parse_vert<'a, 'b: 'a, 'src: 'b>(
    line: usize,
    code: &'src str,
    (name, pos_str): (&'a str, &'a str),
    vert_creation: &[VertCreation<'b, 'src>],
) -> Result<(VertCreation<'a, 'src>, Vertex), LoadGraphError> {
    let (id, alias, creation) =
        if let Some((mut id, mut alias)) = name.split_once(':') {
            (id, alias) = (id.trim(), alias.trim());
            (id, alias, VertCreation { line, id, alias: Some(alias), code })
        } else {
            let id = name.trim();
            (id, id, VertCreation { line, id, alias: None, code })
        };

    for name in [id, alias] {
        // check for empty
        if name.is_empty() {
            return Err(LoadGraphError::empty_name(line, name, code))
        }

        // check for invalid
        {
            fn is_invalid(ch: char) -> bool {
                ch.is_whitespace() || (ch.is_ascii_punctuation() && ch != '_')
            }

            if let Some(invalid_pos) = name.find(is_invalid) {
                let mut invalid = &name[invalid_pos..];
                if let Some(invalid_len) = invalid.find(|ch| !is_invalid(ch)) {
                    invalid = &invalid[..invalid_len];
                }
                return Err(LoadGraphError::invalid_name(line, invalid, code))
            }
        }

        // check for duplicates
        {
            let dupe = vert_creation.iter()
                .find_map(|prev_creation|
                    std::iter::once(prev_creation.id).chain(prev_creation.alias.into_iter())
                        .find_map(|prev_name| name.eq_ignore_ascii_case(prev_name)
                            .then(|| (
                                Source::new(prev_creation.line, prev_name, prev_creation.code),
                                name,
                            )))
                );

            if let Some((prev, curr_name)) = dupe {
                debug_assert_ne!(prev.line, line, "alias == id should not be considered a duplicate");

                return Err(LoadGraphError::duplicate_name(line, curr_name, code, prev))
            }
        }
    }

    let (id, alias) = (id.to_string(), alias.to_string());

    let mut pos_iter = pos_str.split(',');
    let x = pos_comp(line, code, &mut pos_iter)?;
    let y = pos_comp(line, code, &mut pos_iter)?;
    let z = pos_comp(line, code, &mut pos_iter)?;

    if let Some(unexpected) = pos_iter.next() {
        return Err(LoadGraphError::unexpected(line, unexpected, code));
    }
    let pos = Vector3::new(x, y, z);

    Ok((creation, Vertex { id, alias, pos }))
}

impl WeightedGraph {
    pub fn load_from_memory<C: AsRef<str>>(bytes: C) -> Result<Self, LoadGraphError> {
        let bytes = bytes.as_ref();
        let mut line_iter = bytes.lines().enumerate();

        // Check version
        {
            let (line, full_code) = line_iter.next()
                .ok_or(LoadGraphError::unexpected_eof(0, &bytes[0..0], bytes))?;

            if !full_code.starts_with('v') {
                return Err(LoadGraphError::missing_version(line, &full_code[..1], full_code));
            }

            let version_str = &full_code[1..];

            let version = version_str.parse::<Version>()
                .map_err(|e| LoadGraphError::parse_int(line, version_str, full_code, e))?;

            if version > CURRENT_VERSION {
                return Err(LoadGraphError::unknown_version(line, version_str, full_code, version));
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

        let mut vert_creation: Vec<VertCreation> = Vec::with_capacity(vert_count_est);
        let mut verts = Vec::with_capacity(vert_count_est);
        let mut edges = Vec::with_capacity(edge_count_est);
        for (line, code) in line_iter {
            let pre_comment = code.find("//")
                .map_or(code, |comment_pos| &code[..comment_pos]);

            if pre_comment.trim().is_empty() { continue; }

            if let Some(edge_code) = pre_comment.split_once("--") {
                // edge
                let edge = parse_edge(line, code, edge_code, &verts)?;
                edges.push(edge);
            } else if let Some(vert_code) = pre_comment.split_once('=') {
                // vertex
                let (creation, vert) = parse_vert(line, code, vert_code, &vert_creation)?;
                vert_creation.push(creation);
                verts.push(vert);
            } else {
                return Err(LoadGraphError::unexpected(line, pre_comment, code));
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

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;
    use super::*;

    #[test]
    fn test_empty() {
        let bytes = "\
            v0.0.1\
            ";

        let g = WeightedGraph::load_from_memory(bytes);

        assert_matches!(g, Ok(_));
    }

    #[test]
    fn test_version_missing() {
        let bytes = "";

        let g = WeightedGraph::load_from_memory(bytes);

        assert_matches!(g,
            Err(LoadGraphError {
                kind: LoadGraphErrorKind::MissingVersion,
                src: Source { line: 0, range, code }
            }) if &code[range.clone()] == ""
        );
    }

    #[test]
    fn test_version_parse() {
        let bytes = "\
            v 0.0.1\
            ";

        let g = WeightedGraph::load_from_memory(bytes);

        assert_matches!(g,
            Err(LoadGraphError {
                kind: LoadGraphErrorKind::ParseInt(_),
                src: Source { line: 0, range, code }
            }) if &code[range.clone()] == " 0"
        );
    }

    #[test]
    fn test_version_compat() {
        let bytes = "\
            v63.0.0\
            ";

        let g = WeightedGraph::load_from_memory(bytes);

        assert_matches!(g,
            Err(LoadGraphError {
                kind: LoadGraphErrorKind::IncompatibleVersion(_),
                src: Source { line: 0, range, code }
            }) if &code[range.clone()] == "63.0.0"
        );
    }

    #[test]
    fn test_version_nonsense() {
        let bytes = "\
            vfdbgkfd\
            ";

        let g = WeightedGraph::load_from_memory(bytes);

        assert_matches!(g,
            Err(LoadGraphError {
                kind: LoadGraphErrorKind::ParseInt(_),
                src: Source { line: 0, range, code }
            }) if &code[range.clone()] == "fdbgkfd"
        );
    }

    #[test]
    fn test1() {
        let bytes = format!("\
            v0.0.1\
            ");

        let g = WeightedGraph::load_from_memory(bytes);

        assert_matches!(g,
            Err(LoadGraphError {
                kind: LoadGraphErrorKind::ParseInt(_),
                src: Source { line: 0, range, code }
            }) if &code[range.clone()] == "fdbgkfd"
        );
    }
}
