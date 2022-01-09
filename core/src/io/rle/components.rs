use itertools::Itertools;
use std::fmt;
use std::str::FromStr;

use super::*;
use crate::axis::{Axis, AXES, U, V, W, X, Y, Z};
use crate::dim::Dim;
use crate::ndrect::BigRect;
use crate::ndvec::{BigVec, BigVec6D, UVec, UVec6D};
use crate::num::{BigInt, One, Zero};

/// RLE contents.
#[derive(Debug, Clone)]
pub struct Rle {
    /// Basic RLE header.
    pub(super) header: RleHeader,
    /// CXRLE (extended RLE) header.
    pub(super) cxrle_header: Option<CxrleHeader>,
    /// RLE runs.
    pub(super) runs: Vec<RleRun>,
    /// Pattern comments, not including CXRLE header.
    pub(super) comments: String,
}
impl Rle {
    /// Removes the CXRLE header.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn without_cxrle(mut self) -> Self {
        self.cxrle_header = None;
        self
    }

    /// Returns the starting position of the first RLE run, in NDCell
    /// coordinates (meaning Y increases upward).
    pub(super) fn first_run_start<D: Dim>(&self) -> BigVec<D> {
        let mut first_run_start = self
            .cxrle_header
            .as_ref()
            // Convert from 6D to ND.
            .map(|cxrle| BigVec::from_fn(|ax| cxrle.pos[ax].clone()))
            .unwrap_or_else(BigVec::zero);
        // Negate all axes except the X axis.
        for &ax in &D::axes()[1..] {
            first_run_start[ax] *= -1;
        }
        first_run_start
    }
    /// Returns the bounding rectangle according to the header; actual contents
    /// may be larger or smaller.
    pub(super) fn rect<D: Dim>(&self) -> BigRect<D> {
        let corner1 = self.first_run_start();

        // Convert from 6D to ND.
        let mut size = UVec::from_fn(|ax| self.header.size[ax]).to_bigvec();
        // Decrement the size along each axis (fixes off-by-one error with
        // `NdRect::span()`, which expects an inclusive range of cells).
        size -= &BigInt::one();
        // Negate all axes except the X axis.
        for &ax in &D::axes()[1..] {
            size[ax] *= -1;
        }

        BigRect::span(corner1.clone(), corner1 + size)
    }

    fn _fmt(&self, f: &mut fmt::Formatter<'_>, is_2_state: TwoState) -> fmt::Result {
        if let Some(cxrle_header) = &self.cxrle_header {
            writeln!(f, "{}", cxrle_header)?;
        }
        for comment_line in self.comments.trim_end().lines() {
            if !comment_line.starts_with('#') {
                write!(f, "#C ")?;
            }
            writeln!(f, "{}", comment_line)?;
        }
        writeln!(f, "{}", self.header)?;

        let mut line_len = 0;
        for run in &self.runs {
            line_len += run.str_len();
            if line_len > MAX_LINE_LEN {
                writeln!(f)?;
                line_len = run.str_len();
            }
            match is_2_state {
                TwoState::TwoStates => write!(f, "{:b}", run)?,
                TwoState::MoreStates => write!(f, "{}", run)?,
            }
        }
        writeln!(f)?;

        Ok(())
    }
}
impl fmt::Display for Rle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self._fmt(f, TwoState::MoreStates)
    }
}
impl fmt::Binary for Rle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self._fmt(f, TwoState::TwoStates)
    }
}
impl FromStr for Rle {
    type Err = RleError;

    fn from_str(s: &str) -> RleResult<Self> {
        let mut header = None;
        let mut cxrle_header = None;
        let mut runs = vec![];
        let mut comments = String::new();

        for line in s.lines() {
            let line = line.trim();
            if line.starts_with("#CXRLE") {
                if cxrle_header.is_some() {
                    return Err(RleError::DuplicateCxrleHeader);
                }
                cxrle_header = Some(line.parse()?);
            } else if line.starts_with('#') {
                comments.push_str(line);
                comments.push('\n');
            } else if line.starts_with('x') && header.is_none() {
                header = Some(line.parse()?);
            } else {
                for run in RLE_RUN_REGEX.find_iter(line) {
                    runs.push(run.as_str().parse()?);
                }
            }
        }

        let header = header.ok_or(RleError::MissingHeader)?;

        Ok(Self {
            header,
            cxrle_header,
            runs,
            comments,
        })
    }
}

/// RLE header contents.
#[derive(Debug, Clone)]
pub struct RleHeader {
    /// Pattern bounding box (sometimes ignored).
    pub size: UVec6D,
    /// Automaton rule.
    pub rule: Option<String>,
}
impl Default for RleHeader {
    fn default() -> Self {
        Self {
            size: UVec6D::repeat(1),
            rule: None,
        }
    }
}
impl fmt::Display for RleHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &ax in AXES {
            // Always include size along X and Y axes. Include size along other
            // axes if they are not 0 or 1.
            if ax == X || ax == Y || self.size[ax] > 1 {
                if ax != X {
                    write!(f, ", ")?;
                }
                write!(f, "{} = {}", ax.name().to_ascii_lowercase(), &self.size[ax])?;
            }
        }

        if let Some(rule) = &self.rule {
            write!(f, ", rule = {}", rule)?;
        }

        Ok(())
    }
}
impl FromStr for RleHeader {
    type Err = RleError;

    fn from_str(s: &str) -> RleResult<Self> {
        let mut ret = Self::default();

        // Parse comma-separated parameters.
        for param in s.split(',') {
            // Each parameter consists of `name = value`.
            match param.split('=').map(str::trim).collect_vec().as_slice() {
                ["x", x] => ret.size[X] = parse_rle_size(x)?,
                ["y", y] => ret.size[Y] = parse_rle_size(y)?,
                ["z", z] => ret.size[Z] = parse_rle_size(z)?,
                ["w", w] => ret.size[W] = parse_rle_size(w)?,
                ["u", u] => ret.size[U] = parse_rle_size(u)?,
                ["v", v] => ret.size[V] = parse_rle_size(v)?,
                ["rule", rule_name] => ret.rule = Some((*rule_name).to_owned()),
                _ => (), // Ignore unknown parameters.
            }
        }

        Ok(ret)
    }
}

fn parse_rle_size(s: &str) -> RleResult<usize> {
    Ok(s.parse::<usize>()
        .map_err(|_| RleError::InvalidSize)?
        .max(1))
}

/// CXRLE header contents.
#[derive(Debug, Default, Clone)]
pub struct CxrleHeader {
    /// Position of bottom-left of pattern (i.e., most negative coordinates).
    ///
    /// This value is using NDCell coordinates, with Y increasing upwards.
    /// Coordinates are negated when writing/reading to/from RLE string.
    pub pos: BigVec6D,
    /// Number of generations simulated.
    pub gen: BigInt,
}
impl fmt::Display for CxrleHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#CXRLE")?;

        // Use the largest nonzero axis to determine the number of dimensions to
        // include in the position in the CXRLE header.
        if let Some(ndim) = AXES
            .iter()
            .rev()
            .find(|&&ax| !self.pos[ax].is_zero())
            .map(|&ax| ax as usize + 1)
        {
            write!(f, " Pos=")?;
            for &ax in crate::axis::ndim_axes(ndim) {
                if ax != Axis::X {
                    write!(f, ",")?;
                }
                write!(f, "{}", self.pos[ax])?;
            }
        }

        if !self.gen.is_zero() {
            write!(f, " Gen={}", self.gen)?;
        }

        Ok(())
    }
}
impl FromStr for CxrleHeader {
    type Err = RleError;

    fn from_str(s: &str) -> RleResult<Self> {
        use RleError::BadCxrleHeader;

        let s = s.strip_prefix("#CXRLE").ok_or(BadCxrleHeader)?.trim();

        let mut ret = Self::default();

        // Find key/value pairs.
        for kv_pair in s.split_whitespace() {
            if let [k, v] = kv_pair.split('=').collect_vec().as_slice() {
                match k.to_ascii_lowercase().as_str() {
                    "pos" => {
                        // Up to 6 position coordinates.
                        let coordinates: Vec<BigInt> = v
                            .split(',')
                            .map(|coord| coord.trim().parse().map_err(|_| BadCxrleHeader))
                            .try_collect()?;
                        for (&ax, coord) in AXES.iter().zip(coordinates) {
                            ret.pos[ax] = coord;
                        }
                    }

                    "gen" => {
                        // Ignore invalid generation count.
                        if let Ok(gen) = v.parse() {
                            ret.gen = gen;
                        }
                    }
                    _ => (), // Ignore unknown keys.
                }
            }
        }

        Ok(ret)
    }
}

#[derive(Debug, Default)]
pub struct RleRunVec(pub Vec<RleRun>);
impl RleRunVec {
    pub fn into_vec(self) -> Vec<RleRun> {
        self.0
    }
    pub fn try_extend(
        &mut self,
        run_iter: impl IntoIterator<Item = RleResult<RleRun>>,
    ) -> RleResult<()> {
        for run in run_iter {
            self.append(run?);
        }
        Ok(())
    }
    pub fn append(&mut self, run: impl Into<RleRun>) {
        let mut run = run.into();
        while let Some(last) = self.0.last() {
            match last.try_merge(run) {
                Ok(merged) => {
                    self.0.pop();
                    run = merged;
                }
                Err(_) => break,
            }
        }
        self.0.push(run);
    }
}

/// Run of repeated items in an RLE.
#[derive(Debug, Copy, Clone)]
pub struct RleRun {
    pub count: usize,
    pub item: RleItem,
}
impl RleRun {
    /// Tries to merge two runs, returning `Ok(merged)` if successful or
    /// `Err((self, other))` if they cannot be merged.
    pub fn try_merge(mut self, other: RleRun) -> std::result::Result<RleRun, (RleRun, RleRun)> {
        match (self.item, other.item) {
            // Omit empty cells at end of row/layer/pattern.
            (RleItem::Cell(0_u8), RleItem::Next(_)) => Ok(other),
            (RleItem::Cell(0_u8), RleItem::End) => Ok(other),
            // Omit end of row/layer before end of a larger layer.
            (RleItem::Next(ax1), RleItem::Next(ax2)) if ax1 < ax2 => Ok(other),
            // Omit end of row/layer before end of pattern.
            (RleItem::Next(_), RleItem::End) => Ok(other),
            // Merge identical items.
            (_, _) => {
                if self.item == other.item {
                    self.count += other.count;
                    Ok(self)
                } else {
                    Err((self, other))
                }
            }
        }
    }
    /// Returns the number of characters in the string representation.
    pub fn str_len(self) -> usize {
        // Find the smallest power of ten that is larger than the count.
        let log10_count = match self.count {
            0 => return 0,
            1 => 0,
            _ => (2..)
                .take_while(|&digits| self.count >= 10_usize.pow(digits as u32 - 1))
                .last()
                .unwrap_or(1_usize),
        };
        log10_count + self.item.str_len()
    }
}
impl From<RleItem> for RleRun {
    fn from(item: RleItem) -> Self {
        Self { count: 1, item }
    }
}
impl FromStr for RleRun {
    type Err = RleError;

    fn from_str(s: &str) -> RleResult<Self> {
        let end_of_count = s
            .find(|ch: char| !ch.is_ascii_digit())
            .ok_or(RleError::InvalidItem)?;

        let count: usize = if end_of_count != 0 {
            s[..end_of_count]
                .parse()
                .map_err(|_| RleError::InvalidCount)?
        } else {
            1
        };
        let item = s[end_of_count..].trim_start().parse()?;

        Ok(Self { count, item })
    }
}
impl fmt::Display for RleRun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.count > 1 {
            write!(f, "{}", self.count)?;
        }
        if self.count > 0 {
            write!(f, "{}", self.item)?;
        }
        Ok(())
    }
}
impl fmt::Binary for RleRun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.count > 1 {
            write!(f, "{}", self.count)?;
        }
        if self.count > 0 {
            write!(f, "{:b}", self.item)?;
        }
        Ok(())
    }
}

/// Single "content item" that may be repeated in an RLE pattern.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RleItem {
    /// Cell state.
    Cell(u8),
    /// Advance to the next row/layer along an axis.
    Next(Axis),
    /// End of the whole RLE.
    End,
}
impl RleItem {
    /// Returns the number of characters in the string representation.
    pub fn str_len(self) -> usize {
        match self {
            Self::Cell(0..=24_u8) => 1,
            Self::Cell(25..=255_u8) => 2,

            Self::Next(X) => 0,
            Self::Next(Y) => 1,
            Self::Next(Z) => 1,
            Self::Next(_) => 2,

            Self::End => 1,
        }
    }
}
impl FromStr for RleItem {
    type Err = RleError;

    fn from_str(s: &str) -> RleResult<Self> {
        // Make sure the string is ASCII so that we can use byte indexing.
        if !s.is_ascii() {
            return Err(RleError::NonAscii);
        }

        if let Some((ch,)) = s.chars().collect_tuple() {
            // One character
            match ch {
                'b' => Ok(Self::Cell(0_u8)),
                'o' => Ok(Self::Cell(1_u8)),

                // Cell states 1 to 24
                'A'..='X' => Ok(Self::Cell(char_diff('A', ch))),

                '.' => Ok(Self::Cell(0_u8)),

                '$' => Ok(Self::Next(Y)),
                '/' => Ok(Self::Next(Z)),

                '!' => Ok(Self::End),

                _ => Err(RleError::UnknownSymbol(ch)),
            }
        } else if let Some((ch1, ch2)) = s.chars().collect_tuple() {
            // Two characters
            match (ch1, ch2) {
                // Cell states 25 to 240
                ('p'..='x', 'A'..='X') => {
                    let state = char_diff('p', ch1) * 24 + char_diff('A', ch2);
                    Ok(Self::Cell(state))
                }
                // Cell states 241 to 255
                ('y', 'A'..='O') => {
                    let state = 240 + char_diff('A', ch2);
                    Ok(Self::Cell(state))
                }

                ('%', 'W') => Ok(Self::Next(W)), // Advance along W axis
                ('%', 'U') => Ok(Self::Next(U)), // Advance along U axis
                ('%', 'V') => Ok(Self::Next(V)), // Advance along V axis
                ('%', _) => Err(RleError::ExpectedWUV),

                _ => Err(RleError::UnknownSymbolPair(ch1, ch2)),
            }
        } else {
            Err(RleError::InvalidItem)
        }
    }
}
impl fmt::Display for RleItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Source: http://golly.sourceforge.net/Help/formats.html#rle
        match *self {
            Self::Cell(0_u8) => write!(f, "."),
            Self::Cell(state) => {
                if state >= 25_u8 {
                    // 25..=48   => "pA".."pX"
                    // 49..=72   => "qA".."qX"
                    // ...
                    // 241..=255 => "yA".."yO"
                    write!(f, "{}", (b'p' + (state - 1) / 24 - 1) as char)?;
                }
                // 1..=24 => 'A'..'X'
                write!(f, "{}", (b'A' + (state - 1) % 24) as char)
            }

            Self::Next(X) => Ok(()), // unused
            Self::Next(Y) => write!(f, "$"),
            Self::Next(Z) => write!(f, "/"),
            Self::Next(W) => write!(f, "%W"),
            Self::Next(U) => write!(f, "%U"),
            Self::Next(V) => write!(f, "%V"),

            Self::End => write!(f, "!"),
        }
    }
}
impl fmt::Binary for RleItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Source: http://golly.sourceforge.net/Help/formats.html#rle
        match *self {
            Self::Cell(0_u8) => write!(f, "b"),
            // All nonzero cells are encoded the same way.
            Self::Cell(_) => write!(f, "o"),

            _ => write!(f, "{}", self),
        }
    }
}

/// Compute the signed "distance" between two characters such that, for example,
/// `char_diff('A', 'A')` = 1 and `char_diff('A' -> 'Z')` = 26.
///
/// # Panics
///
/// This function panics if the Rleresult does not fit in a `char`.
fn char_diff(ch1: char, ch2: char) -> u8 {
    ch2 as u8 - ch1 as u8 + 1
}
