use itertools::Itertools;
use std::fmt;
use std::str::FromStr;

use super::*;
use crate::axis::AXES;
use crate::ndtree::Layer;
use crate::ndvec::BigVec6D;
use crate::num::{BigInt, Zero};

/// Macrocell contents.
#[derive(Debug, Clone)]
pub struct Macrocell {
    /// Automaton rule.
    pub(super) rule: Option<String>,
    /// Number of generations simulated.
    pub(super) gen: BigInt,
    /// Position of center of pattern.
    ///
    /// This value is using NDCell coordinates, with Y increasing upwards.
    pub(super) offset: BigVec6D,
    /// ND-tree nodes.
    pub(super) nodes: Vec<MacrocellNode>,
    /// Pattern comments, not including offset, generation count, etc.
    pub(super) comments: String,
}
impl Macrocell {
    /// Returns the name of the rule.
    pub fn rule(&self) -> Option<&str> {
        self.rule.as_deref()
    }
    /// Sets the rule name.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn with_rule(mut self, rule: Option<impl ToString>) -> Self {
        self.rule = rule.map(|r| r.to_string());
        self
    }

    /// Returns the number of generations.
    pub fn generation(&self) -> BigInt {
        self.gen.clone()
    }
    /// Sets the number of generations, adding a CXRLE header if necessary.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn with_generation(mut self, generation: BigInt) -> Self {
        self.gen = generation;
        self
    }
}
impl fmt::Display for Macrocell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Header
        writeln!(f, "{}", crate::MACROCELL_HEADER)?;
        // Rule name (optional)
        if let Some(rule) = &self.rule {
            writeln!(f, "#R {}", rule)?;
        }
        // Generation count (optional)
        if !self.gen.is_zero() {
            writeln!(f, "#G {}", self.gen)?;
        }
        // Offset (optional) - Use the largest nonzero axis to determine the number of
        // dimensions to include in the offset.
        if let Some(ndim) = AXES
            .iter()
            .rev()
            .find(|&&ax| !self.offset[ax].is_zero())
            .map(|&ax| ax as usize + 1)
        {
            write!(f, "#O")?;
            for &ax in crate::axis::ndim_axes(ndim) {
                write!(f, " {}", self.offset[ax])?;
            }
            writeln!(f)?;
        }
        // Comments
        for line in self.comments.lines() {
            let needs_prefix =
                // '#' is required at beginning of comments
                !line.starts_with('#')
                // '#R' indicates the rule name (not a valid comment)
                || line.starts_with("#R")
                // '#G' indicates the generation count (not a valid comment)
                || line.starts_with("#G")
                // '#O' indicates the offset (not a valid comment)
                || line.starts_with("#O");
            if needs_prefix {
                write!(f, "# ")?;
            }
            writeln!(f, "{}", line)?;
        }
        // Nodes/contents
        for node in &self.nodes {
            match node {
                MacrocellNode::Empty => (),
                MacrocellNode::Leaf8x8 { .. } => {
                    writeln!(f, "Error: cannot generate 8x8 leaf node")?
                }
                MacrocellNode::NonLeaf { .. } => writeln!(f, "{}", node)?,
            }
        }
        Ok(())
    }
}
impl FromStr for Macrocell {
    type Err = MacrocellError;

    fn from_str(s: &str) -> MacrocellResult<Self> {
        let mut lines = s.trim().lines().map(|s| s.trim());
        {
            // Check header.
            let header_line = lines.next().unwrap_or("");
            if !header_line.starts_with("[M2]") {
                return Err(MacrocellError::MissingHeader);
            }
        }

        let mut rule: Option<String> = None;
        let mut gen = BigInt::zero();
        let mut offset = BigVec6D::origin();
        let mut nodes = vec![MacrocellNode::Empty];
        let mut comments = String::new();

        for line in lines {
            if line.is_empty() {
                continue;
            } else if let Some(rest) = line.strip_prefix("#R") {
                rule = Some(rest.trim().to_owned());
            } else if let Some(rest) = line.strip_prefix("#G") {
                gen = rest.trim().parse().unwrap_or(gen);
            } else if line.starts_with("#O") {
                let values_iter = line.split_ascii_whitespace().filter_map(|s| s.parse().ok());
                for (&ax, axis_offset) in AXES.iter().zip(values_iter) {
                    offset[ax] = axis_offset;
                }
            } else if line.starts_with('#') {
                comments.push_str(line);
                comments.push('\n');
            } else {
                nodes.push(line.parse()?);
            }
        }

        Ok(Macrocell {
            rule,
            gen,
            offset,
            nodes,
            comments,
        })
    }
}

#[derive(Debug, Clone)]
pub enum MacrocellNode {
    Empty,
    Leaf8x8 {
        bits: [u8; 8],
    },
    NonLeaf {
        layer: Layer,
        children: Box<[usize]>,
    },
}
impl FromStr for MacrocellNode {
    type Err = MacrocellError;

    fn from_str(s: &str) -> MacrocellResult<Self> {
        use MacrocellError::InvalidContent;

        let first_char = s.chars().next().ok_or(InvalidContent)?;
        match first_char {
            '1'..='9' => {
                let mut num_strs = s.split_ascii_whitespace();
                // First number is layer.
                let layer = num_strs
                    .next()
                    .ok_or(InvalidContent)?
                    .parse()
                    .map(Layer)
                    .map_err(|_| InvalidContent)?;
                // Remaining numbers are indices of children (or cell states, if
                // layer = 1).
                let children = num_strs
                    .map(|n| n.parse())
                    .collect::<Result<Vec<usize>, _>>()
                    .map_err(|_| InvalidContent)?
                    .into_boxed_slice();
                Ok(Self::NonLeaf { layer, children })
            }

            '.' | '*' | '$' => {
                let mut bits = [0_u8; 8];

                let mut y = 0;
                let mut x = 0;

                for ch in s.chars() {
                    if x == 8 || y == 8 {
                        // End of row or square; skip this cell.
                        continue;
                    }

                    if ch == '*' {
                        // Set this cell.
                        bits[y] |= 1 << x;
                    }
                    if ch == '.' || ch == '*' {
                        // Advance to the next cell.
                        x += 1;
                    }
                    if ch == '$' {
                        // Advance to the next row.
                        x = 0;
                        y += 1;
                    }
                }

                Ok(Self::Leaf8x8 { bits })
            }

            _ => Err(InvalidContent),
        }
    }
}
impl fmt::Display for MacrocellNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty | Self::Leaf8x8 { .. } => Err(fmt::Error),
            Self::NonLeaf { layer, children } => {
                write!(f, "{} {}", layer.to_u32(), children.iter().join(" "))
            }
        }
    }
}
