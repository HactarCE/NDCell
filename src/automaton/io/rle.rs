//! Code for reading and writing Golly's 2D RLE format, described here:
//! http://golly.sourceforge.net/Help/formats.html#rle

use pest::Parser;

use super::*;

#[derive(Parser)]
#[grammar = "automaton/io/rle.pest"]
struct Grammar;

struct RleHeader {
    x: isize,
    y: isize,
    rule: Option<String>,
}
struct CxrleHeader {
    pos: Vec2D,
    gen: isize,
}
enum RleItem<C> {
    Cell(C),
    EndRow,
}

type TokenPair<'a> = pest::iterators::Pair<'a, Rule>;

pub trait RleEncode: std::marker::Sized {
    fn to_rle(&self) -> String;
    fn from_rle(s: &str) -> Result<Self, String>;
}
impl RleEncode for NdAutomaton<Dim2D> {
    fn to_rle(&self) -> String {
        unimplemented!()
    }
    fn from_rle(s: &str) -> Result<Self, String> {
        let mut header: Option<RleHeader> = None;
        let mut cxrle: Option<CxrleHeader> = None;
        let mut notes: Vec<String> = vec![];
        let mut cell_array: Vec<Vec<u8>> = vec![vec![]];

        let main = Grammar::parse(Rule::main, s)
            .map_err(|e| e.to_string())?
            .next()
            .ok_or("No main token pair".to_owned())?;
        for pair in main.into_inner() {
            match pair.as_rule() {
                Rule::EOI => (),
                Rule::header => {
                    if header.is_some() {
                        Err("Multiple RLE headers")?;
                    } else {
                        header = Some(parse_header(pair)?);
                    }
                }
                Rule::notes => {
                    let inner = pair.into_inner().next().ok_or("Invalid note")?;
                    match inner.as_rule() {
                        Rule::comment => notes.push(inner.as_str().to_string()),
                        Rule::cxrle => {
                            if cxrle.is_some() {
                                Err("Multiple CXRLE headers")?
                            } else {
                                cxrle = Some(parse_cxrle(inner)?);
                            }
                        }
                        _ => (),
                    }
                }
                Rule::content => {
                    for content_item in pair.into_inner() {
                        if content_item.as_rule() == Rule::content_item {
                            let (n, state): (usize, RleItem<u8>) =
                                parse_content_item(content_item)?;
                            for _ in 0..n {
                                match state {
                                    RleItem::Cell(cell_state) => {
                                        cell_array.last_mut().unwrap().push(cell_state)
                                    }
                                    RleItem::EndRow => cell_array.push(vec![]),
                                }
                            }
                        }
                    }
                }
                _ => Err("Unexpected token pair")?,
            }
        }
        let _header = header.ok_or("Missing RLE header")?;

        let mut ret = NdAutomaton::default();
        let mut pos = Vec2D::origin();
        let x_start: isize;

        if let Some(cxrle) = cxrle {
            ret.generations = cxrle.gen as usize;
            pos = cxrle.pos;
            x_start = *pos.x();
        } else {
            x_start = 0;
        }

        for row in cell_array {
            for cell in row {
                ret.tree.set_cell(pos, cell);
                *pos.x_mut() += 1;
            }
            pos = Vec2D::from([x_start, pos.y() + 1])
        }

        Ok(ret)
    }
}

pub trait CxrleEncode: RleEncode {
    fn to_cxrle(&self) -> String;
}
impl CxrleEncode for NdAutomaton<Dim2D> {
    fn to_cxrle(&self) -> String {
        unimplemented!()
    }
}

fn parse_header(pair: TokenPair) -> Result<RleHeader, String> {
    let mut inners = pair.into_inner();
    let x: isize = inners
        .next()
        .ok_or("No X value in RLE header")?
        .as_str()
        .parse()
        .map_err(|_| "Could not parse RLE X value as integer")?;
    let y: isize = inners
        .next()
        .ok_or("No Y value in RLE header")?
        .as_str()
        .parse()
        .map_err(|_| "Could not parse RLE Y value as integer")?;
    let rule: Option<String> = inners.next().map(|inner| inner.as_str().to_owned());
    Ok(RleHeader { x, y, rule })
}

fn parse_cxrle(pair: TokenPair) -> Result<CxrleHeader, String> {
    let mut pos: Vec2D = Vec2D::origin();
    let mut gen: isize = 0;
    for kv_pair in pair.into_inner() {
        let mut inners = kv_pair.into_inner();
        let k = inners.next().ok_or("Invalid CXRLE key")?.as_str();
        let v = inners.next().ok_or("Invalid CXRLE value")?.as_str();
        match k {
            "Pos" => {
                let mut split_iter = v.split(',');
                let x: isize = split_iter
                    .next()
                    .and_then(|s| s.parse().ok())
                    .ok_or("Invalid CXRLE Pos")?;
                let y: isize = split_iter
                    .next()
                    .and_then(|s| s.parse().ok())
                    .ok_or("Invalid CXRLE Pos")?;
                if let Some(_) = split_iter.next() {
                    Err("Invalid CXRLE Pos")?
                }
                pos = Vec2D::from([x, y]);
            }
            "Gen" => gen = v.parse().ok().ok_or("Invalid CXRLE Gen")?,
            _ => Err("Unknown CXRLE string")?,
        }
    }
    Ok(CxrleHeader { pos, gen })
}

fn parse_content_item<C: RleCellType>(pair: TokenPair) -> Result<(usize, RleItem<C>), String> {
    let mut n: usize = 1;
    let mut item: Option<RleItem<C>> = None;
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::int => n = inner.as_str().parse().map_err(|_| "Invalid RLE content")?,
            Rule::end_row => item = Some(RleItem::EndRow),
            Rule::state => {
                item = Some(RleItem::Cell(
                    RleCellType::from_str(inner.as_str()).map_err(|_| "Cell state out of range")?,
                ))
            }
            _ => Err("Invalid RLE content")?,
        }
    }
    Ok((n, item.ok_or("Invalid RLE content")?))
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidCellState,
    CellStateOutOfRange,
}

pub trait RleCellType: CellType {
    fn push_to_string(self, chars: &mut String);
    fn from_str(s: &str) -> ParseResult<Self>;
}

impl RleCellType for bool {
    fn push_to_string(self, s: &mut String) {
        s.push(if self { 'o' } else { 'b' });
    }
    fn from_str(s: &str) -> ParseResult<Self> {
        RleCellType::from_str(s).and_then(|n| match n {
            0u8 => Ok(false),
            1u8 => Ok(true),
            _ => Err(ParseError::CellStateOutOfRange),
        })
    }
}

impl RleCellType for u8 {
    fn push_to_string(self, s: &mut String) {
        if self == 0 {
            s.push('.');
        } else {
            if self >= 25 {
                s.push(('p' as u8 + (self - 1) / 24 - 1) as char);
            }
            s.push(('A' as u8 + (self - 1) % 24) as char);
        }
    }
    fn from_str(s: &str) -> ParseResult<Self> {
        let mut chars = s.chars();
        let ch1 = chars.next().ok_or(ParseError::InvalidCellState)?;
        match ch1 {
            'b' | '.' => Some(0),
            'o' => Some(1),
            'A'..='X' => Some(char_diff('A', ch1)),
            'p'..='y' => {
                let value1 = char_diff('p', ch1) * 24;
                let ch2 = chars.next();
                match ch2 {
                    Some('A'..='X') => value1.checked_add(char_diff('A', ch2.unwrap())),
                    _ => None,
                }
            }
            _ => None,
        }
        .and_then(|n| match n {
            0..=24 if s.len() == 1 => Some(n),
            25..=255 if s.len() == 2 => Some(n),
            _ => None,
        })
        .ok_or(ParseError::InvalidCellState)
    }
}

fn char_diff(ch1: char, ch2: char) -> u8 {
    ch2 as u8 - ch1 as u8 + 1
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests that we can read and write all 256 cell states in RLE format.
    #[test]
    fn test_rle_cell_states() {
        // Ensure that u8->string and string->u8 is reversible
        for i in 0..=255 {
            let mut s = String::new();
            RleCellType::push_to_string(i, &mut s);
            println!("{:?}", s);
            assert_eq!(Ok(i), RleCellType::from_str(&s));
            // Spot-check a few examples.
            match i {
                0 => assert_eq!(".", s),
                1 => assert_eq!("A", s),
                2 => assert_eq!("B", s),
                24 => assert_eq!("X", s),
                25 => assert_eq!("pA", s),
                240 => assert_eq!("xX", s),
                241 => assert_eq!("yA", s),
                255 => assert_eq!("yO", s),
                _ => (),
            }
        }
        // Check false
        let mut s = String::new();
        RleCellType::push_to_string(false, &mut s);
        assert_eq!(Ok(false), RleCellType::from_str(&s));
        assert_eq!("b", s);
        // Check true
        let mut s = String::new();
        RleCellType::push_to_string(true, &mut s);
        assert_eq!(Ok(true), RleCellType::from_str(&s));
        assert_eq!("o", s);
    }

    /// Tests that the cell state reader fails correctly and does not panic.
    #[test]
    fn test_rle_cell_state_fail() {
        const CHAR_SET: &str = &"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ._";
        for ch1 in format!(" {}", CHAR_SET).chars() {
            for ch2 in CHAR_SET.chars() {
                let s = &format!("{}{}", ch1, ch2);
                let bool_state: ParseResult<bool> = RleCellType::from_str(s);
                let u8_state: ParseResult<u8> = RleCellType::from_str(s);
                if let Ok(n) = u8_state {
                    match n {
                        0 => assert_eq!(Ok(false), bool_state),
                        1 => assert_eq!(Ok(true), bool_state),
                        _ => assert_eq!(Err(ParseError::CellStateOutOfRange), bool_state),
                    }
                    match s.as_ref() {
                        "o" | "b" => (),
                        _ => {
                            let mut test_s = String::new();
                            RleCellType::push_to_string(n, &mut test_s);
                            assert_eq!(s, &test_s);
                        }
                    }
                } else {
                    assert_eq!(Err(ParseError::InvalidCellState), bool_state);
                    assert_eq!(Err(ParseError::InvalidCellState), u8_state);
                }
            }
        }
    }

    // Load and save a glider.
    #[test]
    fn test_basic_rle() {
        let result: NdAutomaton<Dim2D> = RleEncode::from_rle(
            "
#CXRLE Pos=10,-15
# Comment
# Comment 2
x = 3, y = 3, rule = Life
# more
bo$2b
o$3o!

#Another Comment 3
#Comment 4
",
        )
        .unwrap();
        assert_eq!(5, result.tree.get_root().population);
        assert_eq!(1, result.tree.get_cell([11, -15].into()));
        assert_eq!(1, result.tree.get_cell([12, -14].into()));
        assert_eq!(1, result.tree.get_cell([10, -13].into()));
        assert_eq!(1, result.tree.get_cell([11, -13].into()));
        assert_eq!(1, result.tree.get_cell([12, -13].into()));
    }
}
