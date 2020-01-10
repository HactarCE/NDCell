//! Code for reading and writing Golly's 2D RLE format, described here:
//! http://golly.sourceforge.net/Help/formats.html#rle

use super::*;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidCellState,
    CellStateOutOfRange,
}

pub trait RLECellType: CellType {
    fn push_to_string(self, chars: &mut String);
    fn from_str(s: &str) -> ParseResult<Self>;
}

impl RLECellType for bool {
    fn push_to_string(self, s: &mut String) {
        s.push(if self { 'o' } else { 'b' });
    }
    fn from_str(s: &str) -> ParseResult<Self> {
        RLECellType::from_str(s).and_then(|n| match n {
            0u8 => Ok(false),
            1u8 => Ok(true),
            _ => Err(ParseError::CellStateOutOfRange),
        })
    }
}

impl RLECellType for u8 {
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
            RLECellType::push_to_string(i, &mut s);
            println!("{:?}", s);
            assert_eq!(Ok(i), RLECellType::from_str(&s));
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
        RLECellType::push_to_string(false, &mut s);
        assert_eq!(Ok(false), RLECellType::from_str(&s));
        assert_eq!("b", s);
        // Check true
        let mut s = String::new();
        RLECellType::push_to_string(true, &mut s);
        assert_eq!(Ok(true), RLECellType::from_str(&s));
        assert_eq!("o", s);
    }

    /// Tests that the cell state reader fails correctly and does not panic.
    #[test]
    fn test_rle_cell_state_fail() {
        const CHAR_SET: &str = &"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ._";
        for ch1 in CHAR_SET.chars() {
            let s = &ch1.to_string();
            let bool_state: ParseResult<bool> = RLECellType::from_str(s);
            let u8_state: ParseResult<u8> = RLECellType::from_str(s);
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
                        RLECellType::push_to_string(n, &mut test_s);
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
