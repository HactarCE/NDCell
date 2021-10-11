//! Parser for string sublanguages.

use codemap::Span;

use crate::data::LangCell;
use crate::errors::{Error, Result};
use crate::exec::CtxTrait;

/// Parses the contents of a mask string.
pub fn parse_mask_string(span: Span, s: &str) -> Result<Vec<bool>> {
    s.chars()
        .filter(|ch| !ch.is_whitespace())
        .map(|ch| match ch {
            '.' => Ok(false),
            '#' => Ok(true),
            _ => Err(Error::invalid_mask_symbol(span, ch)),
        })
        .collect()
}

/// Parses the contents of a cell array string.
pub fn parse_cell_array_string(
    ctx: &mut impl CtxTrait,
    span: Span,
    s: &str,
) -> Result<Vec<LangCell>> {
    let state_count = ctx.get_states(span)?;

    s.chars()
        .filter(|ch| !ch.is_whitespace())
        .map(|ch| cell_from_token(span, &ch.to_string(), state_count))
        .collect::<Result<_>>()
}

fn cell_from_token(span: Span, s: &str, state_count: usize) -> Result<LangCell> {
    match s {
        "." => Some(0),
        "#" => Some(1),
        _ => s.parse::<LangCell>().ok(),
    }
    .filter(|&c| (c as usize) < state_count)
    .ok_or_else(|| Error::invalid_cell_symbol(span, s))
}
