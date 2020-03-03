mod ast;
mod bytecode;
mod optimizer;
mod tokenizer;

use super::*;
use tokenizer::*;

type CompileResult<T> = Result<T, (Span, &'static str)>;
