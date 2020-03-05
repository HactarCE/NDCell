use std::fmt;

use super::*;

#[derive(Debug)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub instruction_spans: Vec<Span>,
    pub vars: VarMapping,
    pub return_type: Type,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Function [")?;
        writeln!(f, "  Variables [")?;
        for id in 0..self.vars.len() {
            writeln!(
                f,
                "{:<7} {:<15?} {}",
                id,
                self.vars.get_type(id),
                self.vars.get_name(id)
            )?;
        }
        writeln!(f, "  ]")?;
        writeln!(f, "  Instructions [")?;
        for (instruction, span) in self.instructions.iter().zip(&self.instruction_spans) {
            writeln!(f, "    {:<6}  {:?}", span.end, instruction)?;
        }
        writeln!(f, "  ]")?;
        write!(f, "]")?;
        Ok(())
    }
}

impl From<Context> for Function {
    fn from(ctx: Context) -> Self {
        // Split the spans from the instructions so to improve cache locality
        // for instructions.
        let (instruction_spans, instructions) = ctx
            .instructions
            .into_iter()
            // Convert each Spanned<Instruction> into (Span, Instruction).
            .map(Spanned::into_tuple)
            // Collect the (Span, Instruction) iterator into a Vec<Span> and a
            // Vec<Instruction>.
            .unzip();
        Self {
            instructions,
            instruction_spans,
            vars: ctx.vars,
            return_type: ctx.return_type.unwrap_or(Type::Void),
        }
    }
}

pub fn compile_transition_function(block: &ast::Block) -> LangResult<Function> {
    compile_function(
        block,
        FunctionType::TransitionFunction,
        Some(Type::CellState),
    )
}

pub fn compile_helper_function(
    block: &ast::Block,
    return_type: Option<Type>,
) -> LangResult<Function> {
    compile_function(block, FunctionType::HelperFunction, return_type)
}

fn compile_function(
    block: &ast::Block,
    fn_type: FunctionType,
    expected_return_type: Option<Type>,
) -> LangResult<Function> {
    let mut ctx = Context::new(fn_type);
    ctx.return_type = expected_return_type;
    block.make_bytecode(&mut ctx)?;
    Ok(ctx.into())
}

pub fn compile_expr(
    expr: &Spanned<ast::Expr>,
    expected_return_type: Option<Type>,
) -> LangResult<Function> {
    let mut ctx = Context::new(FunctionType::HelperFunction);
    ctx.return_type = expected_return_type;
    ctx.return_expr(expr)?;
    Ok(ctx.into())
}
