mod function;
mod instructions;

use super::*;
pub use function::*;
pub use instructions::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum FunctionType {
    HelperFunction,
    TransitionFunction,
    StatesFunction,
}

struct Context {
    fn_type: FunctionType,
    instructions: Vec<Spanned<Instruction>>,
    vars: VarMapping,
    return_type: Option<Type>,
}
impl Context {
    fn new(fn_type: FunctionType) -> Self {
        Self {
            fn_type,
            instructions: vec![],
            vars: VarMapping::new(),
            return_type: match fn_type {
                FunctionType::TransitionFunction => Some(Type::CellState),
                FunctionType::StatesFunction => Some(Type::Void),
                _ => None,
            },
        }
    }
    fn return_expr(&mut self, expr: &Spanned<ast::Expr>) -> LangResult<Type> {
        let new_return_type = expr.make_bytecode(self)?;
        match self.return_type {
            None => self.return_type = Some(new_return_type),
            Some(ty) if ty != new_return_type => lang_err(
                expr,
                format!(
                    "Type error: expected {} return type, not {}",
                    ty, new_return_type
                ),
            )?,
            _ => (),
        }
        self.instructions.push(expr.replace(Instruction::Return));
        Ok(Type::Void)
    }
}

trait MakeBytecode {
    #[must_use]
    fn make_bytecode(&self, ctx: &mut Context) -> LangResult<Type>;
}

impl MakeBytecode for ast::Block {
    fn make_bytecode(&self, ctx: &mut Context) -> LangResult<Type> {
        for statement in self {
            statement.make_bytecode(ctx)?;
        }
        Ok(Type::Void)
    }
}

impl MakeBytecode for Spanned<ast::Statement> {
    fn make_bytecode(&self, ctx: &mut Context) -> LangResult<Type> {
        match &self.inner {
            ast::Statement::Become(expr) => {
                if ctx.fn_type == FunctionType::TransitionFunction {
                    assert_eq!(ctx.return_type, Some(Type::CellState));
                    match expr.make_bytecode(ctx)? {
                        Type::Int => lang_err(
                            expr,
                            "Type error: use '#' to convert an integer to a cell state",
                        ),
                        Type::CellState => {
                            ctx.instructions.push(self.replace(Instruction::Return));
                            Ok(Type::Void)
                        }
                        other_type => lang_err(
                            expr,
                            format!(
                                "Type error: 'become' requires a cell state, not {}",
                                other_type
                            ),
                        ),
                    }
                } else {
                    lang_err(self, "'become' is only allowed in the transition function")
                }
            }
            ast::Statement::Return(expr) => {
                if ctx.fn_type == FunctionType::TransitionFunction {
                    lang_err(
                        self,
                        "Use 'become' to return a value from the transition function",
                    )
                } else {
                    ctx.return_expr(expr)
                }
            }
        }
    }
}

impl MakeBytecode for Spanned<ast::Expr> {
    fn make_bytecode(&self, ctx: &mut Context) -> LangResult<Type> {
        match &self.inner {
            ast::Expr::Int(i) => {
                ctx.instructions
                    .push(self.replace(Instruction::PushInt(*i)));
                Ok(Type::Int)
            }

            ast::Expr::Tag(expr) => match expr.make_bytecode(ctx)? {
                Type::Int => {
                    ctx.instructions
                        .push(self.replace(Instruction::GetStateFromInt));
                    Ok(Type::CellState)
                }
                other_type => lang_err(
                    &**expr,
                    format!(
                        "Type error: unary '#' requires an integer, not {}",
                        other_type
                    ),
                ),
            },

            ast::Expr::Neg(expr) => match expr.make_bytecode(ctx)? {
                Type::Int => {
                    ctx.instructions.push(self.replace(Instruction::NegInt));
                    Ok(Type::Int)
                }
                other_type => lang_err(
                    &**expr,
                    format!(
                        "Type error: unary '-' requires an integer, not {}",
                        other_type
                    ),
                ),
            },

            ast::Expr::Add(expr1, expr2) | ast::Expr::Sub(expr1, expr2) => {
                let ty1 = expr1.make_bytecode(ctx)?;
                let ty2 = expr2.make_bytecode(ctx)?;
                let op_str = match &self.inner {
                    ast::Expr::Add(_, _) => "+",
                    ast::Expr::Sub(_, _) => "-",
                    _ => unreachable!(),
                };
                match (op_str, ty1, ty2) {
                    ("+", Type::Int, Type::Int) => {
                        ctx.instructions.push(self.replace(Instruction::AddInt));
                        Ok(Type::Int)
                    }
                    ("-", Type::Int, Type::Int) => {
                        ctx.instructions.push(self.replace(Instruction::SubInt));
                        Ok(Type::Int)
                    }
                    (op_str, other_ty1, other_ty2) => lang_err(
                        self,
                        format!(
                            "Type error: '{}' requires two integers, not {} and {}",
                            op_str, other_ty1, other_ty2
                        ),
                    ),
                }
            }

            ast::Expr::Var(name) => {
                if ctx.vars.is_registered(name) {
                    let id = ctx.vars[name.as_ref()];
                    ctx.instructions
                        .push(self.replace(Instruction::VarFetch(id)));
                    Ok(ctx.vars.get_type(id))
                } else {
                    lang_err(
                        self,
                        format!(
                            "Uninitialized variable: Variable '{}' must be initialized before is is used in an expression",
                            name
                        ),
                    )
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bytecode() {
        let source_code = "
// @states [ #dead, #live ]
@transition {
    become #(1 - (2 + -3) + 12)
}
";

        let ast = ast::make_ast(source_code).expect("AST generation failed");
        let transition_function_block = if let ast::Directive::Transition(block) = &ast[0].inner {
            block
        } else {
            panic!("Could not find transition function");
        };
        let transition_function =
            compile_transition_function(&transition_function_block).expect("Compilation failed");

        println!("{}", transition_function);

        assert_eq!(
            "Function [
  Variables [
  ]
  Instructions [
    57      PushInt(1)
    62      PushInt(2)
    67      PushInt(3)
    67      NegInt
    68      AddInt
    68      SubInt
    73      PushInt(12)
    74      AddInt
    74      GetStateFromInt
    74      Return
  ]
]",
            transition_function.to_string()
        );
    }
}
