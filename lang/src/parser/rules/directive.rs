use super::{Expression, List, Parser, StatementBlock, SyntaxRule};
use crate::ast;
use crate::errors::{Error, Result};
use crate::lexer::Token;

/// Matches a directive.
#[derive(Debug, Copy, Clone)]
pub struct Directive;
impl_display!(for Directive, "directive, such as '@ndim 2'");
impl SyntaxRule for Directive {
    type Output = ast::DirectiveId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Ident) && p.string().starts_with('@')
    }

    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            if !(p.next() == Some(Token::Ident) && p.string().starts_with('@')) {
                return p.expected(self);
            }
            match p.string() {
                "@compile" => Ok(ast::DirectiveData::Compile {
                    param_types: p.parse(ast, List::bracket_comma_sep(TypeExpression))?,
                    body: p.parse(ast, StatementBlock)?,
                }),

                "@init" => Ok(ast::DirectiveData::Init(p.parse(ast, StatementBlock)?)),
                "@func" => Err(Error::unimplemented(p.span())),
                // "@func" => Ok(ast::DirectiveData::Function(p.parse(ast, FunctionDefinition)?)),

                // "@rule" => Ok(ast::DirectiveData::Rule(p.parse(ast, StringLiteral)?)),
                "@rule" => Err(Error::unimplemented(p.span())),
                "@ndim" => Ok(ast::DirectiveData::Ndim(p.parse(ast, Expression)?)),
                "@states" => Ok(ast::DirectiveData::States(p.parse(ast, Expression)?)),
                // "@transition" => Ok(ast::DirectiveData::Transition(
                //     p.parse(ast, StatementBlock)?,
                // )),
                "@transition" => Err(Error::unimplemented(p.span())),

                s => Err(match s.to_ascii_lowercase().as_str() {
                    "@init" | "@initialize" | "@initialization" | "@pre" | "@setup" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@init")
                    }
                    "@con" | "@const" | "@constant" | "@define" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@const")
                    }
                    "@def" | "@fn" | "@fun" | "@func" | "@function" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@func")
                    }

                    "@name" | "@rule" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@rule")
                    }
                    "@dim" | "@dimension" | "@dimensions" | "@ndim" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@ndim")
                    }
                    "@state" | "@states" | "@nstates" | "@n_states" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@states")
                    }
                    "@table" | "@tran" | "@trans" | "@transition" | "@transitions" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@transition")
                    }

                    _ => Error::invalid_directive_name(p.span()),
                }),
            }
        })
    }
}

// /// Matches a function definition.
// #[derive(Debug, Copy, Clone)]
// pub struct FunctionDefinition;
// impl_display!(
//     for FunctionDefinition,
//     "function definition, such as 'add(a: Integer, b: Integer) -> Integer'",
// );
// impl SyntaxRule for FunctionDefinition {
//     type Output = ast::FuncId;

//     fn might_match(&self, _tf: Parser<'_>) -> bool {
//         // It's better to give an error saying "expected identifier" with
//         // examples of valid type names than just "expected function definition"
//         // if the user gives a bad return type.
//         true
//     }
//     fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
//         p.parse_and_add_ast_node(ast, |p, ast| {
//             let name = p
//                 .parse(ast, Identifier)
//                 .or_else(|_| p.expected("function name"))?;
//             ast.in_new_scope(|mut ast| {
//                 let params = p.parse(ast, List::paren_comma_sep(FunctionParameter))?.node;
//                 p.parse(ast, Token::Arrow)?;
//                 let ret_type_expr = p.parse(ast, Expression)?;
//                 let ret_type = todo!("const eval type expr");
//                 let body = p.parse(ast, StatementBlock)?;

//                 Ok(ast::FuncData {
//                     name,
//                     params,
//                     ret_type,
//                     body,
//                 })
//             })
//         })
//     }
// }

// /// Matches a function parameter (name followed by type annotation).
// #[derive(Debug, Copy, Clone)]
// pub struct FunctionParameter;
// impl_display!(
//     for FunctionParameter,
//     "function parameter, such as 'arg_name: Integer'",
// );
// impl SyntaxRule for FunctionParameter {
//     type Output = ast::VarId;

//     fn might_match(&self, p: Parser<'_>) -> bool {
//         Identifier.might_match(p)
//     }
//     fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
//         let name = p.parse(ast, Identifier)?;
//         if p.next() != Some(Token::Colon) {
//             return p.expected("type annotation, such as ': Integer'")?;
//         }
//         let type_expr = p.parse(ast, TypeExpression)?;
//         Ok(ast.add_scoped_var(name, Some(type_expr), None, None))
//     }
// }

/// Matches an expression that evaluates to a type.
#[derive(Debug, Copy, Clone)]
pub struct TypeExpression;
impl_display!(for TypeExpression, "type, such as 'Integer' or 'Vector[3]'",);
impl SyntaxRule for TypeExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Expression.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        Expression.consume_match(p, ast)
    }
}
