// use std::collections::HashMap;

// use crate::ast;
// use crate::data::{ConstValue, LlvmValue, Value};
// use crate::errors::{Error, Result};

// type EvalResult<T> = Result<EvalOutcome<T>>;

// enum EvalOutcome<T> {
//     Proceed(T),
//     Return(Value),
// }

// pub enum WalkMode {
//     Interpret,
//     Compile,
// }

// pub struct Walker {
//     pub mode: WalkMode,
//     pub vars: HashMap<ast::VarId, Value>,
// }
// impl Walker {
//     pub fn walk_stmt(&mut self, stmt: ast::Stmt<'_>) -> EvalResult<()> {
//         match stmt.data() {
//             ast::StmtData::Block(inners) => {
//                 for &stmt_id in inners {
//                     self.walk_stmt(stmt.ast.get_node(stmt_id))?;
//                 }
//             }

//             ast::StmtData::Assign(lhs, op, rhs) => {
//                 let lhs = self.walk_expr_assign(stmt.ast.get_node(*lhs));
//                 let rhs = self.walk_expr(stmt.ast.get_node(*rhs));

//                 match op.node {
//                     ast::AssignOp::NoOp => {}
//                     ast::AssignOp::Add => {}
//                     ast::AssignOp::Sub => {}
//                     ast::AssignOp::Mul => {}
//                     ast::AssignOp::Div => {}
//                     ast::AssignOp::Mod => {}
//                     ast::AssignOp::Pow => {}
//                     ast::AssignOp::Shl => {}
//                     ast::AssignOp::ShrSigned => {}
//                     ast::AssignOp::ShrUnsigned => {}
//                     ast::AssignOp::And => {}
//                     ast::AssignOp::Or => {}
//                     ast::AssignOp::Xor => {}
//                 }
//             }

//             ast::StmtData::IfElse(_, _, _) => {}
//             ast::StmtData::Unless(_, _) => {}

//             ast::StmtData::Assert(_, _) => {}
//             ast::StmtData::Error(_) => {}

//             ast::StmtData::Break => {}
//             ast::StmtData::Continue => {}
//             ast::StmtData::ForLoop(_, _, _) => {}

//             ast::StmtData::Become(_) => {}
//             ast::StmtData::Remain => {}
//             ast::StmtData::Return(_) => {}
//         }

//         Ok(EvalOutcome::Proceed(()))
//     }

//     pub fn walk_expr_assign(
//         &mut self,
//         expr: ast::Expr<'_>,
//     ) -> EvalResult<Box<dyn FnOnce(&mut Self, Value)>> {
//         match expr.data() {
//             ast::ExprData::Paren(_) => Err(Error::cannot_assign_to(
//                 expr.span(),
//                 "expression in parentheses",
//             )),

//             ast::ExprData::Variable(var) => Ok(EvalOutcome::Proceed(Box::new(|w, value| {
//                 w.vars.insert(*var, value);
//             }))),
//             ast::ExprData::Constant(_) => Err(Error::cannot_assign_to(
//                 expr.span(),
//                 "expression in parentheses",
//             )),

//             ast::ExprData::BinaryOp(_, _, _) => {}
//             ast::ExprData::PrefixOp(_, _) => {}
//             ast::ExprData::MethodCall { obj, attr, args } => {}
//             ast::ExprData::FuncCall { func, args } => {}
//             ast::ExprData::IndexOp { obj, args } => {}

//             ast::ExprData::VectorConstruct(_) => {}
//         }
//     }

//     pub fn walk_expr(&mut self, expr: ast::Expr<'_>) -> EvalResult<Value> {
//         match expr.data() {
//             ast::ExprData::Paren(_) => {}

//             ast::ExprData::Variable(_) => {}
//             ast::ExprData::Constant(_) => {}

//             ast::ExprData::BinaryOp(_, _, _) => {}
//             ast::ExprData::PrefixOp(_, _) => {}
//             ast::ExprData::MethodCall { obj, attr, args } => {}
//             ast::ExprData::FuncCall { func, args } => {}
//             ast::ExprData::IndexOp { obj, args } => {}

//             ast::ExprData::VectorConstruct(_) => {}
//         }
//     }
// }
