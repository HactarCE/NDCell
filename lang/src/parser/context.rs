// use codemap::{File, Span, Spanned};
// use std::collections::HashMap;

// use super::Parser;
// use crate::ast;
// use crate::lexer::Token;
// use crate::Error;

// /// Parse context.
// #[derive(Debug)]
// pub struct Ctx<'ctx> {
//     /// Program being built.
//     program: &'ctx mut ast::Program,

//     /// Key of directive currently being built.
//     directive_key: Option<ast::DirectiveKey>,

//     /// Table of local variables.
//     locals: HashMap<String, ast::VarId>,
//     /// Stack of scopes. Each scope is a list of local variables that have been
//     /// added or overwritten by this scope, each accompanied by the overwritten
//     /// value (or `None` if it did not exist before).
//     ///
//     /// This does not include the global scope.
//     scope_stack: Vec<Vec<(String, Option<ast::VarId>)>>,
// }
// impl<'ctx> Ctx<'ctx> {
//     pub fn new(program: &'ctx mut ast::Program, directive_key: Option<ast::DirectiveKey>) -> Self {
//         Self {
//             program,
//             directive_key,
//             locals: HashMap::new(),
//             scope_stack: vec![],
//         }
//     }
//     pub fn parser<'b>(
//         &'b mut self,
//         file: &'b File,
//         tokens: &'b [Spanned<Token>],
//     ) -> ast::Fallible<Parser> {
//         match Parser::new(file, tokens) {
//             Ok(ok) => Ok(ok),
//             Err(e) => {
//                 self.program.compile_error(e);
//                 Err(ast::AlreadyReported)
//             }
//         }
//     }

//     pub fn in_new_scope<'b, T>(&'b mut self, f: impl 'b + FnOnce(&mut Self) -> T) -> T {
//         self.scope_stack.push(vec![]);
//         let ret = f(self);
//         for (k, old_value) in self.scope_stack.pop().unwrap().into_iter().rev() {
//             match old_value {
//                 Some(v) => self.locals.insert(k, v),
//                 None => self.locals.remove(&k),
//             };
//         }
//         ret
//     }
//     pub fn dir_key(&self) -> Option<&ast::DirectiveKey> {
//         self.directive_key.as_ref()
//     }
//     pub fn set_dir_key(&mut self, directive_key: Option<ast::DirectiveKey>) {
//         self.directive_key = directive_key;
//     }

//     pub fn add_node<D>(&mut self, span: Span, data: D) -> ast::NodeId<ast::Node<D>>
//     where
//         ast::Node<D>: ast::NodeTrait,
//     {
//         self.program.add_node(span, data)
//     }
//     pub fn get_node<'a, N>(&'a mut self, id: ast::NodeId<N>) -> ast::AstNode<'a, N>
//     where
//         N: ast::NodeTrait,
//     {
//         self.program.get_node(id)
//     }

//     #[must_use]
//     pub fn paren_expr(&self, expr: ast::ExprId) -> ast::ExprData {
//         ast::ExprData::Paren(expr)
//     }

//     pub fn add_scoped_var(
//         &mut self,
//         name: Spanned<String>,
//         type_expr: Option<ast::ExprId>,
//         value_expr: Option<ast::ExprId>,
//         iter_expr: Option<ast::ExprId>,
//     ) -> ast::VarId {
//         // TODO: evaluate type expression
//         // TODO: determine type of expression
//         // TODO: determine iter type of expression
//         let span = name.span;
//         let name = name.node;
//         let var_id = self.add_node(
//             span,
//             ast::VarData {
//                 name: name.clone(),
//                 ty: todo!("determine type of variable"),
//             },
//         );

//         let old_value = self.locals.insert(name.clone(), var_id);
//         if let Some(scope) = self.scope_stack.last() {
//             scope.push((name, old_value));
//             if old_value.is_some() {
//                 self.program.compile_error(Error::name_in_use(span));
//             }
//         } else {
//             // We are at the global scope; don't worry about restoring the
//             // symbol table to the way it was before the global scope.
//         }

//         var_id
//     }

//     pub fn err<T>(&mut self, error: Error) -> ast::Fallible<T> {
//         // TODO: when #[feature(never_type)] stabalizes, use that here and
//         // return `ast::Fallible<!>`.
//         Err(self.error(error))
//     }
//     pub fn error(&mut self, error: Error) -> ast::AlreadyReported {
//         self.program.compile_error(error)
//     }

//     pub fn lookup_symbol(&mut self, span: Span, name: &str) -> ast::ExprData {
//         if let Some(local_var_id) = self.locals.get(name) {
//             ast::ExprData::Variable(*local_var_id)
//         } else if let Some(global_value) = self.program.lookup_global_symbol(name) {
//             ast::ExprData::Constant(global_value)
//         } else {
//             ast::ExprData::Constant(Err(self.error(Error::cannot_resolve_name(span))))
//         }
//     }
// }
