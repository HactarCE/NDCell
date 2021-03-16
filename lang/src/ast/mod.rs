use codemap::{CodeMap, File, Span, Spanned};
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::sync::Arc;

mod cellstate;
mod globals;
mod nodes;

use crate::data::{LangInt, Type, Value};
use crate::errors::{Error, Result};
use crate::lexer::Token;
use crate::parser;
pub use cellstate::CellState;
pub use nodes::*;

#[derive(Debug)]
pub struct Program {
    codemap: CodeMap,
    main_file: Arc<File>,

    /// Lazily-parsed directives.
    directives: HashMap<DirectiveKey, LazyDirective>,
    nodes: Vec<AnyNode>,

    compile_errors: Vec<Error>,
    runtime_errors: Vec<Error>,
}
impl Program {
    /// Produce an AST from program source code.
    pub fn new(codemap: CodeMap, main_file: Arc<File>) -> Self {
        let mut ret = Self {
            codemap,
            main_file: Arc::clone(&main_file),

            directives: HashMap::new(),
            nodes: vec![],

            compile_errors: vec![],
            runtime_errors: vec![],
        };

        match parser::tokenize_and_split_directives(&main_file) {
            Err(e) => {
                ret.compile_error(e);
            }
            Ok(ok) => {
                // For each directive ...
                for directive_tokens in ok {
                    // Use IIFE for error handling.
                    let result = || -> Result<()> {
                        if directive_tokens.is_empty() {
                            internal_error!("empty directive token slice");
                        }
                        // Parse just enough of the directive to know what else might
                        // depend on it.
                        let mut p = parser::Parser::new(&main_file, &directive_tokens)?;
                        let mut ctx = parser::Ctx::new(&mut ret, None);
                        let key = p.parse(&mut ctx, parser::rules::DirectiveKey)?;
                        let value = if ret.directives.contains_key(&key) {
                            // Duplicate! Report an error.
                            LazyDirective::Parsed(Err(
                                ret.compile_error(Error::duplicate_directive(p.span(), &key))
                            ))
                        } else if key
                            .name()
                            .and_then(|name| ret.lookup_global_symbol(name))
                            .is_some()
                        {
                            // Name conflict! Report an error.
                            LazyDirective::Parsed(Err(
                                ret.compile_error(Error::name_in_use(p.span()))
                            ))
                        } else if key
                            .name()
                            .and_then(|name| globals::lookup_builtin(name))
                            .is_some()
                        {
                            // Name conflict with built-in! Report a warning and
                            // proceed.
                            LazyDirective::Parsed(Err(
                                ret.compile_error(Error::name_in_use_by_builtin(p.span()))
                            ))
                        } else {
                            // This is not a duplicate; add it to the hash map.
                            LazyDirective::NotParsedYet(directive_tokens)
                        };
                        ret.directives.insert(key, value);
                        Ok(())
                    }();
                    if let Err(e) = result {
                        ret.compile_error(e);
                    }
                }
            }
        }

        ret
    }

    pub fn codemap(&self) -> &CodeMap {
        &self.codemap
    }

    pub fn add_node<D>(&mut self, span: Span, data: D) -> NodeId<Node<D>>
    where
        Node<D>: NodeTrait,
    {
        let id = self.nodes.len();
        self.nodes.push(Node { id, span, data }.into_any_node());
        NodeId(id, PhantomData)
    }
    pub fn compile_error(&mut self, error: Error) -> AlreadyReported {
        self.compile_errors.push(error);
        AlreadyReported
    }

    pub fn get_node<'ast, N: NodeTrait>(&'ast mut self, id: NodeId<N>) -> AstNode<'ast, N> {
        AstNode { ast: self, id }
    }

    pub fn try_get_directive<'ast>(
        &'ast mut self,
        key: DirectiveKey,
    ) -> Option<Fallible<DirectiveContents>> {
        self.directives
            .contains_key(&key)
            .then(|| self.get_directive(key))
    }
    pub fn get_directive<'ast>(&'ast mut self, key: DirectiveKey) -> Fallible<DirectiveContents> {
        if let Some(value) = self.directives.get_mut(&key) {
            match std::mem::replace(value, LazyDirective::InProgress) {
                LazyDirective::NotParsedYet(tokens) => {
                    // Parse the directive.
                    let file = Arc::clone(&self.main_file);
                    let result = parser::Parser::new(&file, &tokens).and_then(|mut p| {
                        let mut ctx = parser::Ctx::new(self, Some(&key));
                        p.parse(&mut ctx, parser::rules::Directive)
                    });

                    // Record the error if one occurred.
                    let result = match result {
                        Ok((_k, contents)) => Ok(contents),
                        Err(e) => {
                            self.compile_error(e);
                            Err(AlreadyReported)
                        }
                    };

                    // Store the result.
                    self.directives
                        .insert(key, LazyDirective::Parsed(result.clone()));
                    result
                }
                LazyDirective::InProgress => {
                    let result = Err(AlreadyReported);
                    *value = LazyDirective::Parsed(result.clone());
                    self.compile_error(Error::dependency_cycle(None, key));
                    result
                }
                LazyDirective::Parsed(result) => {
                    *value = LazyDirective::Parsed(result.clone());
                    result
                }
            }
        } else {
            // Generate a default value for the directive.
            let default = match &key {
                DirectiveKey::Const { .. } => None,
                DirectiveKey::Func { .. } => None,
                DirectiveKey::Name => None,
                DirectiveKey::Ndim => {
                    Some(Ok(Value::Integer(crate::DEFAULT_NDIM as LangInt)).into())
                }
                DirectiveKey::States => {
                    Some(Ok(Value::Integer(crate::DEFAULT_STATE_COUNT as LangInt)).into())
                }
                DirectiveKey::Transition => {
                    // Default transition function keeps all cells the same. The
                    // span doesn't really matter.
                    let stmt = self.add_node(self.main_file.span.subspan(0, 0), StmtData::Remain);
                    Some(stmt.into())
                }
            };
            // If the directive doesn't have a default value, generate an
            // error.
            if default.is_none() {
                self.compile_error(Error::missing_directive(None, &key));
            }
            let result = default.ok_or(AlreadyReported);
            self.directives
                .insert(key, LazyDirective::Parsed(result.clone()));
            result
        }
    }

    pub fn lookup_global_symbol(&mut self, name: &str) -> Option<Fallible<Value>> {
        if let Some(directive_contents) = None
            .or_else(|| {
                self.try_get_directive(DirectiveKey::Const {
                    name: name.to_owned(),
                })
            })
            .or_else(|| {
                self.try_get_directive(DirectiveKey::Func {
                    name: name.to_owned(),
                })
            })
        {
            match directive_contents {
                Ok(DirectiveContents::Value(v)) => Some(v),
                Ok(DirectiveContents::Code(_)) => {
                    self.compile_error(Error::internal(
                        None,
                        "const/func directive has code contents",
                    ));
                    return Some(Err(AlreadyReported));
                }
                Err(e) => return Some(Err(e)),
            }
        } else if let Some(v) = globals::lookup_builtin(name) {
            Some(Ok(v))
        } else {
            None
        }
    }
}

/// Directive information.
///
/// This is just enough to know what other parts of the code may depend on the directive.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DirectiveKey {
    Const { name: String },
    Func { name: String },
    Name,
    Ndim,
    States,
    Transition,
}
impl fmt::Display for DirectiveKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const { name } => write!(f, "@const {}", name),
            Self::Func { name } => write!(f, "@func {}", name),
            Self::Name => write!(f, "@name"),
            Self::Ndim => write!(f, "@ndim"),
            Self::States => write!(f, "@states"),
            Self::Transition => write!(f, "@transition"),
        }
    }
}
impl DirectiveKey {
    pub fn name(&self) -> Option<&str> {
        match self {
            DirectiveKey::Const { name } | DirectiveKey::Func { name } => Some(name),
            DirectiveKey::Name
            | DirectiveKey::Ndim
            | DirectiveKey::States
            | DirectiveKey::Transition => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DirectiveContents {
    Value(Fallible<Value>),
    Code(StmtId),
}
impl From<Fallible<Value>> for DirectiveContents {
    fn from(v: Fallible<Value>) -> Self {
        Self::Value(v)
    }
}
impl From<StmtId> for DirectiveContents {
    fn from(s: StmtId) -> Self {
        Self::Code(s)
    }
}

/// Lazily-parsed directive.
#[derive(Debug, Clone)]
pub enum LazyDirective {
    /// The directive is present exactly once and its contents have not yet been
    /// parsed.
    NotParsedYet(Vec<Spanned<Token>>),
    /// The directive contents are being parsed.
    InProgress,
    /// The directive contents have been parsed. If an error occurred, it has
    /// already been reported.
    Parsed(Fallible<DirectiveContents>),
}

pub type Fallible<T> = std::result::Result<T, AlreadyReported>;
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct AlreadyReported;
