use super::*;

pub type Expr<'ast> = AstNode<'ast, ExprNode>;
pub type ExprId = NodeId<ExprNode>;
pub type ExprNode = Node<ExprData>;

#[derive(Debug)]
pub enum ExprData {
    /// Parenthetical group.
    Paren(ExprId),

    /// Identifier.
    Identifier(Arc<String>),

    /// Constant value.
    Constant(RtVal),

    /// Binary operator.
    BinaryOp(ExprId, Spanned<BinaryOp>, ExprId),
    /// Prefix operator.
    PrefixOp(Spanned<PrefixOp>, ExprId),
    /// Comparison chain.
    CmpChain(Vec<ExprId>, Vec<Spanned<CompareOp>>),

    /// Attribute access or method call.
    MethodCall {
        obj: ExprId,
        attr: Spanned<Arc<String>>, // TODO: struct/enum for built-in method?
        args: Vec<ExprId>,
    },
    /// Function call.
    FuncCall {
        func: Spanned<Arc<String>>,
        args: Vec<ExprId>,
    },
    /// Index operation.
    IndexOp { obj: ExprId, args: Vec<ExprId> },

    /// Vector constructor.
    VectorConstruct(Vec<ExprId>),
}
impl ExprData {
    pub fn ret_type(&mut self) -> Type {
        todo!()
    }
}

// #[derive(Debug, Default, Copy, Clone)]
// pub struct CannotConstEval;

// impl<'ast> Expr<'ast> {
//     /// Compile-time evaluates the expression. Reports an error and returns `Err(AlreadyReported)` if an error occurs.
//     pub fn const_eval(&mut self) -> Fallible<ConstValue> {
//         match self.try_const_eval() {
//             Some(result) => result,
//             None => Err(self
//                 .ast
//                 .compile_error(Error::cannot_const_eval(self.span()))),
//         }
//     }

//     /// Compile-time evaluates the expression. Returns `None` if the expression
//     /// cannot be evaluated at compile time.
//     pub fn try_const_eval(&mut self) -> Option<Fallible<ConstValue>> {
//         match self.data() {
//             ExprData::Paren(inner) => self.ast.get_node(*inner).try_const_eval(),

//             ExprData::Variable(_) => None,
//             ExprData::Constant(value) => Some(value.clone()),

//             ExprData::BinaryOp(lhs, op, rhs) => match op.node {
//                 BinaryOp::Compare(cmp_op) => match cmp_op {
//                     CompareOp::Eql => todo!("compile-time eval operation"),
//                     CompareOp::Neq => todo!("compile-time eval operation"),
//                     CompareOp::Lt => todo!("compile-time eval operation"),
//                     CompareOp::Gt => todo!("compile-time eval operation"),
//                     CompareOp::Lte => todo!("compile-time eval operation"),
//                     CompareOp::Gte => todo!("compile-time eval operation"),
//                 },

//                 BinaryOp::Add => todo!("compile-time eval operation"),
//                 BinaryOp::Sub => todo!("compile-time eval operation"),
//                 BinaryOp::Mul => todo!("compile-time eval operation"),
//                 BinaryOp::Div => todo!("compile-time eval operation"),
//                 BinaryOp::Mod => todo!("compile-time eval operation"),
//                 BinaryOp::Pow => todo!("compile-time eval operation"),

//                 BinaryOp::Shl => todo!("compile-time eval operation"),
//                 BinaryOp::ShrSigned => todo!("compile-time eval operation"),
//                 BinaryOp::ShrUnsigned => todo!("compile-time eval operation"),

//                 BinaryOp::BitwiseAnd => todo!("compile-time eval operation"),
//                 BinaryOp::BitwiseOr => todo!("compile-time eval operation"),
//                 BinaryOp::BitwiseXor => todo!("compile-time eval operation"),

//                 BinaryOp::LogicalAnd => todo!("compile-time eval operation"),
//                 BinaryOp::LogicalOr => todo!("compile-time eval operation"),
//                 BinaryOp::LogicalXor => todo!("compile-time eval operation"),

//                 BinaryOp::Range => todo!("compile-time eval operation"),

//                 BinaryOp::Is => todo!("compile-time eval operation"),
//             },
//             ExprData::PrefixOp(op, value) => match op.node {
//                 PrefixOp::Pos => todo!("compile-time eval operation"),
//                 PrefixOp::Neg => todo!("compile-time eval operation"),

//                 PrefixOp::BitwiseNot => todo!("compile-time eval operation"),
//                 PrefixOp::LogicalNot => todo!("compile-time eval operation"),

//                 PrefixOp::IntToCell => todo!("compile-time eval operation"),
//             },
//             ExprData::MethodCall { obj, attr, args } => todo!("compile-time eval method call"),
//             ExprData::FuncCall { func, args } => todo!("compile-time eval function call"),
//             ExprData::IndexOp { obj, args } => todo!("compile-time eval index operation"),

//             ExprData::VectorConstruct(_) => todo!("compile-time eval vector construction"),
//         }
//     }
// }

// /// "Function" that takes zero or more arguments and returns a value that can be
// /// compiled and optionally compile-time evaluated. The argument types that
// /// function takes are baked into the function when it is constructed.
// ///
// /// This language uses a very broad definition of "function" so that all kinds
// /// of expressions can be represented using the same system. Properties/methods,
// /// operators, variable access, etc. all have corresponding "Function"
// /// implementors.
// pub trait Function: fmt::Debug {
//     /// End-user-friendly name for the function being called.
//     ///
//     /// For methods and properties, the name should be prefixed with the type
//     /// followed by a period (e.g. "Vector[3].sum").
//     fn name(&self, expr: Expr<'_>) -> String;

//     /// Returns the return type of this function, or an error (e.g.
//     /// InvalidArguments) if the function is passed invalid arguments.
//     fn return_type(&self, expr: Expr<'_>) -> Type;

//     // /// Compiles this function using the given arguments and returns the Value
//     // /// returned from it (which should have the type of ret_ty).
//     // ///
//     // /// This function may panic or return an Err(InternalError) if ArgValues has
//     // /// invalid types.
//     // fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> Value;

//     // /// Evaluates this function using the given ArgValues and returns the Value
//     // /// returned from it (which should have the type of ret_ty) if the
//     // /// expression can be evaluated at compile time, or Err(CannotEvalAsConst)
//     // /// if this expression cannot be evaluated at compile time.
//     // ///
//     // /// This function may panic or return an Err(InternalError) if ArgValues has
//     // /// invalid types.
//     // fn const_eval(&self, info: FuncCallInfo) -> Value {
//     //     Err(CannotEvalAsConst.with_span(info.span))
//     // }

//     // /// Returns this function as an AssignableFunction, or
//     // /// None if it is not assignable.
//     // fn as_assignable(&self, _info: FuncCallInfo) -> Option<&dyn AssignableFunction> {
//     //     None
//     // }
// }
