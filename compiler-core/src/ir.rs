use crate::ast;
use crate::type_::Type;
use std::sync::Arc;
use std::vec::Vec;

/// # An intermediate representation (IR) of Gleam's AST for a "simple" procedural language.
///
/// Right now this IR supports being emitted to either C++ or JavaScript with very little actual
/// transformations or needing to be "lowered" to another IR.


#[derive(Debug, Clone)]
pub enum Statement {
    Return { expr: Expression },
    Assignment { 
        /// If the type is a declaration.
        /// # Examples (if true):
        ///
        /// ```javascript
        /// let a = // expr
        /// ```
        ///
        /// ```c++
        /// auto a = // expr
        /// ```
        ///
        /// # Examples (if false):
        ///
        /// ```javascript
        /// a = // expr
        /// ```
        ///
        /// ```c++
        /// a = // expr
        /// ```
        is_declaration: bool,
        /// The name of the variable - this may be reserved for a given language, or may be a
        /// "redelcaration" so it may not be valid in that programming language to redeclare a
        /// variable.
        ///
        /// Examples:
        ///
        /// ```gleam
        /// let a = 1;
        /// let a = 2;
        /// ```
        ///
        /// ```javascript
        /// const a = 1;
        /// // invalid error, this should be renamed when the ir is being emitted.
        /// const a = 2;
        /// ```
        var: String,
        expr: Expression,
    },
    /// An expression with an unused result. This maybe a side-effect or just dead code.
    Expr { expr: Expression },
    Conditional {
        test: Expression,
        body: Vec<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    /// This can be implemented via an "immediately invoked function expression"
    /// in languages that support it.
    ///
    /// Examples:
    ///
    /// ```javascript
    /// (() => {
    ///   // statements
    /// })();
    /// ```
    ///
    /// ```c++
    /// ([=]() {
    ///   // statements
    /// })();
    /// ```
    Block(Vec<Statement>),
    /// A "literal" type, which is Ints, Floats, and Strings in Gleam.
    /// Booleans are implemented as a TypeConstruction of True/False.
    Literal(Literal),
    Call {
        // invoking a function
    },
    Accessor(Accessor),
    TypeConstruction(TypeConstruction),
    /// A binary operator is 
    BinOp {
        left: Box<Self>,
        op: ast::BinOp,
        right:Box<Self> 
    },
    /// A unary operator is a 
    UnaryOp { 
        op: UnaryOp,
        expr: Box<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int { value: String },
    Float { value: String },
    String { value: String },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp { Negate }

#[derive(Debug, Clone)]
pub enum Accessor {
    Custom { },
    TupleIndex { },
    Variable { },
}

#[derive(Debug, Clone)]
pub enum TypeConstruction {
    Tuple { },
    /// If the list is `[a, b, c, ..rest]` then elements is `a, b, c` and tail is `..rest`.
    List { typ: Arc<Type>, elements: Vec<Expression>, tail: Option<Box<Expression>> },
    Custom { },
    BitString { },
}

#[derive(Debug, Clone)]
pub enum Call {
    /// A "builtin" function is a function that is provided by the gleam compiler. It is usually
    /// apart of the prelude, but can sometimes be provided by the target language itself.
    Builtin(BuiltinFn),
    /// Invoking a Gleam defined function in this module or another.
    Fn { },
}

/// A "builtin" function is a function that is provided by the gleam compiler. It is usually
/// apart of the prelude, but can sometimes be provided by the target language itself.
#[derive(Debug, Clone, Copy)]
pub enum BuiltinFn {
    ListAtLeastLength,
    ListHead,
    ListTail,
}

/// Converts a typed expression that represents the body of a function call in gleam to a
/// procedural IR.
pub fn ast_to_ir(expr: &ast::TypedExpr) -> Vec<Statement> {
    // Don't wrap sequences into lambdas, but just write them out directly as the function body
    match expr {
        ast::TypedExpr::Sequence { expressions, .. } | ast::TypedExpr::Pipeline { expressions, .. } => convert_top_level_exprs_to_ir(&expressions),
        ast::TypedExpr::Case { .. } => todo!(),
        // Other expressions can directly be a single return statement
        _ => vec![Statement::Return { expr: convert_expr_to_ir(expr) }],
    }
}

fn convert_top_level_exprs_to_ir(exprs: &[ast::TypedExpr]) -> Vec<Statement> {
    let last_index = exprs.len() - 1;
    exprs.iter().enumerate().map(|(i, e)| convert_top_level_expr_to_ir(e, i == last_index)).collect()
}

fn convert_top_level_expr_to_ir(expr: &ast::TypedExpr, is_in_return_position: bool) -> Statement {
    match expr {
        ast::TypedExpr::Assignment { .. } => todo!(),
        ast::TypedExpr::Try { .. } => todo!(),
        _ if is_in_return_position => Statement::Return { expr: convert_expr_to_ir(expr) },
        _ => Statement::Expr { expr: convert_expr_to_ir(expr) },
    }
}

fn convert_expr_to_ir(expr: &ast::TypedExpr) -> Expression {
    match expr {
        ast::TypedExpr::Int { value, .. } => Expression::Literal(Literal::Int{ value: value.to_owned() }),
        ast::TypedExpr::Float { value, .. } => Expression::Literal(Literal::String{ value: value.to_owned() }),
        ast::TypedExpr::String { value, .. } => Expression::Literal(Literal::String{ value: value.replace("\n", r#"\n"#) }),
        ast::TypedExpr::BinOp { name, left, right, .. } => Expression::BinOp { 
            left: Box::new(convert_expr_to_ir(left)), op: *name, right: Box::new(convert_expr_to_ir(right)),
        },
        ast::TypedExpr::List { elements, tail, typ, .. } => Expression::TypeConstruction(TypeConstruction::List { 
            typ: typ.clone(),
            elements: elements.iter().map(convert_expr_to_ir).collect(), 
            tail: tail.as_ref().map(|e| Box::new(convert_expr_to_ir(e))),
        }),
        ast::TypedExpr::Sequence { expressions, .. } | ast::TypedExpr::Pipeline { expressions, .. } => Expression::Block(convert_top_level_exprs_to_ir(expressions)),
        _ => todo!()
    }
}
