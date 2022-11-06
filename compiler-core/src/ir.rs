use crate::ast;
use crate::type_::{Type, ValueConstructor, ValueConstructorVariant};
use crate::uid::UniqueIdGenerator;
use std::sync::Arc;
use std::vec::Vec;

/// # An intermediate representation (IR) of Gleam's AST for a "simple" procedural language.
///
/// Right now this IR supports being emitted to either C++ or JavaScript with very little actual
/// transformations or needing to be "lowered" to another IR.
///
/// This IR preserves the full Gleam type information for output languages that are also typed.
/// There are operations in the IR for "casting", which maybe a noop on dynamic languages (such as
/// JavaScript), but would be required for a language like C++.

#[derive(Debug, Clone)]
pub enum Statement {
    Return {
        expr: Expression,
    },
    Assignment {
        var: String,
        expr: Expression,
        typ: Arc<Type>,
    },
    /// An expression with an unused result. This maybe a side-effect or just dead code.
    Expr {
        expr: Expression,
    },
    Conditional {
        test: Expression,
        body: Vec<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    /// A "literal" type, which is Ints, Floats, Booleans, Strings and Nil in Gleam.
    /// Booleans + Nil are technically implemented as a prelude type in Gleam but using a literal simplifies
    /// codegen for most langugages, as builtin types are used..
    Literal(Literal),
    Call(Call),
    Accessor(Accessor),
    TypeConstruction(TypeConstruction),
    /// A binary operator is
    BinOp {
        left: Box<Self>,
        op: ast::BinOp,
        right: Box<Self>,
    },
    /// A unary operator is a
    UnaryOp {
        op: UnaryOp,
        expr: Box<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool { value: bool },
    Int { value: String },
    Float { value: String },
    String { value: String },
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, Clone)]
pub enum Accessor {
    Custom {
        label: String,
        reciever: Box<Expression>,
    },
    TupleIndex {
        index: u64,
        tuple: Box<Expression>,
    },
    LocalVariable {
        name: Identifier,
        typ: Arc<Type>,
    },
    ModuleVariable {
        public: bool,
        module: Vec<String>,
        name: String,
        typ: Arc<Type>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeConstruction {
    Tuple {
        typ: Arc<Type>,
        elements: Vec<Expression>,
    },
    /// If the list is `[a, b, c, ..rest]` then elements is `a, b, c` and tail is `..rest`.
    List {
        typ: Arc<Type>,
        elements: Vec<Expression>,
        tail: Option<Box<Expression>>,
    },
    Custom {
        public: bool,
        module: Vec<String>,
        name: String,
        typ: Arc<Type>,
        args: Vec<Expression>,
    },
    /// Singleton types are special cased, so that codegen can not allocate more memory but use a
    /// single shared reference.
    CustomSingleton {
        public: bool,
        module: Vec<String>,
        name: String,
        typ: Arc<Type>,
    },
    // BitString {},
    /// λ
    Function {
        typ: Arc<Type>,
        args: Vec<FunctionArg>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionArg {
    name: Identifier,
    typ: Arc<Type>,
}

#[derive(Debug, Clone)]
pub enum Identifier {
    /// A name that came from the source (or as result of syntax sugar expansion).
    Named(String),
    /// An  name that is required for the purposes of the IR, but did not originate from the source
    /// program. Codegen backends should emit a variable that makes the most sense for that target.
    Internal(u64),
    /// An unused idenfitier.
    Discard,
}

#[derive(Debug, Clone)]
pub enum Call {
    /// A "builtin" function is a function that is provided by the gleam compiler. It is usually
    /// apart of the prelude, but can sometimes be provided by the target language itself.
    // Builtin(BuiltinFn),
    /// Invoking a Gleam defined function in this module or another.
    Fn {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
}

/// A "builtin" function is a function that is provided by the gleam compiler. It is usually
/// apart of the prelude, but can sometimes be provided by the target language itself.
#[derive(Debug, Clone, Copy)]
pub enum BuiltinFn {
    ListAtLeastLength,
    ListHead,
    ListTail,
}

#[derive(Debug)]
pub struct IntermediateRepresentationConverter {
    internal_variable_id_generator: UniqueIdGenerator,
}

impl IntermediateRepresentationConverter {
    pub fn new() -> Self {
        return IntermediateRepresentationConverter {
            internal_variable_id_generator: UniqueIdGenerator::new(),
        };
    }
    /// Converts a typed expression that represents the body of a function call in gleam to a
    /// procedural IR.
    pub fn ast_to_ir(&mut self, expr: &ast::TypedExpr) -> Vec<Statement> {
        match expr {
            ast::TypedExpr::Sequence { expressions, .. }
            | ast::TypedExpr::Pipeline { expressions, .. } => {
                self.convert_top_level_exprs_to_ir(expressions)
            }
            _ => self.convert_top_level_expr_to_ir(expr, true),
        }
    }

    fn convert_top_level_exprs_to_ir(&mut self, exprs: &[ast::TypedExpr]) -> Vec<Statement> {
        let last_index = exprs.len() - 1;
        exprs
            .iter()
            .enumerate()
            .flat_map(|(i, e)| self.convert_top_level_expr_to_ir(e, i == last_index))
            .collect()
    }

    fn convert_top_level_expr_to_ir(
        &mut self,
        expr: &ast::TypedExpr,
        is_in_return_position: bool,
    ) -> Vec<Statement> {
        match expr {
            ast::TypedExpr::Assignment {
                typ,
                value,
                kind: ast::AssignmentKind::Let,
                pattern: ast::Pattern::Var { name, .. },
                ..
            } => {
                let mut assignment = vec![Statement::Assignment {
                    var: name.to_owned(),
                    expr: self.convert_expr_to_ir(value),
                    typ: typ.to_owned(),
                }];
                if is_in_return_position {
                    assignment.push(Statement::Return {
                        expr: Expression::Accessor(Accessor::LocalVariable {
                            name: Identifier::Named(name.to_owned()),
                            typ: typ.to_owned(),
                        }),
                    })
                }
                assignment
            }
            ast::TypedExpr::Assignment { .. } => todo!(),
            ast::TypedExpr::Try { .. } => todo!(),
            _ if is_in_return_position => vec![Statement::Return {
                expr: self.convert_expr_to_ir(expr),
            }],
            _ => vec![Statement::Expr {
                expr: self.convert_expr_to_ir(expr),
            }],
        }
    }

    fn convert_expr_to_ir(&mut self, expr: &ast::TypedExpr) -> Expression {
        match expr {
            ast::TypedExpr::Int { value, .. } => Expression::Literal(Literal::Int {
                value: value.to_owned(),
            }),
            ast::TypedExpr::Float { value, .. } => Expression::Literal(Literal::String {
                value: value.to_owned(),
            }),
            ast::TypedExpr::String { value, .. } => Expression::Literal(Literal::String {
                value: value.replace("\n", r#"\n"#),
            }),
            ast::TypedExpr::BinOp {
                name, left, right, ..
            } => Expression::BinOp {
                left: Box::new(self.convert_expr_to_ir(left)),
                op: *name,
                right: Box::new(self.convert_expr_to_ir(right)),
            },
            ast::TypedExpr::List {
                elements,
                tail,
                typ,
                ..
            } => Expression::TypeConstruction(TypeConstruction::List {
                typ: typ.clone(),
                elements: elements
                    .iter()
                    .map(|e| self.convert_expr_to_ir(e))
                    .collect(),
                tail: tail.as_ref().map(|e| Box::new(self.convert_expr_to_ir(e))),
            }),
            ast::TypedExpr::Var {
                name, constructor, ..
            } => self.convert_variable_to_ir(name, constructor),
            ast::TypedExpr::Fn {
                typ, args, body, ..
            } => self.convert_fn_to_ir(typ, args, body),
            ast::TypedExpr::Call { fun, args, .. } => self.convert_call_to_ir(fun, args),
            ast::TypedExpr::Negate { value, .. } => Expression::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(self.convert_expr_to_ir(value)),
            },
            ast::TypedExpr::RecordAccess { label, record, .. } => {
                Expression::Accessor(Accessor::Custom {
                    label: label.to_owned(),
                    reciever: Box::new(self.convert_expr_to_ir(record)),
                })
            }
            ast::TypedExpr::ModuleSelect { .. } => todo!(),
            ast::TypedExpr::Tuple { typ, elems, .. } => {
                Expression::TypeConstruction(TypeConstruction::Tuple { 
                    typ: typ.to_owned(), 
                    elements: elems.iter().map(|e| self.convert_expr_to_ir(e)).collect(),
                })
            },
            ast::TypedExpr::TupleIndex { tuple, index, .. } => Expression::Accessor(Accessor::TupleIndex { 
                index: *index, 
                tuple: Box::new(self.convert_expr_to_ir(tuple)),
            }),
            ast::TypedExpr::Todo { .. } => todo!(),
            ast::TypedExpr::BitString { .. } => todo!(),
            ast::TypedExpr::RecordUpdate { .. } => todo!(),
            // The rest here are things that cannot be represented as expressions in our IR, so we
            // wrap them in blocks that are immediately invoked functions.
            ast::TypedExpr::Sequence { expressions, .. }
            | ast::TypedExpr::Pipeline { expressions, .. } => wrap_in_block(
                expr.type_(),
                self.convert_top_level_exprs_to_ir(expressions),
            ),
            ast::TypedExpr::Assignment { .. }
            | ast::TypedExpr::Try { .. }
            | ast::TypedExpr::Case { .. } => wrap_in_block(expr.type_(), self.ast_to_ir(expr)),
        }
    }

    fn convert_call_to_ir(
        &mut self,
        fun: &ast::TypedExpr,
        args: &[ast::CallArg<ast::TypedExpr>],
    ) -> Expression {
        let args: Vec<_> = args
            .iter()
            .map(|arg| self.convert_expr_to_ir(&arg.value))
            .collect();
        // Special case direct construction of records - otherwise these will all get wrapped into
        // an anonymous function and have extra indirection.
        if let ast::TypedExpr::Var {
            constructor:
                ValueConstructor {
                    public,
                    variant: ValueConstructorVariant::Record { module, name, .. },
                    type_,
                },
            ..
        } = fun
        {
            return Expression::TypeConstruction(TypeConstruction::Custom {
                public: *public,
                module: split_module_name(module),
                name: name.to_owned(),
                typ: type_.to_owned(),
                args,
            });
        }
        let callee = Box::new(self.convert_expr_to_ir(fun));
        Expression::Call(Call::Fn { callee, args })
    }

    fn convert_fn_to_ir(
        &mut self,
        typ: &Arc<Type>,
        args: &Vec<ast::Arg<Arc<Type>>>,
        body: &ast::TypedExpr,
    ) -> Expression {
        Expression::TypeConstruction(TypeConstruction::Function {
            typ: typ.to_owned(),
            args: args
                .iter()
                .map(|arg| FunctionArg {
                    name: match arg.get_variable_name() {
                        Some(name) => Identifier::Named(name.to_owned()),
                        None => Identifier::Discard,
                    },
                    typ: arg.type_.to_owned(),
                })
                .collect(),
            body: self.ast_to_ir(body),
        })
    }

    fn convert_variable_to_ir(&mut self, name: &str, constructor: &ValueConstructor) -> Expression {
        match constructor {
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleFn { module, .. },
                type_,
            } => Expression::Accessor(Accessor::ModuleVariable {
                public: *public,
                module: module.to_owned(),
                name: name.to_owned(),
                typ: type_.to_owned(),
            }),
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleConstant { module, .. },
                type_,
            } => Expression::Accessor(Accessor::ModuleVariable {
                public: *public,
                module: split_module_name(module),
                name: name.to_owned(),
                typ: type_.to_owned(),
            }),
            ValueConstructor {
                public,
                variant:
                    ValueConstructorVariant::Record {
                        name,
                        module,
                        arity,
                        ..
                    },
                type_,
            } if *arity > 0 => {
                // Constructors in Gleam are essentially just factory functions. Here we wrap them in
                // functions. We special case direct calls to create custom types in the call operator.
                // This is a fallback path for something like:
                // ```gleam
                // type Foo {
                //   Foo(String)
                // }
                // fn bar(str: String) -> Foo {
                //   let x = Foo;
                //   x(str)
                // }
                // ```
                let (args, _) = type_
                    .fn_types()
                    .expect("Constructor variable to be a function");
                let args: Vec<_> = args
                    .into_iter()
                    .map(|typ| FunctionArg {
                        name: self.generate_internal_id(),
                        typ,
                    })
                    .collect();
                Expression::TypeConstruction(TypeConstruction::Function {
                    typ: type_.to_owned(),
                    args: args.clone(),
                    body: vec![Statement::Return {
                        expr: Expression::TypeConstruction(TypeConstruction::Custom {
                            public: *public,
                            module: split_module_name(module),
                            name: name.to_owned(),
                            typ: type_.to_owned(),
                            args: args
                                .into_iter()
                                .map(|arg| {
                                    Expression::Accessor(Accessor::LocalVariable {
                                        name: arg.name,
                                        typ: arg.typ,
                                    })
                                })
                                .collect(),
                        }),
                    }],
                })
            }
            ValueConstructor {
                variant: ValueConstructorVariant::Record { name, .. },
                type_,
                ..
            } if type_.is_bool() => Expression::Literal(Literal::Bool {
                value: name == "True",
            }),
            ValueConstructor {
                variant: ValueConstructorVariant::Record { .. },
                type_,
                ..
            } if type_.is_nil() => Expression::Literal(Literal::Nil),
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::Record { module, name, .. },
                type_,
            } => Expression::TypeConstruction(TypeConstruction::CustomSingleton {
                public: *public,
                module: split_module_name(module),
                name: name.to_owned(),
                typ: type_.to_owned(),
            }),
            ValueConstructor {
                variant: ValueConstructorVariant::LocalVariable { .. },
                type_,
                ..
            } => Expression::Accessor(Accessor::LocalVariable {
                name: Identifier::Named(name.to_owned()),
                typ: type_.to_owned(),
            }),
        }
    }

    fn generate_internal_id(&mut self) -> Identifier {
        Identifier::Internal(self.internal_variable_id_generator.next())
    }
}

/// Some expressions can only be converted into statements, so we need to wrap the statements
/// within a function.
fn wrap_in_block(typ: Arc<Type>, expr: Vec<Statement>) -> Expression {
    Expression::Call(Call::Fn {
        args: vec![],
        callee: Box::new(Expression::TypeConstruction(TypeConstruction::Function {
            typ: Arc::new(Type::Fn {
                args: vec![],
                retrn: typ,
            }),
            args: vec![],
            body: expr,
        })),
    })
}

fn split_module_name(module: &str) -> Vec<String> {
    module.split('/').map(|s| s.to_string()).collect()
}
