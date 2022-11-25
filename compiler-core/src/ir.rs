use crate::ast;
use crate::type_::{ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant};
use crate::uid::UniqueIdGenerator;
use im;
use std::sync::Arc;
use std::vec::Vec;
use tracing::Id;

/// # An intermediate representation (IR) of Gleam's AST for a "simple" procedural language.
///
/// Right now this IR supports being emitted to either C++ or JavaScript with very little actual
/// transformations or needing to be "lowered" to another IR.
///
/// This IR preserves the full Gleam type information for output languages that are also typed.
/// There are operations in the IR for "casting", which maybe a noop on dynamic languages (such as
/// JavaScript), but would be required for a language like C++.

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Return {
        expr: Expression<'a>,
    },
    Assignment {
        var: Identifier<'a>,
        expr: Expression<'a>,
        typ: Arc<Type>,
    },
    /// An expression with an unused result. This maybe a side-effect or just dead code.
    Expr {
        expr: Expression<'a>,
    },
    Conditional {
        test: Expression<'a>,
        body: Vec<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    /// A "literal" type, which is Ints, Floats, Booleans, Strings and Nil in Gleam.
    /// Booleans + Nil are technically implemented as a prelude type in Gleam but using a literal simplifies
    /// codegen for most langugages, as builtin types are used..
    Literal(Literal<'a>),
    Call(Call<'a>),
    Accessor(Accessor<'a>),
    TypeConstruction(TypeConstruction<'a>),
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
pub enum Literal<'a> {
    Bool { value: bool },
    Int { value: &'a str },
    Float { value: &'a str },
    String { value: String },
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, Clone)]
pub enum Accessor<'a> {
    Custom {
        label: &'a str,
        reciever: Box<Expression<'a>>,
    },
    TupleIndex {
        index: u64,
        tuple: Box<Expression<'a>>,
    },
    LocalVariable {
        name: Identifier<'a>,
        typ: Arc<Type>,
    },
    ModuleVariable {
        public: bool,
        module: Vec<&'a str>,
        /// This is the imported name of the module, if not set, then the module is either defined
        /// within the current module OR is an unqualified import.
        module_alias: Option<&'a str>,
        name: &'a str,
        typ: Arc<Type>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeConstruction<'a> {
    Tuple {
        typ: Arc<Type>,
        elements: Vec<Expression<'a>>,
    },
    /// If the list is `[a, b, c, ..rest]` then elements is `a, b, c` and tail is `..rest`.
    List {
        typ: Arc<Type>,
        elements: Vec<Expression<'a>>,
        tail: Option<Box<Expression<'a>>>,
    },
    Custom {
        public: bool,
        module: Vec<&'a str>,
        /// This is the imported name of the module, if not set, then the module is either defined
        /// within the current module OR is an unqualified import.
        module_alias: Option<&'a str>,
        name: &'a str,
        typ: Arc<Type>,
        args: Vec<Expression<'a>>,
    },
    /// Singleton types are special cased, so that codegen can not allocate more memory but use a
    /// single shared reference.
    CustomSingleton {
        public: bool,
        module: Vec<&'a str>,
        /// This is the imported name of the module, if not set, then the module is either defined
        /// within the current module OR is an unqualified import.
        module_alias: Option<&'a str>,
        name: &'a str,
        typ: Arc<Type>,
    },
    // BitString {},
    /// λ
    Function {
        typ: Arc<Type>,
        args: Vec<FunctionArg<'a>>,
        body: Vec<Statement<'a>>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionArg<'a> {
    name: Identifier<'a>,
    typ: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Identifier<'a> {
    /// A name that came from the source (or as result of syntax sugar expansion).
    /// The number is the times that the variable has been declared in the current scope.
    /// For example - Gleam allows for a sequence like so:
    /// ```gleam
    /// let a = 5
    /// let a = "foo"
    /// let a = True
    /// ```
    ///
    /// In our IR, we represent `a` as `Named("a", 0)`, `Named("a", 1)` and `Named("a", 2)`
    /// for target languages that don't support redeclaring variables within the same scope.
    Named(&'a str, u64),
    /// An  name that is required for the purposes of the IR, but did not originate from the source
    /// program. Codegen backends should emit a variable that makes the most sense for that target.
    Internal(u64),
    /// An unused idenfitier.
    Discard,
}

#[derive(Debug, Clone)]
pub enum Call<'a> {
    /// A "builtin" function is a function that is provided by the gleam compiler. It is usually
    /// apart of the prelude, but can sometimes be provided by the target language itself.
    // Builtin(BuiltinFn),
    /// Invoking a Gleam defined function in this module or another.
    Fn {
        callee: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
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
pub struct IntermediateRepresentationConverter<'module> {
    internal_variable_id_generator: UniqueIdGenerator,
    current_scope_vars: im::HashMap<&'module str, u64>,
}

impl<'module> IntermediateRepresentationConverter<'module> {
    pub fn new() -> Self {
        return IntermediateRepresentationConverter {
            internal_variable_id_generator: UniqueIdGenerator::new(),
            current_scope_vars: im::HashMap::new(),
        };
    }
    /// Converts a typed expression that represents the body of a function call in gleam to a
    /// procedural IR.
    pub fn ast_to_ir(&mut self, expr: &'module ast::TypedExpr) -> Vec<Statement<'module>> {
        match expr {
            ast::TypedExpr::Sequence { expressions, .. }
            | ast::TypedExpr::Pipeline { expressions, .. } => {
                self.convert_top_level_exprs_to_ir(expressions)
            }
            _ => self.convert_top_level_expr_to_ir(expr, true),
        }
    }

    fn convert_top_level_exprs_to_ir(
        &mut self,
        exprs: &'module [ast::TypedExpr],
    ) -> Vec<Statement<'module>> {
        let last_index = exprs.len() - 1;
        exprs
            .iter()
            .enumerate()
            .flat_map(|(i, e)| self.convert_top_level_expr_to_ir(e, i == last_index))
            .collect()
    }

    fn convert_top_level_expr_to_ir(
        &mut self,
        expr: &'module ast::TypedExpr,
        is_in_return_position: bool,
    ) -> Vec<Statement<'module>> {
        match expr {
            ast::TypedExpr::Assignment {
                typ,
                value,
                kind: ast::AssignmentKind::Let,
                pattern: ast::Pattern::Var { name, .. },
                ..
            } => {
                let mut assignment = vec![Statement::Assignment {
                    var: self.allocate_named_id(name),
                    expr: self.convert_expr_to_ir(value),
                    typ: typ.to_owned(),
                }];
                if is_in_return_position {
                    assignment.push(Statement::Return {
                        expr: Expression::Accessor(Accessor::LocalVariable {
                            name: self.lookup_named_id(name),
                            typ: typ.to_owned(),
                        }),
                    })
                }
                assignment
            }
            ast::TypedExpr::Assignment { .. } => todo!(),
            ast::TypedExpr::Try { .. } => todo!(),
            // TODO: When `try` is supported, this is no longer valid, but JS makes this assumption
            // so it's probably fine until https://github.com/gleam-lang/gleam/issues/1834 is
            // fixed. Once that is fixed we should just be able to delete this case so it's wrapped
            // in a block.
            ast::TypedExpr::Sequence { expressions, .. }
            | ast::TypedExpr::Pipeline { expressions, .. } => {
                self.convert_top_level_exprs_to_ir(&expressions)
            }
            _ if is_in_return_position => vec![Statement::Return {
                expr: self.convert_expr_to_ir(expr),
            }],
            _ => vec![Statement::Expr {
                expr: self.convert_expr_to_ir(expr),
            }],
        }
    }

    fn convert_expr_to_ir(&mut self, expr: &'module ast::TypedExpr) -> Expression<'module> {
        match expr {
            ast::TypedExpr::Int { value, .. } => Expression::Literal(Literal::Int { value }),
            ast::TypedExpr::Float { value, .. } => Expression::Literal(Literal::Float { value }),
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
                    label,
                    reciever: Box::new(self.convert_expr_to_ir(record)),
                })
            }
            ast::TypedExpr::ModuleSelect {
                module_alias,
                typ,
                module_name,
                label,
                constructor:
                    ModuleValueConstructor::Fn { .. } | ModuleValueConstructor::Constant { .. },
                ..
            } => Expression::Accessor(Accessor::ModuleVariable {
                public: true,
                module: split_module_name(&module_name),
                module_alias: Some(module_alias),
                name: label,
                typ: typ.to_owned(),
            }),
            // TODO: This case needs to handled via wrapping the constructor
            // see convert_variable_to_ir
            // TODO: Does this need to handle Singletons or does the above?
            ast::TypedExpr::ModuleSelect {
                module_alias,
                typ,
                module_name,
                label,
                constructor: ModuleValueConstructor::Record { .. },
                ..
            } => todo!(),
            ast::TypedExpr::Tuple { typ, elems, .. } => {
                Expression::TypeConstruction(TypeConstruction::Tuple {
                    typ: typ.to_owned(),
                    elements: elems.iter().map(|e| self.convert_expr_to_ir(e)).collect(),
                })
            }
            ast::TypedExpr::TupleIndex { tuple, index, .. } => {
                Expression::Accessor(Accessor::TupleIndex {
                    index: *index,
                    tuple: Box::new(self.convert_expr_to_ir(tuple)),
                })
            }
            ast::TypedExpr::Todo { .. } => todo!(),
            ast::TypedExpr::BitString { .. } => todo!(),
            ast::TypedExpr::RecordUpdate { .. } => todo!(),
            // The rest here are things that cannot be represented as expressions in our IR, so we
            // wrap them in blocks that are immediately invoked functions.
            ast::TypedExpr::Sequence { expressions, .. }
            | ast::TypedExpr::Pipeline { expressions, .. } => self
                .wrap_in_block(expr.type_(), || {
                    self.convert_top_level_exprs_to_ir(expressions)
                }),
            ast::TypedExpr::Assignment { .. }
            | ast::TypedExpr::Try { .. }
            | ast::TypedExpr::Case { .. } => {
                self.wrap_in_block(expr.type_(), || self.ast_to_ir(expr))
            }
        }
    }

    fn convert_call_to_ir(
        &mut self,
        fun: &'module ast::TypedExpr,
        args: &'module [ast::CallArg<ast::TypedExpr>],
    ) -> Expression<'module> {
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
                module_alias: None,
                module: split_module_name(module),
                name,
                typ: type_.to_owned(),
                args,
            });
        } else if let ast::TypedExpr::ModuleSelect {
            module_name,
            constructor: ModuleValueConstructor::Record { name, type_, .. },
            ..
        } = fun
        {
            return Expression::TypeConstruction(TypeConstruction::Custom {
                public: true,
                module_alias: Some(module_name),
                module: split_module_name(module_name),
                name,
                typ: type_.to_owned(),
                args,
            });
        }
        let callee = Box::new(self.convert_expr_to_ir(fun));
        Expression::Call(Call::Fn { callee, args })
    }

    fn convert_fn_to_ir(
        &mut self,
        typ: &'module Arc<Type>,
        args: &'module Vec<ast::Arg<Arc<Type>>>,
        body: &'module ast::TypedExpr,
    ) -> Expression<'module> {
        self.with_new_scope(|| {
            Expression::TypeConstruction(TypeConstruction::Function {
                typ: typ.to_owned(),
                args: args
                    .iter()
                    .map(|arg| FunctionArg {
                        name: match arg.get_variable_name() {
                            Some(name) => self.allocate_named_id(name),
                            None => Identifier::Discard,
                        },
                        typ: arg.type_.to_owned(),
                    })
                    .collect(),
                body: self.ast_to_ir(body),
            })
        })
    }

    fn convert_variable_to_ir(
        &mut self,
        name: &'module str,
        constructor: &'module ValueConstructor,
    ) -> Expression<'module> {
        match constructor {
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleFn { module, .. },
                type_,
            } => Expression::Accessor(Accessor::ModuleVariable {
                public: *public,
                module_alias: None,
                module: module.iter().map(|s| &s[..]).collect(),
                name,
                typ: type_.to_owned(),
            }),
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleConstant { module, .. },
                type_,
            } => Expression::Accessor(Accessor::ModuleVariable {
                public: *public,
                module_alias: None,
                module: split_module_name(module),
                name,
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
                            module_alias: None,
                            module: split_module_name(module),
                            name,
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
                module_alias: None,
                name,
                typ: type_.to_owned(),
            }),
            ValueConstructor {
                variant: ValueConstructorVariant::LocalVariable { .. },
                type_,
                ..
            } => Expression::Accessor(Accessor::LocalVariable {
                name: self.lookup_named_id(name),
                typ: type_.to_owned(),
            }),
        }
    }

    fn generate_internal_id(&mut self) -> Identifier<'module> {
        Identifier::Internal(self.internal_variable_id_generator.next())
    }
    fn lookup_named_id(&mut self, name: &'module str) -> Identifier<'module> {
        match self.current_scope_vars.get(name) {
            None => todo!("Unexpected missing scope var"),
            Some(id) => Identifier::Named(name, *id),
        }
    }
    fn allocate_named_id(&mut self, name: &'module str) -> Identifier<'module> {
        let n = match self.current_scope_vars.get(name) {
            None => 0,
            Some(n) => *n + 1,
        };
        self.current_scope_vars.insert(name, n);
        Identifier::Named(name, n)
    }
    fn with_new_scope<Block, Output>(&mut self, block: Block) -> Output
    where
        Block: Fn() -> Output,
    {
        let parent_scope = self.current_scope_vars;
        let child_scope = parent_scope.clone();
        self.current_scope_vars = child_scope;
        let result = block();
        self.current_scope_vars = parent_scope;
        result
    }

    /// Some expressions can only be converted into statements, so we need to wrap the statements
    /// within a function.
    fn wrap_in_block<Block>(&mut self, typ: Arc<Type>, expr: Block) -> Expression<'module>
    where
        Block: Fn() -> Vec<Statement<'module>>,
    {
        Expression::Call(Call::Fn {
            args: vec![],
            callee: Box::new(Expression::TypeConstruction(TypeConstruction::Function {
                typ: Arc::new(Type::Fn {
                    args: vec![],
                    retrn: typ,
                }),
                args: vec![],
                body: self.with_new_scope(expr),
            })),
        })
    }
}

fn split_module_name(module: &str) -> Vec<&str> {
    module.split('/').collect()
}
