use crate::ast;
use crate::cplusplus::error::Error;
use crate::cplusplus::symbolizer::Symbolizer;
use crate::cplusplus::INDENT;
use crate::docvec;
use crate::ir;
use crate::pretty::*;
use crate::type_::Type;
use itertools::Itertools;
use std::sync::Arc;
use std::vec::Vec;

pub struct NativeIrCodeGenerator {
    symbolizer: Symbolizer,
}

impl<'module> NativeIrCodeGenerator {
    pub fn new() -> Self {
        NativeIrCodeGenerator {
            symbolizer: Symbolizer::new(),
        }
    }

    pub fn ir_to_doc(
        &mut self,
        statements: Vec<ir::Statement<'module>>,
    ) -> Result<Document<'module>, Error> {
        Ok(Document::Vec(
            Itertools::intersperse(
                statements.into_iter().map(|s| self.ir_statement_to_doc(s)),
                Ok(line()),
            )
            .try_collect()?,
        ))
    }

    fn ir_statement_to_doc(
        &mut self,
        statement: ir::Statement<'module>,
    ) -> Result<Document<'module>, Error> {
        Ok(match statement {
            ir::Statement::Return { expr } => {
                docvec!["return ", self.ir_expr_to_doc(expr)?, ";"]
            }
            ir::Statement::Assignment { var, expr, typ } => {
                docvec![
                    self.typ_to_symbol(typ)?,
                    " ",
                    self.ir_identifier_to_doc(var)?,
                    " = ",
                    self.ir_expr_to_doc(expr)?,
                    ";"
                ]
            }
            ir::Statement::Expr { expr } => docvec![self.ir_expr_to_doc(expr)?, ";"],
            ir::Statement::Conditional { test, body } => docvec![
                "if (",
                self.ir_expr_to_doc(test)?,
                ") {",
                self.ir_to_doc(body)?.nest(INDENT).group(),
                "}",
            ],
        })
    }

    fn ir_expr_to_doc(
        &mut self,
        expr: ir::Expression<'module>,
    ) -> Result<Document<'module>, Error> {
        Ok(match expr {
            ir::Expression::Literal(literal) => self.ir_literal_to_doc(literal)?,
            ir::Expression::Call(call) => self.ir_call_to_doc(call)?,
            ir::Expression::Accessor(accessor) => self.ir_accessor_to_doc(accessor)?,
            ir::Expression::TypeConstruction(construction) => {
                self.ir_type_construction_to_doc(construction)?
            }
            ir::Expression::BinOp { left, op, right } => {
                docvec![
                    self.wrap_expr(*left)?,
                    " ",
                    generate_bin_op(op)?,
                    " ",
                    self.wrap_expr(*right)?,
                ]
            }
            ir::Expression::UnaryOp { op, expr } => {
                docvec![self.generate_unary_op(op)?, self.wrap_expr(*expr)?]
            }
        })
    }

    fn ir_literal_to_doc(
        &mut self,
        literal: ir::Literal<'module>,
    ) -> Result<Document<'module>, Error> {
        Ok(match literal {
            ir::Literal::Bool { value } => if value { "true" } else { "false" }.to_doc(),
            // TODO: Can we tell the compiler that the str's lifetime here is not tied to `literal`
            // but to `'module`?
            ir::Literal::Int { value } => Document::String(value.to_owned()),
            ir::Literal::Float { value } => Document::String(value.to_owned()),
            ir::Literal::String { value } => {
                Document::String(value).surround("gleam::MakeString(u8\"", "\")")
            }
            ir::Literal::Nil => "gleam::Nil::INSTANCE".to_doc(),
        })
    }

    fn ir_call_to_doc(&mut self, call: ir::Call<'module>) -> Result<Document<'module>, Error> {
        Ok(match call {
            ir::Call::Fn { callee, args } => {
                let formatted_args = comma_seperate(
                    args.into_iter()
                        .map(|e| self.ir_expr_to_doc(e))
                        .try_collect()?,
                );
                docvec![self.ir_expr_to_doc(*callee)?, "(", formatted_args, ")",]
            }
        })
    }

    fn ir_accessor_to_doc(
        &mut self,
        accessor: ir::Accessor<'module>,
    ) -> Result<Document<'module>, Error> {
        Ok(match accessor {
            ir::Accessor::Custom { label, reciever } => {
                docvec![self.ir_expr_to_doc(*reciever)?, "->", label.to_doc()]
            }
            ir::Accessor::TupleIndex { index, tuple } => self.ir_expr_to_doc(*tuple)?.surround(
                docvec!["gleam::Get<", Document::String(format!("{}", index)), ">("],
                ")",
            ),
            ir::Accessor::LocalVariable { name, .. } => self.ir_identifier_to_doc(name)?,
            ir::Accessor::ModuleVariable {
                public,
                module,
                module_alias,
                name,
                typ,
            } => self.module_symbol(name, public, &module[..], module_alias, &typ)?,
        })
    }

    fn ir_identifier_to_doc(
        &mut self,
        identifier: ir::Identifier<'module>,
    ) -> Result<Document<'module>, Error> {
        Ok(match identifier {
            ir::Identifier::Named(name, count) => {
                if count == 0 {
                    name.to_doc()
                } else {
                    docvec![name, "$", count]
                }
            }
            ir::Identifier::Internal(count) => {
                if count == 0 {
                    "_tmp$$".to_doc()
                } else {
                    docvec!["_tmp$$", count]
                }
            }
            ir::Identifier::Discard(count) => {
                if count == 0 {
                    "_$".to_doc()
                } else {
                    docvec!["_$", count]
                }
            }
        })
    }

    fn ir_type_construction_to_doc(
        &mut self,
        construction: ir::TypeConstruction<'module>,
    ) -> Result<Document<'module>, Error> {
        Ok(match construction {
            ir::TypeConstruction::Tuple { typ, elements } => {
                docvec![
                    "gleam::MakeTuple",
                    self.symbolizer.symbol_args(&typ)?,
                    "(",
                    comma_seperate(
                        elements
                            .into_iter()
                            .map(|e| self.ir_expr_to_doc(e))
                            .try_collect()?
                    ),
                    ")",
                ]
            }
            ir::TypeConstruction::List {
                typ,
                elements,
                tail,
            } => {
                docvec![
                    "gleam::MakeList",
                    self.symbolizer.symbol_args(&typ)?,
                    "(",
                    docvec![
                        comma_seperate(
                            elements
                                .into_iter()
                                .map(|e| self.ir_expr_to_doc(e))
                                .try_collect()?
                        )
                        .surround("{", "}"),
                        tail.map(|e| {
                            let t = self.ir_expr_to_doc(*e)?;
                            Ok(break_(",", ", ").append(t))
                        })
                        .unwrap_or_else(|| Ok(nil()))?,
                    ]
                    .nest(INDENT)
                    .group(),
                    ")",
                ]
            }
            ir::TypeConstruction::Custom {
                public,
                module,
                module_alias,
                name,
                args,
                typ,
            } => {
                docvec![
                    "gleam::MakeRef<",
                    self.module_symbol(name, public, &module[..], module_alias, &typ)?,
                    ">(",
                    comma_seperate(
                        args.into_iter()
                            .map(|e| self.ir_expr_to_doc(e))
                            .try_collect()?
                    )
                    .nest(INDENT)
                    .group(),
                    ")",
                ]
            }
            ir::TypeConstruction::CustomSingleton {
                public,
                module,
                module_alias,
                name,
                typ,
            } => {
                docvec![
                    "gleam::MakeRef<",
                    self.module_symbol(name, public, &module[..], module_alias, &typ)?,
                    ">()",
                ]
            }
            ir::TypeConstruction::Function { typ, args, body } => {
                let (_, result_type) = typ.fn_types().ok_or(Error::InternalError {
                    message: format!("Unexpected type for function: {:?}", typ),
                })?;
                let statements = self.ir_to_doc(body)?;
                docvec![
                    "[=](",
                    self.function_args(args)?,
                    ") -> ",
                    self.typ_to_symbol(result_type)?,
                    " {",
                    docvec![line(), statements,].nest(INDENT).group(),
                    line(),
                    "}",
                ]
            }
        })
    }

    fn function_args(
        &mut self,
        args: Vec<ir::FunctionArg<'module>>,
    ) -> Result<Document<'module>, Error> {
        let mut arg_docs = vec![];
        for arg in args {
            arg_docs.push(docvec![
                self.symbolizer.type_to_symbol(&arg.typ)?,
                " ",
                self.ir_identifier_to_doc(arg.name)?,
            ]);
        }
        Ok(comma_seperate(arg_docs))
    }

    fn wrap_expr(&mut self, expr: ir::Expression<'module>) -> Result<Document<'module>, Error> {
        let needs_wrap = !matches!(
            expr,
            ir::Expression::Literal(_)
                | ir::Expression::Accessor(
                    ir::Accessor::LocalVariable { .. } | ir::Accessor::ModuleVariable { .. }
                )
        );
        if !needs_wrap {
            return self.ir_expr_to_doc(expr);
        }
        return Ok(self.ir_expr_to_doc(expr)?.surround("(", ")"));
    }

    fn typ_to_symbol(&mut self, typ: Arc<Type>) -> Result<Document<'module>, Error> {
        self.symbolizer.type_to_symbol(&typ)
    }

    fn module_symbol(
        &mut self,
        name: &str,
        public: bool,
        module: &[&str],
        module_alias: Option<&str>,
        typ: &Type,
    ) -> Result<Document<'module>, Error> {
        self.symbolizer
            .module_symbol(name, public, module, module_alias, typ)
    }

    fn generate_unary_op(&mut self, op: ir::UnaryOp) -> Result<&'static str, Error> {
        Ok(match op {
            ir::UnaryOp::Negate => "!",
        })
    }
}

fn comma_seperate(elements: Vec<Document<'_>>) -> Document<'_> {
    join(elements, break_(",", ", "))
}

fn generate_bin_op(op: ast::BinOp) -> Result<&'static str, Error> {
    Ok(match op {
        ast::BinOp::Eq => "==",
        ast::BinOp::NotEq => "!=",
        ast::BinOp::LtInt => "<",
        ast::BinOp::LtEqInt => "<=",
        ast::BinOp::LtFloat => "<",
        ast::BinOp::LtEqFloat => "<=",
        ast::BinOp::GtEqInt => ">=",
        ast::BinOp::GtInt => ">",
        ast::BinOp::GtEqFloat => ">=",
        ast::BinOp::GtFloat => ">",
        ast::BinOp::AddInt => "+",
        ast::BinOp::AddFloat => "+",
        ast::BinOp::SubInt => "-",
        ast::BinOp::SubFloat => "-",
        ast::BinOp::MultInt => "*",
        ast::BinOp::MultFloat => "*",
        ast::BinOp::DivInt => "/",
        ast::BinOp::DivFloat => "/",
        ast::BinOp::RemainderInt => "%",
        ast::BinOp::Concatenate => "+",
        ast::BinOp::And => "&&",
        ast::BinOp::Or => "||",
    })
}
