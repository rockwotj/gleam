use crate::ast::{Arg, AssignmentKind, BinOp, CallArg, Pattern, TypedExpr};
use crate::cplusplus::declaration::{function_args, to_symbol, transform_type};
use crate::cplusplus::error::Error;
use crate::cplusplus::scope::{LexicalScope, LocalVariable};
use crate::cplusplus::INDENT;
use crate::docvec;
use crate::ir;
use crate::pretty::*;
use crate::type_::{PatternConstructor, Type, ValueConstructor, ValueConstructorVariant};
use itertools::Itertools;
use std::borrow::Borrow;
use std::sync::Arc;
use std::vec::Vec;

pub struct NativeIrCodeGenerator {
    scope: LexicalScope,
}

impl NativeIrCodeGenerator {
    pub fn new(scope: LexicalScope) -> Self {
        return NativeIrCodeGenerator { scope };
    }

    pub fn ir_to_doc(&mut self, statements: &Vec<ir::Statement<'_>>) -> Result<Document<'_>, Error> {
        Ok(Document::Vec(
                Itertools::intersperse(
                    statements.iter().map(|s| self.ir_statement_to_doc(s)),
                    Ok(line()),
                    )
                .try_collect()?,
                ))
    }

    fn ir_statement_to_doc(&mut self, statement: &ir::Statement<'_>) -> Result<Document<'_>, Error> {
        Ok(match statement {
            ir::Statement::Return { expr } => {
                docvec!["return ", self.ir_expr_to_doc(expr)?, ";"]
            }
            ir::Statement::Assignment { var, expr, typ } => {
                let declared = self.scope.declare_local_var(var, &typ);
                docvec![
                    self.typ_to_symbol(typ.clone())?,
                    " ",
                    declared.name,
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

    fn ir_expr_to_doc(&mut self, expr: &ir::Expression<'_>) -> Result<Document<'_>, Error> {
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
                    generate_bin_op(&op)?,
                    self.wrap_expr(*right)?,
                ]
            }
            ir::Expression::UnaryOp { op, expr } => {
                docvec![self.generate_unary_op(op)?, self.wrap_expr(*expr)?]
            }
        })
    }

    fn ir_literal_to_doc(&mut self, literal: &ir::Literal<'_>) -> Result<Document<'_>, Error> {
        Ok(match literal {
            ir::Literal::Bool { value } => if value { "true" } else { "false" }.to_doc(),
            ir::Literal::Int { value } => value.to_doc(),
            ir::Literal::Float { value } => value.to_doc(),
            ir::Literal::String { value } => {
                Document::String(value).surround("gleam::MakeString(u8\"", "\")")
            }
            ir::Literal::Nil => "gleam::Nil::INSTANCE".to_doc(),
        })
    }

    fn ir_call_to_doc(&mut self, call: &ir::Call<'_>) -> Result<Document<'_>, Error> {
        Ok(match call {
            ir::Call::Fn { callee, args } => {
                let formatted_args = comma_seperate_results(
                    args.into_iter().map(|e| self.ir_expr_to_doc(e)),
                ).try_collect()?;
                docvec![
                    self.ir_expr_to_doc(*callee)?,
                    "(",
                    Document::Vec(formatted_args),
                    ")",
                ]
            },
        })
    }

    fn ir_accessor_to_doc(&mut self, accessor: &ir::Accessor<'_>) -> Result<Document<'_>, Error> {
        Ok(match accessor {
            ir::Accessor::Custom { label, reciever } => {
                docvec![self.ir_expr_to_doc(*reciever)?, "->", label.to_doc()]
            },
            ir::Accessor::TupleIndex { index, tuple } => self.ir_expr_to_doc(*tuple)?.surround(
                docvec!["std::get<", Document::String(format!("{}", index)), ">("],
                ")",
            ),
            ir::Accessor::LocalVariable { name, .. } => self.ir_identifier_to_doc(name)?,
            ir::Accessor::ModuleVariable {
                public,
                module,
                module_alias,
                name,
                typ,
            } => {
                todo!()
            },
        })
    }

    fn ir_identifier_to_doc(&mut self, identifier: &ir::Identifier<'_>) -> Result<Document<'_>, Error> {
        todo!()
    }

    fn ir_type_construction_to_doc(
        &mut self,
        construction: &ir::TypeConstruction<'_>,
    ) -> Result<Document<'_>, Error> {
        Ok(match construction {
            ir::TypeConstruction::Tuple { typ, elements } => {
                // TODO: Specify type args.
                docvec![
                    "gleam::MakeTuple",
                    "(",
                    comma_seperate(
                        elements.into_iter().map(|e| self.ir_expr_to_doc(e)).try_collect()?
                    ),
                    ")",
                ]
            },
            ir::TypeConstruction::List { typ, elements, tail } => {
                // TODO: Specify type args.
                docvec![
                    "gleam::MakeList",
                    "(",
                    comma_seperate(
                        elements.into_iter().map(|e| self.ir_expr_to_doc(e)).try_collect()?
                    ),
                    tail.map(|e| self.ir_expr_to_doc(e)),
                    ")",
                ]
            },
            ir::TypeConstruction::Custom { public, module, module_alias, name, typ, args } => todo!(),
            ir::TypeConstruction::CustomSingleton { public, module, module_alias, name, typ } => todo!(),
            ir::TypeConstruction::Function { typ, args, body } => {
                let (_, result_type) = typ.fn_types().ok_or(Error::InternalError {
                    message: format!("Unexpected type for function: {:?}", typ),
                })?;
                let statements = self.ir_to_doc(body)?;
                docvec![
                    "[=](",
                    function_args(args),
                    ") -> ",
                    self.typ_to_symbol(result_type)?,
                    " {",
                    statements.nest(INDENT).group(),
                    line(),
                    "}",
                ]
            },
        })
    }

    fn wrap_expr(&mut self, expr: &ir::Expression<'_>) -> Result<Document<'_>, Error> {
        let needs_wrap = match expr {
            ir::Expression::Literal(_) => false,
            ir::Expression::Accessor(ir::Accessor::LocalVariable { .. }) => false,
            ir::Expression::Accessor(ir::Accessor::ModuleVariable { .. }) => false,
            _ => true,
        };
        if !needs_wrap {
            return self.ir_expr_to_doc(expr);
        }
        return Ok(self.ir_expr_to_doc(expr)?.surround("(", ")"));
    }

    fn typ_to_symbol(&mut self, typ: Arc<Type>) -> Result<Document<'_>, Error> {
        todo!()
    }

    fn generate_unary_op(&mut self, op: ir::UnaryOp) -> Result<&'static str, Error> {
        Ok(match op {
            ir::UnaryOp::Negate => "!",
        })
    }
}

fn comma_seperate(elements: Vec<Document<'_>>) -> Document<'_> {
    Document::Vec(Itertools::intersperse(elements.into_iter(), break_(",", ", ")).collect())
}

fn is_wrapped_binop_child_expression(expr: &TypedExpr) -> bool {
    if expr.is_literal() {
        return false;
    }
    match expr {
        TypedExpr::Var { .. } => false,
        TypedExpr::BinOp { name, .. } => *name != BinOp::And && *name != BinOp::Or,
        TypedExpr::RecordAccess { .. } => false,
        _ => true,
    }
}

fn generate_bin_op(op: &BinOp) -> Result<&'static str, Error> {
    Ok(match op {
        BinOp::Eq => "==",
        BinOp::NotEq => "!=",
        BinOp::LtInt => "<",
        BinOp::LtEqInt => "<=",
        BinOp::LtFloat => "<",
        BinOp::LtEqFloat => "<=",
        BinOp::GtEqInt => ">=",
        BinOp::GtInt => ">",
        BinOp::GtEqFloat => ">=",
        BinOp::GtFloat => ">",
        BinOp::AddInt => "+",
        BinOp::AddFloat => "+",
        BinOp::SubInt => "-",
        BinOp::SubFloat => "-",
        BinOp::MultInt => "*",
        BinOp::MultFloat => "*",
        BinOp::DivInt => "/",
        BinOp::DivFloat => "/",
        BinOp::RemainderInt => "%",
        BinOp::Concatenate => "+",
        BinOp::And => "&&",
        BinOp::Or => "||",
    })
}
