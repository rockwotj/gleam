use crate::ast::{Arg, AssignmentKind, BinOp, CallArg, Pattern, TypedExpr};
use crate::cplusplus::declaration::{function_args, to_symbol, transform_type};
use crate::cplusplus::error::Error;
use crate::cplusplus::scope::{LexicalScope, LocalVariable};
use crate::cplusplus::INDENT;
use crate::docvec;
use crate::pretty::*;
use crate::type_::{PatternConstructor, Type, ValueConstructor, ValueConstructorVariant};
use itertools::Itertools;
use std::borrow::Borrow;
use std::sync::Arc;
use std::vec::Vec;

pub struct GeneratedExpr<'a> {
    pub eval: Document<'a>,
    pub result: Document<'a>,
}

impl<'a> GeneratedExpr<'a> {
    fn nil() -> GeneratedExpr<'a> {
        GeneratedExpr {
            eval: nil(),
            result: nil(),
        }
    }
    fn new(eval: Document<'a>, result: Document<'a>) -> GeneratedExpr<'a> {
        GeneratedExpr { eval, result }
    }
    fn result(result: Document<'_>) -> GeneratedExpr<'_> {
        GeneratedExpr {
            eval: nil(),
            result,
        }
    }
    fn of(result: &str) -> GeneratedExpr<'_> {
        GeneratedExpr {
            eval: nil(),
            result: Document::String(result.to_owned()),
        }
    }
}

pub(crate) struct ExpressionGenerator {
    lexical_scope: LexicalScope,
}

impl<'module> ExpressionGenerator {
    pub fn new(lexical_scope: LexicalScope) -> Self {
        ExpressionGenerator { lexical_scope }
    }

    pub fn generate_expr(
        &mut self,
        expr: &'module TypedExpr,
    ) -> Result<GeneratedExpr<'module>, Error> {
        Ok(match expr {
            TypedExpr::Int { value, .. } => GeneratedExpr::of(value),
            TypedExpr::Float { value, .. } => GeneratedExpr::of(value),
            TypedExpr::String { value, .. } => {
                // TODO: Are there other values (\t, \r, etc) that we need to escape?
                let doc = if value.contains('\n') {
                    Document::String(value.replace('\n', r#"\n"#))
                } else {
                    value.to_doc()
                };
                // TODO: Do we need to escape quotes?
                GeneratedExpr::result(doc.surround("gleam::MakeRef<gleam::String>(u8\"", "\")"))
            }
            TypedExpr::Var {
                name, constructor, ..
            } => self.generate_variable(name, constructor)?,
            TypedExpr::RecordAccess { record, label, .. } => {
                self.generate_record_access(label, record)?
            }
            TypedExpr::BinOp {
                name,
                left,
                right,
                typ,
                ..
            } => {
                if *name == BinOp::And || *name == BinOp::Or {
                    self.generate_lazy_bin_op(typ, name, left, right)?
                } else {
                    self.generate_eager_binop(name, left, right)?
                }
            }
            TypedExpr::Assignment {
                kind: AssignmentKind::Let,
                value,
                typ,
                pattern,
                ..
            } => self.generate_assignment(value, typ, pattern)?,
            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                self.generate_sequence(expressions)?
            }
            TypedExpr::Call { fun, args, .. } => self.generate_call(fun, args)?,
            TypedExpr::Fn {
                args, body, typ, ..
            } => self.generate_fn(typ, args, body)?,
            TypedExpr::List {
                typ,
                elements,
                tail,
                ..
            } => self.generate_list(typ, elements, tail)?,
            _ => {
                return Err(Error::Unimplemented {
                    message: format!("{:?}", expr),
                })
            }
        })
    }

    fn generate_list(
        &mut self,
        typ: &'module Arc<Type>,
        elements: &'module [TypedExpr],
        tail: &'module Option<Box<TypedExpr>>,
    ) -> Result<GeneratedExpr<'module>, Error> {
        let mut list_eval = nil();
        let mut element_results: Vec<Document<'module>> = vec![];
        for element in elements {
            let GeneratedExpr { eval, result } = self.generate_expr(element)?;
            list_eval = list_eval.append(eval);
            element_results.push(result);
        }
        let element_type = typ
            .list_element_type()
            .expect("Unable to determine list element type");
        let GeneratedExpr {
            eval: tail_eval,
            result: tail_result,
        } = match tail {
            Some(t) => self.generate_expr(t)?,
            None => GeneratedExpr::nil(),
        };
        list_eval = list_eval.append(tail_eval);
        let list_result = docvec![
            "gleam::MakeList<",
            transform_type(element_type.borrow()),
            ">({",
            Document::Vec(
                Itertools::intersperse(element_results.into_iter(), break_(",", ", ")).collect()
            ),
            "}",
            if tail_result.is_empty() {
                nil()
            } else {
                break_(",", ", ").append(tail_result)
            },
            ")",
        ];
        return Ok(GeneratedExpr::new(list_eval, list_result));
    }

    fn generate_variable(
        &mut self,
        name: &'module str,
        constructor: &'module ValueConstructor,
    ) -> Result<GeneratedExpr<'module>, Error> {
        Ok(match constructor {
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                type_,
            } => {
                let (mut args, ret_type) =
                    type_.fn_types().ok_or_else(|| Error::InternalError {
                        message: format!("Unexpected type for ModuleFn: {:?}", type_),
                    })?;
                args.push(ret_type);
                GeneratedExpr::result(to_symbol(name, *public, module, &args))
            }
            ValueConstructor {
                public,
                variant:
                    ValueConstructorVariant::Record {
                        module,
                        name,
                        arity,
                        ..
                    },
                type_,
            } if *arity > 0 => {
                let (mut args, ret_type) =
                    type_.fn_types().ok_or_else(|| Error::InternalError {
                        message: format!("Unexpected type for record constructor: {:?}", type_),
                    })?;
                let module: Vec<String> = module.split('/').map(|s| s.to_string()).collect();
                let arg_types = Document::Vec(
                    Itertools::intersperse(
                        args.iter().map(|arg| transform_type(arg)),
                        break_(",", ", "),
                    )
                    .collect(),
                );
                args.push(ret_type);
                GeneratedExpr::result(docvec!(
                    "gleam::WrappedConstructor<",
                    // TODO: This seems to be the wrong public?
                    to_symbol(name, *public, &module, &args),
                    break_(",", ", "),
                    arg_types,
                    ">()",
                ))
            }
            ValueConstructor {
                variant: ValueConstructorVariant::Record { module, name, .. },
                ..
            } if module.is_empty() && name == "True" => GeneratedExpr::of("true"),
            ValueConstructor {
                variant: ValueConstructorVariant::Record { module, name, .. },
                ..
            } if module.is_empty() && name == "False" => GeneratedExpr::of("false"),
            ValueConstructor {
                public,
                variant: ValueConstructorVariant::Record { module, name, .. },
                type_,
            } => {
                let args = match type_.as_ref() {
                    Type::App { args, .. } => Ok(args),
                    _ => Err(Error::InternalError {
                        message: format!("Unexpected type for record singleton: {:?}", type_),
                    }),
                }?;
                let module: Vec<String> = module.split('/').map(|s| s.to_string()).collect();
                // TODO: If this is a template we need to declare that
                GeneratedExpr::result(docvec![
                    "gleam::MakeRef<",
                    to_symbol(name, *public, &module, args),
                    ">()",
                ])
            }
            _ => GeneratedExpr::result(self.lexical_scope.local_var(name)?.name),
        })
    }

    fn generate_record_access(
        &mut self,
        label: &'module str,
        record: &'module TypedExpr,
    ) -> Result<GeneratedExpr<'module>, Error> {
        let generated_record = self.generate_expr(record)?;

        Ok(GeneratedExpr::new(
            generated_record.eval,
            generated_record
                .result
                .append(docvec!["->", Document::String(label.to_owned()), "()",]),
        ))
    }

    fn generate_fn(
        &mut self,
        typ: &'module Arc<Type>,
        args: &'module Vec<Arg<Arc<Type>>>,
        body: &'module TypedExpr,
    ) -> Result<GeneratedExpr<'module>, Error> {
        let (_, result_type) = typ.fn_types().ok_or(Error::InternalError {
            message: format!("Unexpected type for function: {:?}", typ),
        })?;
        self.lexical_scope = self.lexical_scope.clone().into_child();
        for arg in args {
            if let Some(name) = arg.names.get_variable_name() {
                let _ = self.lexical_scope.declare_local_var(name, &arg.type_);
            }
        }
        let GeneratedExpr { eval, result } = self.generate_expr(body)?;
        self.lexical_scope =
            self.lexical_scope
                .clone()
                .into_parent()
                .ok_or(Error::InternalError {
                    message: "Unexpected root scope".into(),
                })?;
        let body_expr = docvec![line(), eval, docvec!["return ", result, ";"]]
            .nest(INDENT)
            .group();
        let lambda_decl = docvec![
            "[=](",
            function_args(args),
            ") -> ",
            transform_type(&result_type),
            " {",
            body_expr,
            line(),
            "}",
        ];
        return Ok(GeneratedExpr::result(lambda_decl));
    }

    fn generate_call(
        &mut self,
        fun: &'module TypedExpr,
        args: &'module [CallArg<TypedExpr>],
    ) -> Result<GeneratedExpr<'module>, Error> {
        let call_fn = match fun {
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        public,
                        variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                        type_,
                    },
                ..
            } => {
                let (mut args, ret_type) =
                    type_.fn_types().ok_or_else(|| Error::InternalError {
                        message: format!("Unexpected type for module fn: {:?}", type_),
                    })?;
                args.push(ret_type);
                GeneratedExpr::result(to_symbol(name, *public, module, &args))
            }
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        public,
                        variant: ValueConstructorVariant::Record { module, name, .. },
                        type_,
                    },
                ..
            } => {
                let args = match type_.as_ref() {
                    Type::App { args, .. } => Ok(args.clone()),
                    Type::Fn { args, retrn } => {
                        let mut args = args.clone();
                        args.push(retrn.clone());
                        Ok(args)
                    }
                    _ => Err(Error::InternalError {
                        message: format!("Unexpected type for record constructor: {:?}", type_),
                    }),
                }?;
                let module: Vec<String> = module.split('/').map(|s| s.to_string()).collect();
                // TODO: If this is a template we need to declare that
                GeneratedExpr::result(
                    to_symbol(name, *public, &module, &args).surround("gleam::MakeRef<", ">"),
                )
            }
            _ => {
                let GeneratedExpr { eval, result } = self.generate_expr(fun)?;
                GeneratedExpr::new(eval, result)
            }
        };
        let mut statements = call_fn.eval;
        let generated_args: Vec<GeneratedExpr<'module>> = args
            .iter()
            .map(|arg| self.generate_expr(&arg.value))
            .try_collect()?;
        let mut args: Vec<Document<'module>> = vec![];
        for generated_arg in generated_args {
            statements = statements.append(generated_arg.eval);
            args.push(generated_arg.result);
        }
        let call =
            Document::Vec(Itertools::intersperse(args.into_iter(), break_(",", ", ")).collect())
                .surround(call_fn.result.append("("), ")");
        Ok(GeneratedExpr::new(statements, call))
    }

    fn generate_sequence(
        &mut self,
        expressions: &'module Vec<TypedExpr>,
    ) -> Result<GeneratedExpr<'module>, Error> {
        let mut seq = nil();
        let mut result = nil();
        for expr in expressions {
            let generated = self.generate_expr(expr)?;
            let prev = if result.is_empty() {
                result
            } else {
                docvec![result, ";", line()]
            };
            seq = seq.append(docvec![prev, generated.eval]);
            result = generated.result;
        }
        return Ok(GeneratedExpr::new(seq, result));
    }

    fn generate_assignment(
        &mut self,
        value: &'module TypedExpr,
        typ: &'module Arc<Type>,
        pattern: &'module Pattern<PatternConstructor, Arc<Type>>,
    ) -> Result<GeneratedExpr<'module>, Error> {
        let generated_value = self.generate_expr(value)?;
        if let Pattern::Var { name, .. } = pattern {
            let LocalVariable { name, .. } = self.lexical_scope.declare_local_var(name, typ);
            return Ok(GeneratedExpr {
                eval: generated_value.eval,
                result: docvec![
                    transform_type(typ),
                    " ",
                    name,
                    " = ",
                    generated_value.result,
                ],
            });
        }
        Err(Error::Unimplemented {
            message: format!("Unsupported pattern assignment: {:?}", pattern),
        })
    }

    fn generate_lazy_bin_op(
        &mut self,
        typ: &'module Arc<Type>,
        name: &'module BinOp,
        left: &'module TypedExpr,
        right: &'module TypedExpr,
    ) -> Result<GeneratedExpr<'module>, Error> {
        // TODO: There are cases when this can be simplified to a "normal" || or && in C++, but then
        // this also needs to be handled in the wrapping logic.
        let left_expr = self.generate_expr(left)?;
        let LocalVariable {
            name: tmp_var_name, ..
        } = self.lexical_scope.declare_local_var("tmp", typ);
        let right_expr = self.generate_expr(right)?;
        let negate = if *name == BinOp::And { "" } else { "!" };
        let lazy_eval = docvec![
            line(),
            right_expr.eval,
            tmp_var_name.clone(),
            " = ",
            right_expr.result,
            ";"
        ]
        .nest(INDENT)
        .group();
        Ok(GeneratedExpr {
            eval: docvec![
                left_expr.eval,
                "bool ",
                tmp_var_name.clone(),
                " = ",
                left_expr.result,
                ";",
                line(),
                "if (",
                negate.to_doc(),
                tmp_var_name.clone(),
                ") {",
                lazy_eval,
                line(),
                "}",
                line(),
            ],
            result: tmp_var_name.clone(),
        })
    }

    fn generate_eager_binop(
        &mut self,
        name: &'module BinOp,
        left: &'module TypedExpr,
        right: &'module TypedExpr,
    ) -> Result<GeneratedExpr<'module>, Error> {
        let left_expr = self.generate_expr(left)?;
        let right_expr = self.generate_expr(right)?;
        let wrap_left = is_wrapped_binop_child_expression(left);
        let wrap_right = is_wrapped_binop_child_expression(right);
        let final_result = docvec![
            if wrap_left {
                left_expr.result.surround("(", ")")
            } else {
                left_expr.result
            },
            " ",
            generate_op(name)?,
            " ",
            if wrap_right {
                right_expr.result.surround("(", ")")
            } else {
                right_expr.result
            },
        ];
        Ok(GeneratedExpr::new(
            docvec![left_expr.eval, right_expr.eval],
            final_result,
        ))
    }
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

fn generate_op(op: &BinOp) -> Result<&'static str, Error> {
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
