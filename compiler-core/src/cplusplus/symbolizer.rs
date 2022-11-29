use crate::cplusplus::error::Error;
use crate::docvec;
use crate::pretty::*;
use crate::type_::{Type, TypeVar};
use itertools::Itertools;
use std::{ops::Deref, sync::Arc};

pub(crate) struct Symbolizer {}

impl Symbolizer {
    pub fn new() -> Self {
        Symbolizer {}
    }

    pub fn type_to_symbol<'a, 'b>(&mut self, typ: &'a Type) -> Result<Document<'b>, Error> {
        return Ok(if typ.is_int() {
            "int64_t".to_doc()
        } else if typ.is_bool() {
            "bool".to_doc()
        } else if typ.is_float() {
            "double".to_doc()
        } else if typ.is_string() {
            "gleam::String".to_doc()
        } else {
            match typ {
                Type::App {
                    name,
                    public,
                    module,
                    args,
                } => {
                    let sym = self.app_symbol(name, *public, &module[..], args)?;
                    sym.surround("gleam::Ref<", ">")
                }
                Type::Fn { args, retrn } => self.function_type(retrn, args)?,
                Type::Var { type_ } => match type_.borrow().deref() {
                    TypeVar::Link { type_: typ } => self.type_to_symbol(typ)?,
                    TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                        self.generate_generic_type_param(*id)
                    }
                },
                Type::Tuple { elems } => {
                    let elems = self.app_symbol_args(elems)?;
                    elems.surround("gleam::Ref<gleam::Tuple", ">")
                }
            }
        });
    }

    pub fn function_type<'a, 'b>(
        &mut self,
        result: &'a Type,
        args: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        let mut doc = docvec![
            "gleam::Function<",
            self.type_to_symbol(result)?,
            break_(",", ", ")
        ];
        doc = doc.append(comma_seperate(
            args.iter().map(|arg| self.type_to_symbol(arg)).try_collect()?,
        ));
        Ok(doc.append(">"))
    }

    pub fn app_symbol<'a, 'b, S: AsRef<str>>(
        &mut self,
        name: &'a str,
        public: bool,
        module: &'a [S],
        args: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        let mut doc = if module.is_empty() {
            "gleam::".to_doc()
        } else {
            Document::String(module.iter().map(|s| s.as_ref()).join("::")).surround("::", "::")
        };
        if !public {
            doc = doc.append("_private::");
        }
        Ok(docvec![
            doc,
            Document::String(name.to_owned()),
            self.app_symbol_args(args)?,
        ])
    }

    pub fn extract_symbol_args(&mut self, typ: &Type) -> Vec<Arc<Type>> {
        match typ.deref() {
            Type::App { args, .. } => args.to_owned(),
            Type::Tuple { elems } => elems.to_owned(),
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_: typ } => self.extract_symbol_args(typ),
                TypeVar::Generic { .. } | TypeVar::Unbound { .. } => vec![],
            },
            Type::Fn { .. } => vec![],
        }
    }

    pub fn symbol_args<'a, 'b>(&mut self, typ: &'a Type) -> Result<Document<'b>, Error> {
        let args = self.extract_symbol_args(typ);
        self.app_symbol_args(&args)
    }

    pub fn app_symbol_name(&mut self, typ: &Type) -> Result<String, Error> {
        match typ.deref() {
            Type::App { name, .. } => Ok(name.to_owned()),
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_: typ } => self.app_symbol_name(typ),
                TypeVar::Generic { .. } | TypeVar::Unbound { .. } => Err(Error::InternalError {
                    message: "Unexpected generic type when app type expected".to_owned(),
                }),
            },
            _ => Err(Error::InternalError {
                message: "Unexpected generic type when app type expected".to_owned(),
            }),
        }
    }

    pub fn app_symbol_args<'a, 'b>(
        &mut self,
        args: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        if args.is_empty() {
            return Ok(nil());
        }
        Ok(
            comma_seperate(args.iter().map(|a| self.type_to_symbol(a)).try_collect()?)
                .surround("<", ">"),
        )
    }

    pub fn template_specification<'a, 'b>(
        &mut self,
        args: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        let template_params: Vec<_> = self
            .infer_template_parameters(args)
            .into_iter()
            .map(|a| self.type_to_symbol(&a).map(|d| docvec!["typename ", d]))
            .try_collect()?;
        let template_params: Vec<_> = template_params.into_iter().unique().collect();
        if template_params.is_empty() {
            return Ok(nil());
        }
        Ok(docvec![
            "template <",
            comma_seperate(template_params),
            ">",
            line()
        ])
    }

    fn infer_template_parameters(&mut self, args: &[Arc<Type>]) -> Vec<Arc<Type>> {
        args.iter()
            .flat_map(|a| self.infer_template_parameter(a))
            .collect()
    }

    fn infer_template_parameter(&mut self, arg: &Arc<Type>) -> Vec<Arc<Type>> {
        match arg.deref() {
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_ } => self.infer_template_parameter(type_),
                _ => vec![arg.clone()],
            },
            Type::App { args, .. } => self.infer_template_parameters(args),
            Type::Tuple { elems } => self.infer_template_parameters(elems),
            Type::Fn { args, retrn } => vec![
                self.infer_template_parameters(args),
                self.infer_template_parameter(retrn),
            ]
            .concat(),
        }
    }

    fn generate_generic_type_param<'a>(&mut self, id: u64) -> Document<'a> {
        Document::String(format!("T${}", id))
    }
}

fn comma_seperate(elements: Vec<Document<'_>>) -> Document<'_> {
    join(elements, break_(",", ", "))
}
