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

    pub fn to_symbol<'a, 'b>(&mut self, typ: &'a Type) -> Result<Document<'b>, Error> {
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
                    let sym = self.to_app_symbol(name, *public, &module[..], args)?;
                    sym.surround("gleam::Ref<", ">")
                }
                Type::Fn { args, retrn } => self.function_type(retrn, args)?,
                Type::Var { type_ } => match type_.borrow().deref() {
                    TypeVar::Link { type_: typ } => self.to_symbol(&typ)?,
                    TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                        self.generate_generic_type_param(*id)
                    }
                },
                Type::Tuple { elems } => {
                    let elems = self.to_app_symbol_args(elems)?;
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
            self.to_symbol(&result)?,
            break_(",", ", ")
        ];
        doc = doc.append(comma_seperate(
            args.iter().map(|arg| self.to_symbol(arg)).try_collect()?,
        ));
        Ok(doc.append(">"))
    }

    pub fn to_app_symbol<'a, 'b, S: AsRef<str>>(
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
            self.to_app_symbol_args(args)?,
        ])
    }

    pub fn to_symbol_args<'a, 'b>(
        &mut self,
        typ: &'a Type,
    ) -> Result<Document<'b>, Error> {
        Ok(match typ.deref() {
            Type::App { args, .. } => {
                self.to_app_symbol_args(args)?
            },
            Type::Tuple { elems } => self.to_app_symbol_args(elems)?,
            Type::Var { type_ } => {
                match type_.borrow().deref() {
                    TypeVar::Link { type_: typ } => self.to_symbol_args(&typ)?,
                    TypeVar::Generic { .. } | TypeVar::Unbound { .. } => nil()
                }
            },
            Type::Fn { .. } => nil(),
        })
    }

    pub fn to_app_symbol_args<'a, 'b>(
        &mut self,
        args: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        if args.is_empty() {
            return Ok(nil());
        }
        Ok(
            comma_seperate(args.iter().map(|a| self.to_symbol(&a)).try_collect()?)
                .surround("<", ">"),
        )
    }

    pub fn infer_template_parameters<'a>(&mut self, args: &'a [Arc<Type>]) -> Vec<Arc<Type>> {
        args.iter()
            .flat_map(|a| self.infer_template_parameter(a))
            .collect()
    }

    fn infer_template_parameter<'a>(&mut self, arg: &'a Arc<Type>) -> Vec<Arc<Type>> {
        match arg.deref() {
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_ } => self.infer_template_parameter(type_),
                _ => vec![arg.clone()],
            },
            _ => vec![],
        }
    }

    fn generate_generic_type_param<'a>(&mut self, id: u64) -> Document<'a> {
        Document::String(format!("T${}", id))
    }
}

fn comma_seperate(elements: Vec<Document<'_>>) -> Document<'_> {
    Document::Vec(Itertools::intersperse(elements.into_iter(), break_(",", ", ")).collect())
}
