use crate::cplusplus::error::Error;
use crate::docvec;
use crate::pretty::*;
use crate::type_::{Type, TypeVar};
use itertools::Itertools;
use std::{cell::RefCell, ops::Deref, sync::Arc};

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
                    // Sort of a hack - but force the specification here for template args.
                    let linked: Vec<_> = args
                        .iter()
                        .map(|a| match a.as_ref() {
                            Type::Var { .. } => a.clone(),
                            _ => Arc::new(Type::Var {
                                type_: Arc::new(RefCell::new(TypeVar::Link { type_: a.clone() })),
                            }),
                        })
                        .collect();
                    let sym = self.to_app_symbol(name, *public, &module[..], &linked)?;
                    sym.surround("gleam::Ref<", ">")
                }
                Type::Fn { args, retrn } => self.function_type(retrn, args)?,
                Type::Var { type_ } => match type_.borrow().deref() {
                    TypeVar::Link { type_: typ } => self.to_symbol(&typ)?,
                    TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                        self.generate_generic_type_param(*id)
                    }
                },
                Type::Tuple { .. } => todo!("tuple types"),
            }
        });
    }

    pub fn function_type<'a, 'b>(&mut self, result: &'a Type, args: &'a [Arc<Type>]) -> Result<Document<'b>, Error> {
        let mut doc = docvec![
            "gleam::Function<",
            self.to_symbol(&result)?,
            break_(",", ", ")
        ];
        doc = doc.append(
                comma_seperate(
                    args.iter().map(|arg| self.to_symbol(arg)).try_collect()?
                    )
                );
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
            // generate_template_args(args),
        ])
    }

    fn generate_generic_type_param<'a>(&mut self, id: u64) -> Document<'a> {
        Document::String(format!("T${}", id))
    }
}

fn comma_seperate(elements: Vec<Document<'_>>) -> Document<'_> {
    Document::Vec(Itertools::intersperse(elements.into_iter(), break_(",", ", ")).collect())
}
