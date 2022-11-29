use crate::ast::{Arg, TypedStatement};
use crate::cplusplus::error::Error;
use crate::cplusplus::expression::*;
use crate::cplusplus::INDENT;
use crate::docvec;
use crate::ir::IntermediateRepresentationConverter;
use crate::pretty::*;
use crate::type_::{Type, TypeVar};
use itertools::Itertools;
use std::{cell::RefCell, ops::Deref, sync::Arc};

use super::record::StructGenerator;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Declaration<'a> {
    Class { doc: Document<'a>, public: bool },
    Fn { doc: Document<'a>, public: bool },
    // TODO: Support me!
    // Alias { doc: Document<'a>, public: bool },
    // Constant { doc: Document<'a>, public: bool },
}

impl<'a> Declaration<'a> {
    pub fn is_public(&self) -> bool {
        *match self {
            Declaration::Class { public, .. } => public,
            Declaration::Fn { public, .. } => public,
            // Declaration::Alias { public, .. } => public,
            // Declaration::Constant { public, .. } => public,
        }
    }
    pub fn into_doc(self) -> Document<'a> {
        match self {
            Declaration::Class { doc, .. } => doc,
            Declaration::Fn { doc, .. } => doc,
            // Declaration::Alias { doc, .. } => doc,
            // Declaration::Constant { doc, .. } => doc,
        }
    }
    pub fn type_order(&self) -> i32 {
        match self {
            Declaration::Class { .. } => 2,
            Declaration::Fn { .. } => 4,
            // Declaration::Constant { .. } => 1,
            // Declaration::Alias { .. } => 3,
        }
    }
}

pub(crate) fn implementation(statement: &TypedStatement) -> Result<Option<Document<'_>>, Error> {
    Ok(match statement {
        TypedStatement::Fn {
            name,
            arguments,
            return_type,
            body,
            ..
        } => {

            let mut ir_generator = IntermediateRepresentationConverter::new_for_function(arguments);
            let ir = ir_generator.ast_to_ir(body);

            let mut generator = NativeIrCodeGenerator::new();
            let doc = generator.ir_to_doc(ir)?;

            Some(docvec![
                function_signature(name, arguments, return_type),
                " {",
                line().append(doc).nest(INDENT).group(),
                line(),
                "};"
            ])
        }
        TypedStatement::TypeAlias { .. } => None,
        TypedStatement::CustomType { .. } => None,
        TypedStatement::ExternalFn { .. } => None,
        TypedStatement::ExternalType { .. } => None,
        TypedStatement::Import { .. } => None,
        TypedStatement::ModuleConstant { .. } => None,
    })
}

pub(crate) fn function_args(args: &[Arg<Arc<Type>>]) -> Document<'_> {
    let mut i = 0;
    let args = args.iter().map(|arg| {
        docvec!(
            transform_type(&arg.type_),
            " ",
            match arg.names.get_variable_name() {
                Some(x) => x.to_doc(),
                None => {
                    // Handle duplicates
                    let name = if i == 0 {
                        "_".to_doc()
                    } else {
                        Document::String(format!("_${}", i))
                    };
                    i += 1;
                    name
                }
            },
        )
    });
    Document::Vec(Itertools::intersperse(args, break_(",", ", ")).collect())
}

fn function_signature<'a>(
    name: &'a str,
    args: &'a [Arg<Arc<Type>>],
    return_type: &'a Arc<Type>,
) -> Document<'a> {
    let mut all_types: Vec<_> = args.iter().map(|a| a.type_.clone()).collect();
    all_types.push(return_type.clone());
    let template_args = generate_template_declaration(&all_types);
    let mut decl = docvec![template_args, transform_type(return_type), " "];
    decl = decl.append(Document::String(name.to_owned()));
    return decl.append(function_args(args).surround("(", ")"));
}

pub(crate) fn forward_declarations(
    statement: &TypedStatement,
) -> Result<Vec<Declaration<'_>>, Error> {
    Ok(match statement {
        TypedStatement::Import { .. } => vec![],
        TypedStatement::Fn {
            name,
            arguments,
            return_type,
            public,
            ..
        } => vec![Declaration::Fn {
            doc: docvec!(function_signature(name, arguments, return_type), ";"),
            public: *public,
        }],
        TypedStatement::TypeAlias { .. } => vec![],
        TypedStatement::CustomType {
            name,
            constructors,
            public,
            typed_parameters,
            ..
        } => {
            let mut gen = StructGenerator::new();
            let doc = gen.forward_declare_record(name, constructors, typed_parameters)?;
            vec![Declaration::Class {
                doc,
                public: *public
            }]
        },
        TypedStatement::ExternalFn { .. } => vec![],
        TypedStatement::ExternalType { .. } => vec![],
        TypedStatement::ModuleConstant { .. } => vec![],
    })
}

pub(crate) fn declarations(statement: &TypedStatement) -> Result<Vec<Declaration<'_>>, Error> {
    Ok(match statement {
        TypedStatement::Import { .. } => vec![],
        TypedStatement::Fn { .. } => vec![],
        TypedStatement::TypeAlias { .. } => vec![],
        TypedStatement::CustomType {
            name,
            constructors,
            public,
            typed_parameters,
            ..
        } => {
            let mut gen = StructGenerator::new();
            let doc = gen.generate_record_impl(name, constructors, typed_parameters)?;
            vec![Declaration::Class {
                doc,
                public: *public
            }]
        },
        TypedStatement::ExternalFn { .. } => vec![],
        TypedStatement::ExternalType { .. } => vec![],
        TypedStatement::ModuleConstant { .. } => vec![],
    })
}

fn generate_template_declaration<'a, 'b>(typed_parameters: &'a [Arc<Type>]) -> Document<'b> {
    let generic_args: Vec<_> = typed_parameters
        .iter()
        .flat_map(|p| p.type_vars())
        .filter(|type_var| !type_var.borrow().is_link())
        .map(|type_var| docvec!["typename ", transform_type(&Type::Var { type_: type_var })])
        .unique()
        .collect();
    if generic_args.is_empty() {
        nil()
    } else {
        Document::Vec(Itertools::intersperse(generic_args.into_iter(), break_(",", ", ")).collect())
            .surround("template <", ">")
            .append(line())
    }
}

fn generate_template_args<'a, 'b>(typed_parameters: &'a [Arc<Type>]) -> Document<'b> {
    let generic_args: Vec<_> = typed_parameters
        .iter()
        .flat_map(|p| p.type_vars())
        .map(|type_var| transform_type(&Type::Var { type_: type_var }))
        .unique()
        .collect();
    if generic_args.is_empty() {
        nil()
    } else {
        Document::Vec(Itertools::intersperse(generic_args.into_iter(), break_(",", ", ")).collect())
            .surround("<", ">")
    }
}

pub(crate) fn function_type<'a>(result: Arc<Type>, args: Vec<Arc<Type>>) -> Document<'a> {
    let mut doc = docvec![
        "gleam::Function<",
        transform_type(&result),
        break_(",", ", ")
    ];
    doc = doc.append(Document::Vec(
        Itertools::intersperse(
            args.iter().map(|arg| transform_type(arg)),
            break_(",", ", "),
        )
        .collect(),
    ));
    doc.append(">")
}

pub(crate) fn to_symbol<'a, 'b>(
    name: &'a str,
    public: bool,
    module: &'a Vec<String>,
    args: &'a [Arc<Type>],
) -> Document<'b> {
    let mut doc = if module.is_empty() {
        "gleam::".to_doc()
    } else {
        Document::String(module.join("::")).surround("::", "::")
    };
    if !public {
        doc = doc.append("_private::");
    }
    docvec![
        doc,
        Document::String(name.to_owned()),
        generate_template_args(args),
    ]
}

pub(crate) fn transform_type<'a, 'b>(type_: &'a Type) -> Document<'b> {
    return if type_.is_int() {
        "int64_t".to_doc()
    } else if type_.is_bool() {
        "bool".to_doc()
    } else if type_.is_float() {
        "double".to_doc()
    } else if type_.is_string() {
        "gleam::String".to_doc()
    } else {
        match type_ {
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
                to_symbol(name, *public, module, &linked).surround("gleam::Ref<", ">")
            }
            Type::Fn { args, retrn } => function_type(retrn.clone(), args.clone()),
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_: typ } => transform_type(typ),
                TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                    generate_generic_type_param(*id)
                }
            },
            Type::Tuple { .. } => "?".to_doc(),
        }
    };
}

fn generate_generic_type_param<'a>(id: u64) -> Document<'a> {
    Document::String(format!("T${}", id))
}

