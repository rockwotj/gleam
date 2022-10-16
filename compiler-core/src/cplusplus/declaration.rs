use crate::ast::{Arg, RecordConstructor, RecordConstructorArg, TypedStatement};
use crate::cplusplus::error::Error;
use crate::cplusplus::expression::*;
use crate::cplusplus::scope::LexicalScope;
use crate::cplusplus::INDENT;
use crate::docvec;
use crate::pretty::*;
use crate::type_::Type;
use itertools::Itertools;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Declaration<'a> {
    Class { doc: Document<'a>, public: bool },
    Fn { doc: Document<'a>, public: bool },
    Alias { doc: Document<'a>, public: bool },
    Constant { doc: Document<'a>, public: bool },
}

impl<'a> Declaration<'a> {
    pub fn is_public(&self) -> bool {
        *match self {
            Declaration::Class { public, .. } => public,
            Declaration::Fn { public, .. } => public,
            Declaration::Alias { public, .. } => public,
            Declaration::Constant { public, .. } => public,
        }
    }
    pub fn into_doc(self) -> Document<'a> {
        match self {
            Declaration::Class { doc, .. } => doc,
            Declaration::Fn { doc, .. } => doc,
            Declaration::Alias { doc, .. } => doc,
            Declaration::Constant { doc, .. } => doc,
        }
    }
    pub fn type_order(&self) -> i32 {
        match self {
            Declaration::Constant { .. } => 1,
            Declaration::Class { .. } => 2,
            Declaration::Alias { .. } => 3,
            Declaration::Fn { .. } => 4,
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
            let mut scope = LexicalScope::new_root();
            for arg in arguments {
                if let Some(name) = arg.names.get_variable_name() {
                    let _ = scope.declare_local_var(name, &arg.type_);
                }
            }
            let mut generator = ExpressionGenerator::new(scope);
            let GeneratedExpr { eval, result } = generator.generate_expr(body)?;
            Some(docvec![
                function_signature(name, arguments, return_type),
                " {",
                docvec![line(), eval, docvec!["return ", result, ";"]]
                    .nest(INDENT)
                    .group(),
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

pub(crate) fn function_args(args: &Vec<Arg<Arc<Type>>>) -> Document<'_> {
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
    name: &'a String,
    args: &'a Vec<Arg<Arc<Type>>>,
    return_type: &'a Arc<Type>,
) -> Document<'a> {
    let mut decl = docvec!(transform_type(&return_type), " ");
    decl = decl.append(Document::String(name.clone()));
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
            ..
        } => record_forward_declarations(constructors, name)
            .into_iter()
            .map(|doc| Declaration::Class {
                doc,
                public: *public,
            })
            .collect(),
        TypedStatement::ExternalFn { .. } => vec![],
        TypedStatement::ExternalType { .. } => vec![],
        TypedStatement::ModuleConstant { .. } => vec![],
    })
}

fn record_forward_declarations<'a>(
    variants: &'a Vec<RecordConstructor<Arc<Type>>>,
    name: &'a String,
) -> Vec<Document<'a>> {
    let superclass = if variants.len() > 1 {
        vec![docvec!["class ", name, ";"]]
    } else {
        vec![]
    };
    let classes = variants
        .iter()
        .map(|variant| docvec!["class ", variant.name, ";"])
        .collect();
    vec![superclass, classes].concat()
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
            ..
        } => record_declarations(name, constructors)
            .into_iter()
            .map(|doc| Declaration::Class {
                doc,
                public: *public,
            })
            .collect(),
        TypedStatement::ExternalFn { .. } => vec![],
        TypedStatement::ExternalType { .. } => vec![],
        TypedStatement::ModuleConstant { .. } => vec![],
    })
}

fn record_declarations<'a>(
    name: &'a String,
    variants: &'a Vec<RecordConstructor<Arc<Type>>>,
) -> Vec<Document<'a>> {
    let superclass = if variants.len() > 1 {
        let (first, rest) = variants.split_first().unwrap();
        let shared_arguments: Vec<(String, RecordConstructorArg<Arc<Type>>)> = first
            .arguments
            .iter()
            .enumerate()
            .filter_map(|(idx, arg)| {
                let is_common_arg = rest.iter().all(|other| match other.arguments.get(idx) {
                    None => false,
                    Some(other_arg) => arg.label == other_arg.label && arg.type_ == other_arg.type_,
                });
                match &arg.label {
                    Some(name) if is_common_arg => Some((name.clone(), arg.clone())),
                    _ => None,
                }
            })
            .collect();
        let shared_argument_names: im::HashSet<String> = shared_arguments
            .iter()
            .map(|(name, _)| name.clone())
            .collect();
        Some(SuperClass {
            name: name.clone(),
            shared_arguments,
            shared_argument_names,
        })
    } else {
        None
    };
    let decls: Vec<Document<'a>> = variants
        .iter()
        .map(|variant| record_declaration(&variant.name, &variant.arguments, &superclass))
        .collect();
    vec![
        superclass
            .map(|c| {
                vec![record_declaration(
                    &c.name,
                    &c.shared_arguments.into_iter().map(|(_, v)| v).collect_vec(),
                    &None,
                )]
            })
            .unwrap_or(vec![]),
        decls,
    ]
    .concat()
}

#[derive(Debug, PartialEq)]
struct SuperClass {
    name: String,
    shared_arguments: Vec<(String, RecordConstructorArg<Arc<Type>>)>,
    shared_argument_names: im::HashSet<String>,
}

fn record_declaration<'a, 'b>(
    variant_name: &'a String,
    args: &'a Vec<RecordConstructorArg<Arc<Type>>>,
    supertype: &'a Option<SuperClass>,
) -> Document<'b> {
    let inheritance = match supertype {
        None => "".to_doc(),
        Some(superclass) => docvec![" : public ", Document::String(superclass.name.clone())],
    };
    let super_members = match supertype {
        None => im::HashSet::new(),
        Some(superclass) => superclass.shared_argument_names.clone(),
    };
    let mut constructor_args: Vec<Document<'b>> = vec![];
    let mut super_constructor_args: Vec<Document<'b>> = vec![];
    let mut member_initialization: Vec<Document<'b>> = vec![];
    let mut accessors: Vec<Document<'b>> = vec![];
    let mut members: Vec<Document<'b>> = vec![];
    for (i, arg) in args.iter().enumerate() {
        let constructor_arg_name = Document::String(match &arg.label {
            Some(label) => label.clone(),
            None => format!("x${}", i),
        });
        constructor_args.push(docvec![
            transform_type(&arg.type_),
            " ",
            constructor_arg_name.clone()
        ]);

        let is_shared_member = arg
            .label
            .as_ref()
            .map(|l| super_members.contains(l))
            .unwrap_or(false);
        if is_shared_member {
            super_constructor_args.push(constructor_arg_name);
        } else {
            let accessor_name = match &arg.label {
                Some(label) => label.clone(),
                // TODO: This is not pretty, is there a better pattern here?
                None => format!("x${}", i),
            };
            let member_name = match &arg.label {
                Some(label) => format!("m${}", label.clone()),
                None => format!("m${}", i),
            };
            accessors.push(docvec![
                transform_type(&arg.type_),
                " ",
                Document::String(accessor_name),
                "() const { return ",
                Document::String(member_name.clone()),
                "; }"
            ]);
            members.push(docvec![
                transform_type(&arg.type_),
                " ",
                Document::String(member_name.clone()),
                ";"
            ]);
            member_initialization.push(
                Document::String(member_name).append(constructor_arg_name.surround("(", ")")),
            );
        }
    }
    let constructor_args = Document::Vec(
        Itertools::intersperse(constructor_args.into_iter(), break_(",", ", ")).collect(),
    );
    if let Some(superclass) = supertype {
        let super_initialization = Document::String(superclass.name.clone()).append(
            Document::Vec(
                Itertools::intersperse(super_constructor_args.into_iter(), break_(",", ", "))
                    .collect(),
            )
            .surround("(", ")"),
        );
        member_initialization.insert(0, super_initialization);
    }
    let mut member_initialization = Document::Vec(
        Itertools::intersperse(member_initialization.into_iter(), break_(",", ", ")).collect(),
    );
    if !member_initialization.is_empty() {
        member_initialization = " : ".to_doc().append(member_initialization);
    }
    let accessors = Document::Vec(Itertools::intersperse(accessors.into_iter(), line()).collect());
    let members = Document::Vec(Itertools::intersperse(members.into_iter(), line()).collect());
    docvec![
        "class ",
        Document::String(variant_name.clone()),
        inheritance,
        " {",
        line(),
        "public:",
        docvec![
            line(),
            Document::String(variant_name.clone()),
            "(",
            constructor_args,
            ")",
            member_initialization,
            " {}",
            line(),
            accessors
        ]
        .nest(INDENT)
        .group(),
        line(),
        "private:",
        docvec![line(), members].nest(INDENT).group(),
        line(),
        "};",
    ]
}

pub(crate) fn function_type<'a>(result: Arc<Type>, args: Vec<Arc<Type>>) -> Document<'a> {
    let mut doc = docvec![
        "gleam::Function<",
        transform_type(&result),
        break_(",", ", ")
    ];
    doc = doc.append(Document::Vec(
        Itertools::intersperse(
            args.iter().map(|arg| transform_type(&arg)),
            break_(",", ", "),
        )
        .collect(),
    ));
    return doc.append(">");
}

pub(crate) fn to_symbol<'a, 'b>(
    name: &'a String,
    public: bool,
    module: &'a Vec<String>,
) -> Document<'b> {
    let mut doc = if module.is_empty() {
        "gleam::".to_doc()
    } else {
        Document::String(module.join("::")).surround("::", "::")
    };
    if !public {
        doc = doc.append("_private::");
    }
    doc.append(Document::String(name.clone()))
}

pub(crate) fn transform_type<'a, 'b>(type_: &'a Type) -> Document<'b> {
    return if type_.is_int() {
        "int64_t".to_doc()
    } else if type_.is_bool() {
        "bool".to_doc()
    } else if type_.is_float() {
        "double".to_doc()
    } else {
        match type_ {
            Type::App {
                name,
                public,
                module,
                ..
            } => to_symbol(name, *public, module).surround("gleam::Ref<", ">"),
            Type::Fn { args, retrn } => {
                function_type(retrn.clone(), args.clone())
            }
            Type::Var { .. } => "?".to_doc(),
            Type::Tuple { .. } => "?".to_doc(),
        }
    };
}
