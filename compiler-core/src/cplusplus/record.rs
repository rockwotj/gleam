use crate::ast::RecordConstructor;
use crate::ast::RecordConstructorArg;
use crate::cplusplus::error::Error;
use crate::cplusplus::symbolizer::Symbolizer;
use crate::cplusplus::INDENT;
use crate::docvec;
use crate::pretty::*;
use crate::type_::Type;
use itertools::Itertools;
use std::sync::Arc;

struct SharedFields {
    names: im::HashSet<String>,
    arguments: Vec<(String, RecordConstructorArg<Arc<Type>>)>,
}

enum StructType<'a> {
    SuperType,
    Variant {
        supertype_name: &'a str,
        shared: &'a SharedFields,
    },
}

impl<'a> StructType<'a> {
    fn is_shared_field(&self, name: &'_ str) -> bool {
        match self {
            StructType::SuperType => false,
            StructType::Variant { shared, .. } => shared.names.contains(name),
        }
    }
}

pub(crate) struct StructGenerator {
    symbolizer: Symbolizer,
}

impl StructGenerator {
    pub fn new() -> StructGenerator {
        StructGenerator {
            symbolizer: Symbolizer::new(),
        }
    }

    pub fn forward_declare_record<'a>(
        &mut self,
        name: &'a str,
        constructors: &'a [RecordConstructor<Arc<Type>>],
        typed_parameters: &'a [Arc<Type>],
    ) -> Result<Document<'a>, Error> {
        let template_args = self.symbolizer.template_specification(typed_parameters)?;
        // Declare the type that is used within gleam.
        let mut forward_decls = vec![docvec![template_args.clone(), "struct ", name, ";"]];
        // Declare each constructor variant as it's own struct.
        for constructor in constructors {
            forward_decls.push(docvec![
                template_args.clone(),
                "struct ",
                Document::String(name.to_owned()),
                "$",
                Document::String(constructor.name.to_owned()),
                ";"
            ]);
        }
        Ok(join(forward_decls, line()))
    }

    pub fn generate_record_impl<'a, 'b>(
        &mut self,
        name: &'a str,
        constructors: &'a [RecordConstructor<Arc<Type>>],
        typed_parameters: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        let shared = self.compute_shared_fields(name, constructors)?;
        let supertype = StructType::Variant {
            supertype_name: name,
            shared: &shared,
        };
        let mut docs: Vec<Document<'b>> = constructors
            .iter()
            .map(|constructor| {
                self.record_document(
                    &constructor.name,
                    &supertype,
                    &constructor.arguments,
                    typed_parameters,
                )
            })
            .try_collect()?;
        let super_fields: Vec<_> = shared.arguments.into_iter().map(|(_, arg)| arg).collect();
        docs.insert(
            0,
            self.record_document(
                name,
                &StructType::SuperType,
                &super_fields,
                typed_parameters,
            )?,
        );
        Ok(join(docs, lines(2)))
    }

    fn record_document<'a, 'b>(
        &mut self,
        name: &'a str,
        supertype: &'a StructType<'a>,
        fields: &'a [RecordConstructorArg<Arc<Type>>],
        typed_parameters: &'a [Arc<Type>],
    ) -> Result<Document<'b>, Error> {
        let mut constructor_args: Vec<Document<'b>> = vec![];
        let mut super_constructor_args: Vec<Document<'b>> = vec![];
        let mut member_initializers: Vec<Document<'b>> = vec![];
        let mut members: Vec<Document<'b>> = vec![];
        for (i, field) in fields.iter().enumerate() {
            let name = Document::String(match &field.label {
                Some(label) => label.clone(),
                None => format!("_${}", i),
            });
            let typ = self.symbolizer.type_to_symbol(&field.type_)?;
            constructor_args.push(docvec![typ.clone(), " ", name.clone()]);
            let is_shared_member = field
                .label
                .as_ref()
                .map(|l| supertype.is_shared_field(l))
                .unwrap_or_default();
            if is_shared_member {
                super_constructor_args.push(name);
            } else {
                members.push(docvec![typ.clone(), " ", name.clone(), ";"]);
                member_initializers.push(docvec![name.clone(), "(", name.clone(), ")"]);
            }
        }
        let mut struct_name = Document::String(name.to_owned());
        let mut super_declaration = nil();
        if let StructType::Variant { supertype_name, .. } = supertype {
            let super_name_doc = Document::String((*supertype_name).to_owned());
            let super_type_args = self.symbolizer.app_symbol_args(typed_parameters)?;
            let super_initializer = docvec![
                super_name_doc.clone(),
                super_type_args.clone(),
                "(",
                join(super_constructor_args, break_(",", ", ")),
                ")",
            ];
            member_initializers.insert(0, super_initializer);
            struct_name = struct_name.append(docvec!["$", super_name_doc.clone()]);
            super_declaration = docvec![" : public ", super_name_doc, super_type_args];
        }
        let constructor = docvec![
            "explicit ",
            struct_name.clone(),
            "(",
            join(constructor_args, break_(",", ", ")),
            ")",
            if member_initializers.is_empty() {
                nil()
            } else {
                docvec![" : ", join(member_initializers, break_(",", ", ")),]
            },
            " {}",
        ];
        let destructor = if let StructType::SuperType = supertype {
            docvec!["virtual ~", struct_name.clone(), "() = default;"]
        } else {
            nil()
        };
        Ok(docvec![
            self.symbolizer.template_specification(typed_parameters)?,
            "struct ",
            struct_name,
            super_declaration,
            " {",
            docvec![line(), constructor].nest(INDENT).group(),
            // TODO: Only add the newline if needed
            docvec![line(), destructor].nest(INDENT).group(),
            docvec![line(), join(members, line())].nest(INDENT).group(),
            line(),
            "};"
        ])
    }

    fn compute_shared_fields(
        &mut self,
        name: &str,
        constructors: &[RecordConstructor<Arc<Type>>],
    ) -> Result<SharedFields, Error> {
        let (first, rest) = constructors.split_first().ok_or(Error::InternalError {
            message: format!("No variants for {}", name),
        })?;
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
        Ok(SharedFields {
            arguments: shared_arguments,
            names: shared_argument_names,
        })
    }
}
