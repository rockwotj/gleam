use crate::cplusplus::declaration::{
    declarations, forward_declarations, implementation, Declaration,
};
use crate::{
    ast::TypedModule, docvec, io::Utf8Writer, line_numbers::LineNumbers, pretty::*, Error,
};

use crate::ast::TypedStatement;
use itertools::Itertools;
use std::path::Path;

mod declaration;
pub(crate) mod error;
mod expression;
mod keywords;
mod meta;
mod scope;
#[cfg(test)]
mod tests;

pub type Output<'a> = Result<Document<'a>, error::Error>;

const INDENT: isize = 2;

pub const PRELUDE_HEADER: &str = include_str!("../templates/gleam.h");
pub const PRELUDE_IMPL: &str = include_str!("../templates/gleam.cc");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CppStatementType {
    Alias,
    Function,
    Class,
    Include,
    Constant,
}

impl CppStatementType {
    fn classify(statement: &TypedStatement) -> CppStatementType {
        match statement {
            TypedStatement::Fn { .. } => CppStatementType::Function,
            TypedStatement::TypeAlias { .. } => CppStatementType::Alias,
            TypedStatement::CustomType { .. } => CppStatementType::Class,
            TypedStatement::ExternalFn { .. } => CppStatementType::Function,
            TypedStatement::ExternalType { .. } => CppStatementType::Alias,
            TypedStatement::Import { .. } => CppStatementType::Include,
            TypedStatement::ModuleConstant { .. } => CppStatementType::Constant,
        }
    }
}

pub fn module_header(
    module: &TypedModule,
    _line_numbers: &LineNumbers,
    path: &Path,
    src: &str,
    writer: &mut impl Utf8Writer,
) -> Result<(), Error> {
    // TODO: private stuff needs to be grouped into namespaces.
    // Let's hope this stuff is ordered correctly!
    let mut forward_declarations: Vec<Declaration<'_>> = module
        .statements
        .iter()
        .map(|s| forward_declarations(s))
        .flatten_ok()
        .try_collect()
        .map_err(|err| Error::CPlusPlus {
            path: path.to_path_buf(),
            src: src.to_string(),
            error: err,
        })?;

    forward_declarations.sort_by(|a, b| {
        a.type_order()
            .cmp(&b.type_order())
            .then(a.is_public().cmp(&b.is_public()))
    });

    let mut declarations: Vec<Declaration<'_>> = module
        .statements
        .iter()
        .map(|s| declarations(s))
        .flatten_ok()
        .try_collect()
        .map_err(|err| Error::CPlusPlus {
            path: path.to_path_buf(),
            src: src.to_string(),
            error: err,
        })?;

    declarations.sort_by(|a, b| {
        a.type_order()
            .cmp(&b.type_order())
            .then(a.is_public().cmp(&b.is_public()))
    });

    let statements: Vec<Document<'_>> = Itertools::intersperse(
        vec![forward_declarations, declarations]
            .concat()
            .into_iter()
            .map(|d| d.into_doc()),
        lines(2),
    )
    .collect();

    let mut document = Document::Vec(statements);
    document = meta::wrap_with_namespace_scope(document, module);
    document = docvec!("#include <gleam.h>", line()).append(document);
    for import_name in collect_imports(module) {
        let import = if import_name.starts_with("<") && import_name.ends_with(">") {
            Document::String(import_name)
        } else {
            docvec!("\"", Document::String(import_name), "\"")
        };
        document = docvec!("#include ", import, line())
    }
    document = meta::wrap_with_include_guards(document, module);
    document.pretty_print(80, writer)?;
    Ok(())
}

pub fn module_impl(
    module: &TypedModule,
    _line_numbers: &LineNumbers,
    path: &Path,
    src: &str,
    writer: &mut impl Utf8Writer,
) -> Result<(), Error> {
    let mut document = nil();
    let declarations: Vec<Document<'_>> = module
        .statements
        .iter()
        .map(|s| implementation(s))
        .filter_map_ok(std::convert::identity)
        .try_collect()
        .map_err(|err| Error::CPlusPlus {
            path: path.to_path_buf(),
            src: src.to_string(),
            error: err,
        })?;

    document = document.append(Document::Vec(
        Itertools::intersperse(declarations.into_iter(), lines(2)).collect(),
    ));

    let header_name = module.name.last().ok_or(Error::CPlusPlus {
        path: path.to_path_buf(),
        src: src.to_string(),
        error: error::Error::InvalidModuleName,
    })?;
    document = meta::wrap_with_namespace_scope(document, module);
    document = docvec!("#include \"", header_name, ".h\"", line()).append(document);
    document.pretty_print(80, writer)?;
    Ok(())
}

fn collect_imports(module: &TypedModule) -> Vec<String> {
    module
        .statements
        .iter()
        .flat_map(|s| match s {
            TypedStatement::ExternalFn { module, .. } => vec![module.clone()],
            TypedStatement::Import { module, .. } => vec![module.join("/") + ".h"],
            TypedStatement::ExternalType { .. } => vec![],
            TypedStatement::CustomType { .. } => vec![],
            TypedStatement::Fn { .. } => vec![],
            TypedStatement::TypeAlias { .. } => vec![],
            TypedStatement::ModuleConstant { .. } => vec![],
        })
        .collect()
}
