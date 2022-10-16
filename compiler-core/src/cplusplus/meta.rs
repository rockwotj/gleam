use crate::ast::TypedModule;
use crate::cplusplus::keywords::to_identifier;
use crate::docvec;
use crate::pretty::*;
use heck::{ToShoutySnakeCase, ToSnakeCase};

pub fn wrap_with_namespace_scope<'a>(doc: Document<'a>, module: &'a TypedModule) -> Document<'a> {
    let mut head = line();
    let mut tail = line();
    for module_name_part in &module.name {
        let doc_part = Document::String(to_identifier(module_name_part.to_snake_case()));
        head = head
            .append("namespace ")
            .append(doc_part.clone())
            .append(" {")
            .append(line());
        tail = tail
            .append("} // namespace ")
            .append(doc_part)
            .append(line());
    }
    doc.surround(head.append(line()), line().append(tail))
}

pub fn wrap_with_include_guards<'a>(doc: Document<'a>, module: &'a TypedModule) -> Document<'a> {
    let module_name = module.name.join("_").to_shouty_snake_case();
    let name_doc = Document::String(module_name.clone());
    let package_name = module.type_info.package.to_shouty_snake_case();
    let package_doc = Document::String(package_name);
    let head = docvec!(
        "#ifndef ",
        package_doc.clone(),
        "_",
        name_doc.clone(),
        "_H_",
        line(),
        "#define ",
        package_doc.clone(),
        "_",
        name_doc.clone(),
        "_H_",
        lines(2)
    );
    let tail = docvec!(
        line(),
        "#endif // ",
        package_doc,
        "_",
        name_doc,
        "_H_",
        line()
    );
    doc.surround(head, tail)
}
