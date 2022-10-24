mod expression;
mod functions;
mod list;
mod records;

#[macro_export]
macro_rules! assert_cpp {
    ($src:expr $(,)?) => {{
        use crate::{
            build::Origin,
            cplusplus::{module_header, module_impl},
            line_numbers::LineNumbers,
            type_::{build_prelude, infer_module},
            uid::UniqueIdGenerator,
        };
        use std::path::Path;
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "module".to_string()];
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let ast = infer_module(
            crate::build::Target::Native,
            &ids,
            ast,
            Origin::Src,
            "my_package",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module_header(&ast, &line_numbers, Path::new("test.h"), $src, &mut output).unwrap();
        output.push_str("\n---\n");
        module_impl(&ast, &line_numbers, Path::new("test.cc"), $src, &mut output).unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}
