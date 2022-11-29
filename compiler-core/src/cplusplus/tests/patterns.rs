use crate::assert_cpp;

#[test]
fn bool_pattern() {
    assert_cpp!(
        r#"
pub fn negate(b: Bool) -> Bool {
    case b {
      True -> False
      False -> True
    }
}"#
    );
}
