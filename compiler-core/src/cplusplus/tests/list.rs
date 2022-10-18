use crate::assert_cpp;

#[test]
fn make_list() {
    assert_cpp!(
        r#"
pub fn palindrome(x: String, y: String, z: String) -> List(String) {
    [x, y, z, y, x]
}"#
    );
}
