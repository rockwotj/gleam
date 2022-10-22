use crate::assert_cpp;

#[test]
fn exported_functions() {
    assert_cpp!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
    );
}

#[test]
fn private_functions() {
    assert_cpp!(
        r#"
fn add(x, y) {
    x + y
}"#,
    );
}

#[test]
fn anonymous_functions() {
    assert_cpp!(
        r#"
fn squared(x) {
  let y = fn(z) { z * z }
  y(x)
}
    "#
    )
}

#[test]
fn captured_variables() {
    assert_cpp!(
        r#"
fn do_stuff(x, y) {
  let f = fn(z) { { x * y } + z }
  f(x + y)
}
    "#
    )
}

#[test]
fn multiple_module_functions() {
    assert_cpp!(
        r#"
fn add(x, y) {
  x + y
}
fn subtract(x, y) {
  add(x, 0 - y)
}
    "#
    )
}

#[test]
fn generic_function() {
    assert_cpp!(
        r#"
fn identity(x) {
  x
}
fn wrapped(x) {
  identity(x)
}
        "#
        )
}

#[test]
fn specialized() {
    assert_cpp!(
        r#"
fn identity(x) {
  x
}
fn add(a, b) {
  identity(a) + identity(b)
}
        "#
        )
}
