use crate::assert_cpp;

#[test]
fn single_variant() {
    assert_cpp!(
        r#"
pub type Person { Person(name: String, age: Int) }
"#,
    );
}

#[test]
fn unnamed_fields() {
    assert_cpp!(
        r#"
pub type Ip { Ip(String) }
"#,
    );
}

#[test]
fn multiple_variants() {
    assert_cpp!(
        r#"
pub type User {
  LoggedIn(name: String)
  Guest
}
"#,
    );
}

#[test]
fn shared_fields() {
    assert_cpp!(
        r#"
pub type Animal {
  Cat(name: String, likes_milk: Bool)
  Dog(name: String, barks: Bool)
}
"#,
    );
}

#[test]
fn generic_single_variant() {
    assert_cpp!(
        r#"
pub type Box(inner_type) {
  Box(inner: inner_type)
}
"#,
    );
}

#[test]
fn generic_multiple_variant() {
    assert_cpp!(
        r#"
pub type Either(left_type, right_type) {
  Left(v: left_type)
  Right(v: right_type)
}
"#,
    );
}

#[test]
fn singleton() {
    assert_cpp!(
        r#"
pub type Void { Void }

fn noop() -> Void {
  let v = Void;
  v
}
"#,
    );
}

#[test]
fn constructor_fn() {
    assert_cpp!(
        r#"
pub type Person { Person(name: String) }

fn new(name: String) -> Person {
  let constructor_fn = Person;
  constructor_fn(name)
}
"#,
    );
}
