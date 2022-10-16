use crate::assert_cpp;

#[test]
fn boolean_operators() {
    assert_cpp!(
        r#"
pub fn or_op(x, y) {
  x < 5 || y < 10
}
pub fn and_op(x, y) {
  x < 5 && y < 10
}
pub fn both_ops(x, y) {
  { x < 5 || y < 10 } && { x > 5 || y > 10 }
}"#
    );
}

#[test]
fn variables() {
    assert_cpp!(
        r#"
pub fn var_test(a: Float, b: Float, c: Float) -> Float {
  let x = a;
  let y = b;
  let z = c;
  let q = a +. x +. y +. z;
  let v = x +. b +. c;
  v +. q +. z +. y +. x
}
    "#
    )
}

#[test]
fn record_access() {
    assert_cpp!(
        r#"
pub type Person {
  Person(name: String, age: Int)
}

pub fn next_age(person: Person) -> Int {
  person.age + 1
}
    "#
    )
}

#[test]
fn record_construction() {
    assert_cpp!(
        r#"
pub type Person {
  Person(name: String, age: Int)
}

pub fn increment(n: Int) -> Int {
  n + 1
}

pub fn birthday(person: Person) -> Person {
  let next_age = increment(person.age);
  Person(person.name, next_age)
}
    "#
    )
}

#[test]
fn record_construction_labels() {
    assert_cpp!(
        r#"
pub type Person {
  Person(name: String, age: Int)
}

pub fn increment(n: Int) -> Int {
  n + 1
}

pub fn birthday(person: Person) -> Person {
  let next_age = increment(person.age);
  Person(age: next_age, name: person.name)
}
    "#
    )
}
