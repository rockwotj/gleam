
pub fn add(a: Int, b: Int) -> Int {
  a + b + a
}

pub type Box(inner_type) {
  Box(inner: inner_type)
}

pub fn make_box(v: inner_type) -> Box(inner_type) {
  Box(v)
}

pub type Either(left, right) {
  Left(inner: left)
  Right(inner: right)
}

pub fn make_left(v: left) {
  Left(v)
}

pub fn make_right(v: right) {
  Right(v)
}

pub fn main() {
  1;
}
