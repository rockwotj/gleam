pub type Box(inner_type) {
  Box(inner: inner_type)
}

pub fn make_string_box(v: String) -> Box(String) {
  let r = Box(v)
  r
}

pub fn make_string_list(v: String) -> List(String) {
  let r = [v]
  r
}

pub fn main(_args: List(String)) -> Int {
  0
}
