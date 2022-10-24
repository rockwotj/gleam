#include <gleam.h>

#include "hello_world.h"

namespace {

/**
 * Actually the gleam run main. We support multiple variants of main, examples:
 *
 * ```gleam
 * pub fn main() -> Int
 * pub fn main(args: List(String)) -> Int
 * ```
 *
 * TODO: Support the following variants:
 * ```gleam
 * pub fn main() -> Nil
 * pub fn main(args: List(String)) -> Nil
 * pub fn main() -> Result(Nil, Int) // Or maybe support any Result type?
 * pub fn main(args: List(String)) -> Result(Nil, Int)
 * ```
 */
template <typename MainFn>
int RunMain(MainFn main_fn, int argc, char** argv);

template <>
[[maybe_unused]] int RunMain(int64_t (*main_fn)(), int, char**) {
  return static_cast<int>(main_fn());
}

template <>
[[maybe_unused]] int RunMain(
    int64_t (*main_fn)(gleam::Ref<gleam::List<gleam::String>>), int argc,
    char** argv) {
  gleam::Ref<gleam::List<gleam::String>> gleam_args =
      gleam::List<gleam::String>::empty();
  for (int i = argc; i > 0; --i) {
    gleam::String arg = gleam::MakeString(argv[i - 1]);
    gleam_args =
        gleam::MakeRef<gleam::NonEmptyList<gleam::String>>(arg, gleam_args);
  }
  return static_cast<int>(main_fn(gleam_args));
}

}  // namespace

int main(int argc, char** argv) {
  return RunMain(::hello_world::main, argc, argv);
}
