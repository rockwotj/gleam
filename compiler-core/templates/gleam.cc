#include "gleam.h"

namespace gleam {

String MakeString(char* str) { return MakeRef<std::string>(str); }

}  // namespace gleam
