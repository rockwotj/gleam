#ifndef GLEAM_PRELUDE_H_
#define GLEAM_PRELUDE_H_

#include <string>
#include <stdint.h>
#include <memory>
#include <functional>

/// This namespace defines the prelude for Native Gleam.
namespace gleam {

using String = std::string;
template<typename T>
using Ref = std::shared_ptr<T>;
template<typename T, typename... Args>
Ref<T> MakeRef(Args&&... args) {
  return std::make_shared<T>(args...);
}

/// A base type for all anonymous or referenced functions declared in Native Gleam
///
/// \tparam ReturnType The result of the function.
/// \tparam Args The arguments to the function.
/// Example translation:
/// \code
/// ```gleam
/// let adder = fn(a: Int, b: Int) -> Int { a + b }
/// ```
///
/// ```cpp
/// [=](int64_t a, int64_t b) -> int64_t {
///     return a + b;
///   }
/// }
/// ```
/// \endcode
template <typename ReturnType, typename... Args>
using Function = std::function<ReturnType(Args...)>;

namespace _private {

template <typename ConstructorType>, typename... Args>
class ConstructorWrapper : public Function<Ref<ReturnType>, Args...> {
public:
  explicit ConstructorWrapper() {}

  Ref<ReturnType> operator()(Args... args) const override {
    return MakeRef<ReturnType>(args...);
  }
};

}  // namespace _private


template <typename ConstructorType, typename... Args>
Ref<Function<Ref<ConstructorType>, Args>> WrappedConstructor() {
  Function<Ref<ReturnType>, Args...>* wrapped = new _private::ConstructorWrapper<ReturnType, Args...>();
  return Ref<Function<ReturnType, Args...>>(wrapped);
}

}  // namespace gleam

#endif // GLEAM_PRELUDE_H_
