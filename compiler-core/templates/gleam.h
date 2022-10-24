#ifndef GLEAM_PRELUDE_H_
#define GLEAM_PRELUDE_H_

#include <stdint.h>

#include <functional>
#include <initializer_list>
#include <memory>
#include <string>

/// This namespace defines the prelude for Native Gleam.
namespace gleam {

template <typename T>
using Ref = std::shared_ptr<T>;
template <typename T, typename... Args>
Ref<T> MakeRef(Args&&... args) {
  return std::make_shared<T>(args...);
}
using String = Ref<std::string>;
String MakeString(char* str);

/// A base type for all anonymous or referenced functions declared in Native
/// Gleam
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

template <typename ConstructorType, typename... Args>
Function<Ref<ConstructorType>, Args...> WrappedConstructor() {
  return [](Args... args) -> Ref<ConstructorType> {
    return MakeRef<ConstructorType>(args...);
  };
}

template <typename T>
class List {
 public:
  virtual ~List() = default;

  static Ref<List<T>> empty();
};

template <typename T>
class NonEmptyList : public List<T> {
 public:
  NonEmptyList(T head, Ref<List<T>> tail)
      : List<T>(), head_(std::move(head)), tail_(std::move(tail)) {}

  const T& head() const { return head_; }

  const Ref<List<T>>& tail() const { return tail_; }

 private:
  T head_;
  Ref<List<T>> tail_;
};

template <typename T>
class EmptyList : public List<T> {};

template <typename T>
Ref<List<T>> List<T>::empty() {
  static Ref<List<T>> instance = []() { return MakeRef<EmptyList<T>>(); }();
  return instance;
}

template <typename T>
Ref<List<T>> MakeList(std::initializer_list<T> list,
                      Ref<List<T>> init = List<T>::empty()) {
  Ref<List<T>> result = init;
  for (auto it = std::rbegin(list); it != std::rend(list); ++it) {
    result = MakeRef<NonEmptyList<T>>(*it, result);
  }
  return result;
}

}  // namespace gleam

#endif  // GLEAM_PRELUDE_H_
