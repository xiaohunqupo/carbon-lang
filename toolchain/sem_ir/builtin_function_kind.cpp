// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/sem_ir/builtin_function_kind.h"

#include <utility>

#include "toolchain/sem_ir/file.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::SemIR {

// A function that validates that a builtin was declared properly.
using ValidateFn = auto(const File& sem_ir, llvm::ArrayRef<TypeId> arg_types,
                        TypeId return_type) -> bool;

namespace {
// Information about a builtin function.
struct BuiltinInfo {
  llvm::StringLiteral name;
  ValidateFn* validate;
};

// The maximum number of type parameters any builtin needs.
constexpr int MaxTypeParams = 2;

// State used when validating a builtin signature that persists between
// individual checks.
struct ValidateState {
  // The type values of type parameters in the builtin signature. Invalid if
  // either no value has been deduced yet or the parameter is not used.
  TypeId type_params[MaxTypeParams] = {TypeId::None, TypeId::None};
};

// Constraint that a type is generic type parameter `I` of the builtin,
// satisfying `TypeConstraint`. See ValidateSignature for details.
template <int I, typename TypeConstraint>
struct TypeParam {
  static_assert(I >= 0 && I < MaxTypeParams);

  static auto Check(const File& sem_ir, ValidateState& state, TypeId type_id)
      -> bool {
    if (state.type_params[I].has_value() && type_id != state.type_params[I]) {
      return false;
    }
    if (!TypeConstraint::Check(sem_ir, state, type_id)) {
      return false;
    }
    state.type_params[I] = type_id;
    return true;
  }
};

// Constraint that a type is a specific builtin. See ValidateSignature for
// details.
template <const InstId& BuiltinId>
struct BuiltinType {
  static auto Check(const File& sem_ir, ValidateState& /*state*/,
                    TypeId type_id) -> bool {
    return sem_ir.types().GetInstId(type_id) == BuiltinId;
  }
};

// Constraint that a type is `()`, used as the return type of builtin functions
// with no return value.
struct NoReturn {
  static auto Check(const File& sem_ir, ValidateState& /*state*/,
                    TypeId type_id) -> bool {
    auto tuple = sem_ir.types().TryGetAs<SemIR::TupleType>(type_id);
    if (!tuple) {
      return false;
    }
    return sem_ir.type_blocks().Get(tuple->elements_id).empty();
  }
};

// Constraint that a type is `bool`.
using Bool = BuiltinType<BoolType::SingletonInstId>;

// Constraint that requires the type to be a sized integer type.
struct AnySizedInt {
  static auto Check(const File& sem_ir, ValidateState& /*state*/,
                    TypeId type_id) -> bool {
    return sem_ir.types().Is<IntType>(type_id);
  }
};

// Constraint that requires the type to be an integer type.
struct AnyInt {
  static auto Check(const File& sem_ir, ValidateState& state, TypeId type_id)
      -> bool {
    return AnySizedInt::Check(sem_ir, state, type_id) ||
           BuiltinType<IntLiteralType::SingletonInstId>::Check(sem_ir, state,
                                                               type_id);
  }
};

// Constraint that requires the type to be a float type.
struct AnyFloat {
  static auto Check(const File& sem_ir, ValidateState& state, TypeId type_id)
      -> bool {
    if (BuiltinType<LegacyFloatType::SingletonInstId>::Check(sem_ir, state,
                                                             type_id)) {
      return true;
    }
    return sem_ir.types().Is<FloatType>(type_id);
  }
};

// Constraint that requires the type to be the type type.
using Type = BuiltinType<TypeType::SingletonInstId>;

// Constraint that requires the type to be a type value, whose type is type
// type. Also accepts symbolic constant value types.
struct AnyType {
  static auto Check(const File& sem_ir, ValidateState& state, TypeId type_id)
      -> bool {
    if (BuiltinType<TypeType::SingletonInstId>::Check(sem_ir, state, type_id)) {
      return true;
    }
    return sem_ir.types().GetAsInst(type_id).type_id() ==
           TypeType::SingletonTypeId;
  }
};

// Checks that the specified type matches the given type constraint.
template <typename TypeConstraint>
auto Check(const File& sem_ir, ValidateState& state, TypeId type_id) -> bool {
  while (type_id.has_value()) {
    // Allow a type that satisfies the constraint.
    if (TypeConstraint::Check(sem_ir, state, type_id)) {
      return true;
    }

    // Also allow a class type that adapts a matching type.
    auto class_type = sem_ir.types().TryGetAs<ClassType>(type_id);
    if (!class_type) {
      break;
    }
    type_id = sem_ir.classes()
                  .Get(class_type->class_id)
                  .GetAdaptedType(sem_ir, class_type->specific_id);
  }
  return false;
}

}  // namespace

// Validates that this builtin has a signature matching the specified signature.
//
// `SignatureFnType` is a C++ function type that describes the signature that is
// expected for this builtin. For example, `auto (AnyInt, AnyInt) -> AnyInt`
// specifies that the builtin takes values of two integer types and returns a
// value of a third integer type. Types used within the signature should provide
// a `Check` function that validates that the Carbon type is expected:
//
//   auto Check(const File&, ValidateState&, TypeId) -> bool;
//
// To constrain that the same type is used in multiple places in the signature,
// `TypeParam<I, T>` can be used. For example:
//
//   auto (TypeParam<0, AnyInt>, AnyInt) -> TypeParam<0, AnyInt>
//
// describes a builtin that takes two integers, and whose return type matches
// its first parameter type. For convenience, typedefs for `TypeParam<I, T>`
// are used in the descriptions of the builtins.
template <typename SignatureFnType>
static auto ValidateSignature(const File& sem_ir,
                              llvm::ArrayRef<TypeId> arg_types,
                              TypeId return_type) -> bool {
  using SignatureTraits = llvm::function_traits<SignatureFnType*>;
  ValidateState state;

  // Must have expected number of arguments.
  if (arg_types.size() != SignatureTraits::num_args) {
    return false;
  }

  // Argument types must match.
  if (![&]<size_t... Indexes>(std::index_sequence<Indexes...>) {
        return ((Check<typename SignatureTraits::template arg_t<Indexes>>(
                    sem_ir, state, arg_types[Indexes])) &&
                ...);
      }(std::make_index_sequence<SignatureTraits::num_args>())) {
    return false;
  }

  // Result type must match.
  if (!Check<typename SignatureTraits::result_t>(sem_ir, state, return_type)) {
    return false;
  }

  return true;
}

// Descriptions of builtin functions follow. For each builtin, a corresponding
// `BuiltinInfo` constant is declared describing properties of that builtin.
namespace BuiltinFunctionInfo {

// Convenience name used in the builtin type signatures below for a first
// generic type parameter that is constrained to be an integer type.
using IntT = TypeParam<0, AnyInt>;

// Convenience name used in the builtin type signatures below for a second
// generic type parameter that is constrained to be an integer type.
using IntU = TypeParam<1, AnyInt>;

// Convenience name used in the builtin type signatures below for a first
// generic type parameter that is constrained to be a sized integer type.
using SizedIntT = TypeParam<0, AnySizedInt>;

// Convenience name used in the builtin type signatures below for a first
// generic type parameter that is constrained to be an float type.
using FloatT = TypeParam<0, AnyFloat>;

// Not a builtin function.
constexpr BuiltinInfo None = {"", nullptr};

// Prints a single character.
constexpr BuiltinInfo PrintChar = {
    "print.char", ValidateSignature<auto(AnySizedInt)->AnySizedInt>};

// Prints an integer.
constexpr BuiltinInfo PrintInt = {
    "print.int", ValidateSignature<auto(AnySizedInt)->NoReturn>};

// Reads a single character from stdin.
constexpr BuiltinInfo ReadChar = {"read.char",
                                  ValidateSignature<auto()->AnySizedInt>};

// Returns the `Core.IntLiteral` type.
constexpr BuiltinInfo IntLiteralMakeType = {"int_literal.make_type",
                                            ValidateSignature<auto()->Type>};

// Returns the `iN` type.
// TODO: Should we use a more specific type as the type of the bit width?
constexpr BuiltinInfo IntMakeTypeSigned = {
    "int.make_type_signed", ValidateSignature<auto(AnyInt)->Type>};

// Returns the `uN` type.
constexpr BuiltinInfo IntMakeTypeUnsigned = {
    "int.make_type_unsigned", ValidateSignature<auto(AnyInt)->Type>};

// Returns float types, such as `f64`. Currently only supports `f64`.
constexpr BuiltinInfo FloatMakeType = {"float.make_type",
                                       ValidateSignature<auto(AnyInt)->Type>};

// Returns the `bool` type.
constexpr BuiltinInfo BoolMakeType = {"bool.make_type",
                                      ValidateSignature<auto()->Type>};

// Converts between integer types, truncating if necessary.
constexpr BuiltinInfo IntConvert = {"int.convert",
                                    ValidateSignature<auto(AnyInt)->AnyInt>};

// Converts between integer types, with a diagnostic if the value doesn't fit.
constexpr BuiltinInfo IntConvertChecked = {
    "int.convert_checked", ValidateSignature<auto(AnyInt)->AnyInt>};

// "int.snegate": integer negation.
constexpr BuiltinInfo IntSNegate = {"int.snegate",
                                    ValidateSignature<auto(IntT)->IntT>};

// "int.sadd": integer addition.
constexpr BuiltinInfo IntSAdd = {"int.sadd",
                                 ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.ssub": integer subtraction.
constexpr BuiltinInfo IntSSub = {"int.ssub",
                                 ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.smul": integer multiplication.
constexpr BuiltinInfo IntSMul = {"int.smul",
                                 ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.sdiv": integer division.
constexpr BuiltinInfo IntSDiv = {"int.sdiv",
                                 ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.smod": integer modulo.
constexpr BuiltinInfo IntSMod = {"int.smod",
                                 ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.unegate": unsigned integer negation.
constexpr BuiltinInfo IntUNegate = {
    "int.unegate", ValidateSignature<auto(SizedIntT)->SizedIntT>};

// "int.uadd": unsigned integer addition.
constexpr BuiltinInfo IntUAdd = {
    "int.uadd", ValidateSignature<auto(SizedIntT, SizedIntT)->SizedIntT>};

// "int.usub": unsigned integer subtraction.
constexpr BuiltinInfo IntUSub = {
    "int.usub", ValidateSignature<auto(SizedIntT, SizedIntT)->SizedIntT>};

// "int.umul": unsigned integer multiplication.
constexpr BuiltinInfo IntUMul = {
    "int.umul", ValidateSignature<auto(SizedIntT, SizedIntT)->SizedIntT>};

// "int.udiv": unsigned integer division.
constexpr BuiltinInfo IntUDiv = {
    "int.udiv", ValidateSignature<auto(SizedIntT, SizedIntT)->SizedIntT>};

// "int.mod": integer modulo.
constexpr BuiltinInfo IntUMod = {
    "int.umod", ValidateSignature<auto(SizedIntT, SizedIntT)->SizedIntT>};

// "int.complement": integer bitwise complement.
constexpr BuiltinInfo IntComplement = {"int.complement",
                                       ValidateSignature<auto(IntT)->IntT>};

// "int.and": integer bitwise and.
constexpr BuiltinInfo IntAnd = {"int.and",
                                ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.or": integer bitwise or.
constexpr BuiltinInfo IntOr = {"int.or",
                               ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.xor": integer bitwise xor.
constexpr BuiltinInfo IntXor = {"int.xor",
                                ValidateSignature<auto(IntT, IntT)->IntT>};

// "int.left_shift": integer left shift.
constexpr BuiltinInfo IntLeftShift = {
    "int.left_shift", ValidateSignature<auto(IntT, IntU)->IntT>};

// "int.left_shift": integer right shift.
constexpr BuiltinInfo IntRightShift = {
    "int.right_shift", ValidateSignature<auto(IntT, IntU)->IntT>};

// "int.eq": integer equality comparison.
constexpr BuiltinInfo IntEq = {"int.eq",
                               ValidateSignature<auto(IntT, IntU)->Bool>};

// "int.neq": integer non-equality comparison.
constexpr BuiltinInfo IntNeq = {"int.neq",
                                ValidateSignature<auto(IntT, IntU)->Bool>};

// "int.less": integer less than comparison.
constexpr BuiltinInfo IntLess = {"int.less",
                                 ValidateSignature<auto(IntT, IntU)->Bool>};

// "int.less_eq": integer less than or equal comparison.
constexpr BuiltinInfo IntLessEq = {"int.less_eq",
                                   ValidateSignature<auto(IntT, IntU)->Bool>};

// "int.greater": integer greater than comparison.
constexpr BuiltinInfo IntGreater = {"int.greater",
                                    ValidateSignature<auto(IntT, IntU)->Bool>};

// "int.greater_eq": integer greater than or equal comparison.
constexpr BuiltinInfo IntGreaterEq = {
    "int.greater_eq", ValidateSignature<auto(IntT, IntU)->Bool>};

// "float.negate": float negation.
constexpr BuiltinInfo FloatNegate = {"float.negate",
                                     ValidateSignature<auto(FloatT)->FloatT>};

// "float.add": float addition.
constexpr BuiltinInfo FloatAdd = {
    "float.add", ValidateSignature<auto(FloatT, FloatT)->FloatT>};

// "float.sub": float subtraction.
constexpr BuiltinInfo FloatSub = {
    "float.sub", ValidateSignature<auto(FloatT, FloatT)->FloatT>};

// "float.mul": float multiplication.
constexpr BuiltinInfo FloatMul = {
    "float.mul", ValidateSignature<auto(FloatT, FloatT)->FloatT>};

// "float.div": float division.
constexpr BuiltinInfo FloatDiv = {
    "float.div", ValidateSignature<auto(FloatT, FloatT)->FloatT>};

// "float.eq": float equality comparison.
constexpr BuiltinInfo FloatEq = {"float.eq",
                                 ValidateSignature<auto(FloatT, FloatT)->Bool>};

// "float.neq": float non-equality comparison.
constexpr BuiltinInfo FloatNeq = {
    "float.neq", ValidateSignature<auto(FloatT, FloatT)->Bool>};

// "float.less": float less than comparison.
constexpr BuiltinInfo FloatLess = {
    "float.less", ValidateSignature<auto(FloatT, FloatT)->Bool>};

// "float.less_eq": float less than or equal comparison.
constexpr BuiltinInfo FloatLessEq = {
    "float.less_eq", ValidateSignature<auto(FloatT, FloatT)->Bool>};

// "float.greater": float greater than comparison.
constexpr BuiltinInfo FloatGreater = {
    "float.greater", ValidateSignature<auto(FloatT, FloatT)->Bool>};

// "float.greater_eq": float greater than or equal comparison.
constexpr BuiltinInfo FloatGreaterEq = {
    "float.greater_eq", ValidateSignature<auto(FloatT, FloatT)->Bool>};

// "bool.eq": bool equality comparison.
constexpr BuiltinInfo BoolEq = {"bool.eq",
                                ValidateSignature<auto(Bool, Bool)->Bool>};

// "bool.neq": bool non-equality comparison.
constexpr BuiltinInfo BoolNeq = {"bool.neq",
                                 ValidateSignature<auto(Bool, Bool)->Bool>};

// "type.and": facet type combination.
constexpr BuiltinInfo TypeAnd = {
    "type.and", ValidateSignature<auto(AnyType, AnyType)->AnyType>};

}  // namespace BuiltinFunctionInfo

CARBON_DEFINE_ENUM_CLASS_NAMES(BuiltinFunctionKind) = {
#define CARBON_SEM_IR_BUILTIN_FUNCTION_KIND(Name) \
  BuiltinFunctionInfo::Name.name,
#include "toolchain/sem_ir/builtin_function_kind.def"
};

// Returns the builtin function kind with the given name, or None if the name
// is unknown.
auto BuiltinFunctionKind::ForBuiltinName(llvm::StringRef name)
    -> BuiltinFunctionKind {
#define CARBON_SEM_IR_BUILTIN_FUNCTION_KIND(Name) \
  if (name == BuiltinFunctionInfo::Name.name) {   \
    return BuiltinFunctionKind::Name;             \
  }
#include "toolchain/sem_ir/builtin_function_kind.def"
  return BuiltinFunctionKind::None;
}

auto BuiltinFunctionKind::IsValidType(const File& sem_ir,
                                      llvm::ArrayRef<TypeId> arg_types,
                                      TypeId return_type) const -> bool {
  static constexpr ValidateFn* ValidateFns[] = {
#define CARBON_SEM_IR_BUILTIN_FUNCTION_KIND(Name) \
  BuiltinFunctionInfo::Name.validate,
#include "toolchain/sem_ir/builtin_function_kind.def"
  };
  return ValidateFns[AsInt()](sem_ir, arg_types, return_type);
}

// Determines whether a builtin call involves an integer literal in its
// arguments or return type. If so, for many builtins we want to treat the call
// as being compile-time-only. This is because `Core.IntLiteral` has an empty
// runtime representation, and a value of that type isn't necessarily a
// compile-time constant, so an arbitrary runtime value of type
// `Core.IntLiteral` may not have a value available for the builtin to use. For
// example, given:
//
// var n: Core.IntLiteral() = 123;
//
// we would be unable to lower a runtime operation such as `(1 as i32) << n`
// because the runtime representation of `n` doesn't track its value at all.
//
// For now, we treat all operations involving `Core.IntLiteral` as being
// compile-time-only.
//
// TODO: We will need to accept things like `some_i32 << 5` eventually. We could
// allow builtin calls at runtime if all the IntLiteral arguments have constant
// values, or add logic to the prelude to promote the `IntLiteral` operand to a
// different type in such cases.
//
// TODO: For now, we also treat builtins *returning* `Core.IntLiteral` as being
// compile-time-only. This is mostly done for simplicity, but should probably be
// revisited.
static auto AnyIntLiteralTypes(const File& sem_ir,
                               llvm::ArrayRef<InstId> arg_ids,
                               TypeId return_type_id) -> bool {
  if (sem_ir.types().Is<SemIR::IntLiteralType>(return_type_id)) {
    return true;
  }
  for (auto arg_id : arg_ids) {
    if (sem_ir.types().Is<SemIR::IntLiteralType>(
            sem_ir.insts().Get(arg_id).type_id())) {
      return true;
    }
  }
  return false;
}

auto BuiltinFunctionKind::IsCompTimeOnly(const File& sem_ir,
                                         llvm::ArrayRef<InstId> arg_ids,
                                         TypeId return_type_id) const -> bool {
  switch (*this) {
    case IntConvertChecked:
      // Checked integer conversions are compile-time only.
      return true;

    case IntConvert:
    case IntSNegate:
    case IntComplement:
    case IntSAdd:
    case IntSSub:
    case IntSMul:
    case IntSDiv:
    case IntSMod:
    case IntAnd:
    case IntOr:
    case IntXor:
    case IntLeftShift:
    case IntRightShift:
    case IntEq:
    case IntNeq:
    case IntLess:
    case IntLessEq:
    case IntGreater:
    case IntGreaterEq:
      // Integer operations are compile-time-only if they involve integer
      // literal types. See AnyIntLiteralTypes comment for explanation.
      return AnyIntLiteralTypes(sem_ir, arg_ids, return_type_id);

    case TypeAnd:
      return true;

    default:
      // TODO: Should the sized MakeType functions be compile-time only? We
      // can't produce diagnostics for bad sizes at runtime.
      return false;
  }
}

}  // namespace Carbon::SemIR
