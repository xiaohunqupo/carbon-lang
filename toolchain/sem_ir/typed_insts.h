// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEM_IR_TYPED_INSTS_H_
#define CARBON_TOOLCHAIN_SEM_IR_TYPED_INSTS_H_

#include "toolchain/base/int.h"
#include "toolchain/parse/node_ids.h"
#include "toolchain/sem_ir/builtin_inst_kind.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst_kind.h"

// Representations for specific kinds of instructions.
//
// Each type should be a struct with the following members, in this order:
//
// - Either a `Kind` constant, or a `Kinds` constant and an `InstKind kind;`
//   member. These are described below.
// - Optionally, a `TypeId type_id;` member, for instructions that produce a
//   value. This includes instructions that produce an abstract value, such as a
//   `Namespace`, for which a placeholder type should be used.
// - Up to two `[...]Id` members describing the contents of the struct.
//
// The field names here matter -- the fields must have the names specified
// above, when present. When converting to a `SemIR::Inst`, the `kind` and
// `type_id` fields will become the kind and type associated with the
// type-erased instruction.
//
// Each type that describes a single kind of instructions provides a constant
// `Kind` that associates the type with a particular member of the `InstKind`
// enumeration. This `Kind` declaration also defines the instruction kind by
// calling `InstKind::Define` and specifying additional information about the
// instruction kind. This information is available through the member functions
// of the `InstKind` value declared in `inst_kind.h`, and includes the name
// used in textual IR and whether the instruction is a terminator instruction.
//
// Struct types can also be provided for categories of instructions with a
// common representation, to allow the common representation to be accessed
// conveniently. In this case, instead of providing a constant `Kind` member,
// the struct should have a constant `InstKind Kinds[];` member that lists the
// kinds of instructions in the category, and an `InstKind kind;` member that
// is used to identify the specific kind of the instruction. Separate struct
// types still need to be defined for each instruction kind in the category.

namespace Carbon::SemIR {

// Used for the type of patterns that do not match a fixed type.
//
// TODO: Annotate as a builtin.
struct AutoType {
  static constexpr auto Kind = InstKind::AutoType.Define<Parse::InvalidNodeId>(
      {.ir_name = "auto",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// The type of bool literals and branch conditions, bool.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
//
// TODO: Annotate as a builtin.
struct BoolType {
  static constexpr auto Kind = InstKind::BoolType.Define<Parse::InvalidNodeId>(
      {.ir_name = "bool",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// An adapted type declaration in a class, of the form `adapt T;`.
struct AdaptDecl {
  static constexpr auto Kind = InstKind::AdaptDecl.Define<Parse::AdaptDeclId>(
      {.ir_name = "adapt_decl", .is_lowered = false});

  // No type_id; this is not a value.
  TypeId adapted_type_id;
};

// Takes the address of a reference expression, such as for the `&` address-of
// operator, `&lvalue`.
struct AddrOf {
  // Parse node is usually Parse::PrefixOperatorAmpId.
  static constexpr auto Kind = InstKind::AddrOf.Define<Parse::NodeId>(
      {.ir_name = "addr_of", .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  InstId lvalue_id;
};

// An `addr` pattern, such as `addr self: Self*`. Structurally, `inner_id` will
// generally be a pattern inst.
struct AddrPattern {
  static constexpr auto Kind = InstKind::AddrPattern.Define<Parse::AddrId>(
      {.ir_name = "addr_pattern", .is_lowered = false});

  TypeId type_id;
  // The `self` binding.
  InstId inner_id;
};

// An array indexing operation, such as `array[index]`.
struct ArrayIndex {
  // Parse node is usually Parse::IndexExprId.
  static constexpr auto Kind =
      InstKind::ArrayIndex.Define<Parse::NodeId>({.ir_name = "array_index"});

  TypeId type_id;
  InstId array_id;
  InstId index_id;
};

// Common representation for aggregate access nodes, which access a fixed
// element of an aggregate.
struct AnyAggregateAccess {
  static constexpr InstKind Kinds[] = {
      InstKind::StructAccess, InstKind::TupleAccess,
      InstKind::ClassElementAccess, InstKind::InterfaceWitnessAccess};

  InstKind kind;
  TypeId type_id;
  InstId aggregate_id;
  ElementIndex index;
};

// Common representation for all kinds of aggregate initialization.
struct AnyAggregateInit {
  static constexpr InstKind Kinds[] = {InstKind::ArrayInit, InstKind::ClassInit,
                                       InstKind::StructInit,
                                       InstKind::TupleInit};

  InstKind kind;
  TypeId type_id;
  InstBlockId elements_id;
  InstId dest_id;
};

// Common representation for all kinds of aggregate value.
struct AnyAggregateValue {
  static constexpr InstKind Kinds[] = {
      InstKind::StructValue, InstKind::TupleValue, InstKind::InterfaceWitness};

  InstKind kind;
  TypeId type_id;
  InstBlockId elements_id;
};

// Initializes an array from a tuple. `tuple_id` is the source tuple
// expression. `inits_id` contains one initializer per array element.
// `dest_id` is the destination array object for the initialization.
struct ArrayInit {
  static constexpr auto Kind =
      InstKind::ArrayInit.Define<Parse::NodeId>({.ir_name = "array_init"});

  TypeId type_id;
  InstBlockId inits_id;
  InstId dest_id;
};

// An array of `element_type_id` values, sized to `bound_id`.
struct ArrayType {
  static constexpr auto Kind = InstKind::ArrayType.Define<Parse::ArrayExprId>(
      {.ir_name = "array_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  InstId bound_id;
  TypeId element_type_id;
};

// Perform a no-op conversion to a compatible type.
struct AsCompatible {
  static constexpr auto Kind = InstKind::AsCompatible.Define<Parse::NodeId>(
      {.ir_name = "as_compatible"});

  TypeId type_id;
  InstId source_id;
};

// Performs a source-level initialization or assignment of `lhs_id` from
// `rhs_id`. This finishes initialization of `lhs_id` in the same way as
// `InitializeFrom`.
struct Assign {
  static constexpr auto Kind = InstKind::Assign.Define<
      Parse::NodeIdOneOf<Parse::InfixOperatorEqualId, Parse::VariableDeclId>>(
      {.ir_name = "assign"});

  // Assignments are statements, and so have no type.
  InstId lhs_id;
  InstId rhs_id;
};

// An associated constant declaration in an interface, such as `let T:! type;`.
struct AssociatedConstantDecl {
  static constexpr auto Kind =
      InstKind::AssociatedConstantDecl.Define<Parse::LetDeclId>(
          {.ir_name = "assoc_const_decl", .is_lowered = false});

  TypeId type_id;
  NameId name_id;
};

// An associated entity declared in an interface. This is either an associated
// function or a non-function associated constant such as an associated type.
// This represents the entity before impl lookup is performed, and identifies
// the slot within a witness where the constant value will be found.
struct AssociatedEntity {
  static constexpr auto Kind = InstKind::AssociatedEntity.Define<Parse::NodeId>(
      {.ir_name = "assoc_entity", .constant_kind = InstConstantKind::Always});

  // The type of the associated entity. This is an AssociatedEntityType.
  TypeId type_id;
  ElementIndex index;
  AbsoluteInstId decl_id;
};

// The type of an expression that names an associated entity, such as
// `InterfaceName.Function`.
struct AssociatedEntityType {
  static constexpr auto Kind =
      InstKind::AssociatedEntityType.Define<Parse::InvalidNodeId>(
          {.ir_name = "assoc_entity_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  TypeId interface_type_id;
  TypeId entity_type_id;
};

// A base in a class, of the form `base: base_type;`. A base class is an
// element of the derived class, and the type of the `BaseDecl` instruction is
// an `UnboundElementType`.
struct BaseDecl {
  static constexpr auto Kind = InstKind::BaseDecl.Define<Parse::BaseDeclId>(
      {.ir_name = "base_decl", .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  TypeId base_type_id;
  ElementIndex index;
};

// Common representation for various `bind*` nodes.
struct AnyBindName {
  // TODO: Also handle BindTemplateName once it exists.
  static constexpr InstKind Kinds[] = {InstKind::BindAlias, InstKind::BindName,
                                       InstKind::BindSymbolicName};

  InstKind kind;
  TypeId type_id;
  EntityNameId entity_name_id;
  InstId value_id;
};

// Common representation for various `bind*` nodes, and `export name`.
struct AnyBindNameOrExportDecl {
  // TODO: Also handle BindTemplateName once it exists.
  static constexpr InstKind Kinds[] = {InstKind::BindAlias, InstKind::BindName,
                                       InstKind::BindSymbolicName,
                                       InstKind::ExportDecl};

  InstKind kind;
  TypeId type_id;
  EntityNameId entity_name_id;
  InstId value_id;
};

// Binds a name as an alias.
struct BindAlias {
  static constexpr auto Kind =
      InstKind::BindAlias.Define<Parse::NodeId>({.ir_name = "bind_alias"});

  TypeId type_id;
  EntityNameId entity_name_id;
  InstId value_id;
};

// Binds a name, such as `x` in `var x: i32`.
struct BindName {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::BindName.Define<Parse::NodeId>({.ir_name = "bind_name"});

  TypeId type_id;
  EntityNameId entity_name_id;
  // The value is inline in the inst so that value access doesn't require an
  // indirection.
  InstId value_id;
};

// Binds a symbolic name, such as `x` in `let x:! i32 = 7;`.
struct BindSymbolicName {
  static constexpr auto Kind = InstKind::BindSymbolicName.Define<Parse::NodeId>(
      {.ir_name = "bind_symbolic_name",
       .is_type = InstIsType::Maybe,
       .constant_kind = InstConstantKind::SymbolicOnly});

  TypeId type_id;
  EntityNameId entity_name_id;
  InstId value_id;
};

// A value binding. Used when an expression contains a reference and we want a
// value.
struct BindValue {
  static constexpr auto Kind =
      InstKind::BindValue.Define<Parse::NodeId>({.ir_name = "bind_value"});

  TypeId type_id;
  InstId value_id;
};

// Common representation for various `*binding_pattern` nodes.
struct AnyBindingPattern {
  // TODO: Also handle TemplateBindingPattern once it exists.
  static constexpr InstKind Kinds[] = {InstKind::BindingPattern,
                                       InstKind::SymbolicBindingPattern};

  InstKind kind;
  TypeId type_id;
  EntityNameId entity_name_id;
};

// Represents a non-symbolic binding pattern.
struct BindingPattern {
  static constexpr auto Kind = InstKind::BindingPattern.Define<Parse::NodeId>(
      {.ir_name = "binding_pattern", .is_lowered = false});

  TypeId type_id;
  EntityNameId entity_name_id;
};

// Represents a symbolic binding pattern.
struct SymbolicBindingPattern {
  static constexpr auto Kind =
      InstKind::SymbolicBindingPattern.Define<Parse::NodeId>({
          .ir_name = "symbolic_binding_pattern",
          .is_type = InstIsType::Never,
          .constant_kind = InstConstantKind::SymbolicOnly,
          .is_lowered = false,
      });

  TypeId type_id;
  EntityNameId entity_name_id;
};

// Reads an argument from `BranchWithArg`.
struct BlockArg {
  static constexpr auto Kind =
      InstKind::BlockArg.Define<Parse::NodeId>({.ir_name = "block_arg"});

  TypeId type_id;
  InstBlockId block_id;
};

// A literal bool value, `true` or `false`.
struct BoolLiteral {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::BoolLiteral.Define<Parse::NodeId>(
      {.ir_name = "bool_literal", .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  BoolValue value;
};

// A bound method, that combines a function with the value to use for its
// `self` parameter, such as `object.MethodName`.
struct BoundMethod {
  static constexpr auto Kind = InstKind::BoundMethod.Define<Parse::NodeId>(
      {.ir_name = "bound_method",
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  // The object argument in the bound method, which will be used to initialize
  // `self`, or whose address will be used to initialize `self` for an `addr
  // self` parameter.
  InstId object_id;
  InstId function_id;
};

// The type of bound method values.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct BoundMethodType {
  static constexpr auto Kind =
      InstKind::BoundMethodType.Define<Parse::InvalidNodeId>(
          {.ir_name = "<bound method>",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// Common representation for all kinds of `Branch*` node.
struct AnyBranch {
  static constexpr InstKind Kinds[] = {InstKind::Branch, InstKind::BranchIf,
                                       InstKind::BranchWithArg};

  InstKind kind;
  // Branches don't produce a value, so have no type.
  InstBlockId target_id;
  // Kind-specific data.
  int32_t arg1;
};

// Control flow to branch to the target block.
struct Branch {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::Branch.Define<Parse::NodeId>(
      {.ir_name = "br", .terminator_kind = TerminatorKind::Terminator});

  // Branches don't produce a value, so have no type.
  InstBlockId target_id;
};

// Control flow to branch to the target block if `cond_id` is true.
struct BranchIf {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::BranchIf.Define<Parse::NodeId>(
      {.ir_name = "br", .terminator_kind = TerminatorKind::TerminatorSequence});

  // Branches don't produce a value, so have no type.
  InstBlockId target_id;
  InstId cond_id;
};

// Control flow to branch to the target block, passing an argument for
// `BlockArg` to read.
struct BranchWithArg {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::BranchWithArg.Define<Parse::NodeId>(
      {.ir_name = "br", .terminator_kind = TerminatorKind::Terminator});

  // Branches don't produce a value, so have no type.
  InstBlockId target_id;
  InstId arg_id;
};

// An abstract `callee(args)` call, where the callee may be a function, but
// could also be a generic or other callable structure.
struct Call {
  // For a syntactic call, the parse node will be a CallExprStartId. However,
  // calls can arise from other syntaxes, such as operators and implicit
  // conversions.
  static constexpr auto Kind =
      InstKind::Call.Define<Parse::NodeId>({.ir_name = "call"});

  TypeId type_id;
  InstId callee_id;
  // Runtime arguments in lexical order of the parameter declarations, followed
  // by the argument for the return slot, if present.
  InstBlockId args_id;
};

// A class declaration.
struct ClassDecl {
  static constexpr auto Kind =
      InstKind::ClassDecl.Define<Parse::AnyClassDeclId>(
          {.ir_name = "class_decl"});

  TypeId type_id;
  // TODO: For a generic class declaration, the name of the class declaration
  // should become a parameterized entity name value.
  ClassId class_id;
  // The declaration block, containing the class name's qualifiers and the
  // class's generic parameters.
  InstBlockId decl_block_id;
};

// Access to a member of a class, such as `base.index`. This provides a
// reference for either reading or writing.
struct ClassElementAccess {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::ClassElementAccess.Define<Parse::NodeId>(
          {.ir_name = "class_element_access"});

  TypeId type_id;
  InstId base_id;
  ElementIndex index;
};

// Initializes a class object at dest_id with the contents of elements_id.
struct ClassInit {
  static constexpr auto Kind =
      InstKind::ClassInit.Define<Parse::NodeId>({.ir_name = "class_init"});

  TypeId type_id;
  InstBlockId elements_id;
  InstId dest_id;
};

// The type for a class, either non-generic or specific.
struct ClassType {
  static constexpr auto Kind = InstKind::ClassType.Define<Parse::NodeId>(
      {.ir_name = "class_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  ClassId class_id;
  SpecificId specific_id;
};

// A witness that a type is complete. For now, this only tracks the object
// representation corresponding to the type, and this instruction is currently
// only created for class types, because all other types are their own object
// representation.
//
// TODO: Eventually this should be replaced by a witness for an interface that
// models type completeness, and should track other information such as the
// value representation.
struct CompleteTypeWitness {
  static constexpr auto Kind =
      InstKind::CompleteTypeWitness.Define<Parse::NodeId>(
          {.ir_name = "complete_type_witness",
           .is_type = InstIsType::Never,
           .constant_kind = InstConstantKind::Always});
  // Always the builtin witness type.
  TypeId type_id;
  // The type that is used as the object representation of this type.
  TypeId object_repr_id;
};

// Indicates `const` on a type, such as `var x: const i32`.
struct ConstType {
  static constexpr auto Kind =
      InstKind::ConstType.Define<Parse::PrefixOperatorConstId>(
          {.ir_name = "const_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  TypeId inner_id;
};

// Records that a type conversion `original as new_type` was done, producing the
// result.
struct Converted {
  static constexpr auto Kind =
      InstKind::Converted.Define<Parse::NodeId>({.ir_name = "converted"});

  TypeId type_id;
  InstId original_id;
  InstId result_id;
};

// The `*` dereference operator, as in `*pointer`.
struct Deref {
  static constexpr auto Kind =
      InstKind::Deref.Define<Parse::NodeId>({.ir_name = "deref"});

  TypeId type_id;
  InstId pointer_id;
};

// Used when a semantic error has been detected, and a SemIR InstId is still
// required. For example, when there is a type checking issue, this will be used
// in the type_id. It's typically used as a cue that semantic checking doesn't
// need to issue further diagnostics.
//
// TODO: Annotate as a builtin.
struct ErrorInst {
  static constexpr auto Kind = InstKind::ErrorInst.Define<Parse::InvalidNodeId>(
      {.ir_name = "<error>",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// An `export bind_name` declaration.
struct ExportDecl {
  static constexpr auto Kind =
      InstKind::ExportDecl.Define<Parse::ExportDeclId>({.ir_name = "export"});

  TypeId type_id;
  EntityNameId entity_name_id;
  // The exported entity.
  InstId value_id;
};

// Represents accessing the `type` field in a facet value, which is notionally a
// pair of a type and a witness.
struct FacetAccessType {
  static constexpr auto Kind = InstKind::FacetAccessType.Define<Parse::NodeId>(
      {.ir_name = "facet_access_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::SymbolicOnly});

  TypeId type_id;
  // An instruction that evaluates to a `FacetValue`.
  InstId facet_value_inst_id;
};

// TODO: `FacetAccessWitness`

// A facet type value.
struct FacetType {
  static constexpr auto Kind = InstKind::FacetType.Define<Parse::NodeId>(
      {.ir_name = "facet_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  FacetTypeId facet_type_id;
};

// A facet value, the value of a facet type. This consists of a type and a
// witness that it satisfies the facet type.
struct FacetValue {
  static constexpr auto Kind = InstKind::FacetValue.Define<Parse::NodeId>(
      {.ir_name = "facet_value",
       .is_type = InstIsType::Never,
       .constant_kind = InstConstantKind::Always});

  // A `FacetType`.
  TypeId type_id;
  // The type that you will get if you cast this value to `type`.
  InstId type_inst_id;
  // An `InterfaceWitness` instruction (TODO: `FacetTypeWitness`).
  InstId witness_inst_id;
};

// A field in a class, of the form `var field: field_type;`. The type of the
// `FieldDecl` instruction is an `UnboundElementType`.
struct FieldDecl {
  static constexpr auto Kind =
      InstKind::FieldDecl.Define<Parse::BindingPatternId>(
          {.ir_name = "field_decl", .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  NameId name_id;
  ElementIndex index;
};

// A literal floating point value.
struct FloatLiteral {
  static constexpr auto Kind =
      InstKind::FloatLiteral.Define<Parse::RealLiteralId>(
          {.ir_name = "float_literal",
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  FloatId float_id;
};

// A floating point type.
struct FloatType {
  static constexpr auto Kind = InstKind::FloatType.Define<Parse::InvalidNodeId>(
      {.ir_name = "float_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  // TODO: Consider adding a more compact way of representing either a small
  // float bit width or an inst_id.
  InstId bit_width_id;
};

// The legacy float type. This is currently used for real literals, and is
// treated as f64. It's separate from `FloatType`, and should change to mirror
// integers, likely replacing this with a `FloatLiteralType`.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct LegacyFloatType {
  static constexpr auto Kind =
      InstKind::LegacyFloatType.Define<Parse::InvalidNodeId>(
          {.ir_name = "f64",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// A function declaration.
struct FunctionDecl {
  static constexpr auto Kind =
      InstKind::FunctionDecl.Define<Parse::AnyFunctionDeclId>(
          {.ir_name = "fn_decl", .is_lowered = false});

  TypeId type_id;
  FunctionId function_id;
  // The declaration block, containing the function declaration's parameters and
  // their types.
  InstBlockId decl_block_id;
};

// The type of a function.
struct FunctionType {
  static constexpr auto Kind =
      InstKind::FunctionType.Define<Parse::AnyFunctionDeclId>(
          {.ir_name = "fn_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  FunctionId function_id;
  SpecificId specific_id;
};

// The type of the name of a generic class. The corresponding value is an empty
// `StructValue`.
struct GenericClassType {
  // This is only ever created as a constant, so doesn't have a location.
  static constexpr auto Kind =
      InstKind::GenericClassType.Define<Parse::InvalidNodeId>(
          {.ir_name = "generic_class_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  ClassId class_id;
  SpecificId enclosing_specific_id;
};

// The type of the name of a generic interface. The corresponding value is an
// empty `StructValue`.
struct GenericInterfaceType {
  // This is only ever created as a constant, so doesn't have a location.
  static constexpr auto Kind =
      InstKind::GenericInterfaceType.Define<Parse::InvalidNodeId>(
          {.ir_name = "generic_interface_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  InterfaceId interface_id;
  SpecificId enclosing_specific_id;
};

// An `impl` declaration.
struct ImplDecl {
  static constexpr auto Kind = InstKind::ImplDecl.Define<Parse::AnyImplDeclId>(
      {.ir_name = "impl_decl",
       .constant_kind = InstConstantKind::Always,
       .is_lowered = false});

  // No type: an impl declaration is not a value.
  ImplId impl_id;
  // The declaration block, containing the impl's deduced parameters and its
  // self type and interface type.
  InstBlockId decl_block_id;
};

// An `import` declaration. This is mainly for `import` diagnostics, and a 1:1
// correspondence with actual `import`s isn't guaranteed.
struct ImportDecl {
  static constexpr auto Kind = InstKind::ImportDecl.Define<Parse::ImportDeclId>(
      {.ir_name = "import", .is_lowered = false});

  NameId package_id;
};

// Common representation for all kinds of `ImportRef*` node.
struct AnyImportRef {
  static constexpr InstKind Kinds[] = {InstKind::ImportRefUnloaded,
                                       InstKind::ImportRefLoaded};

  InstKind kind;
  ImportIRInstId import_ir_inst_id;
  // A BindName is currently only set on directly imported names. It is not
  // generically available.
  EntityNameId entity_name_id;
};

// An imported entity that is not yet been loaded.
struct ImportRefUnloaded {
  static constexpr auto Kind =
      InstKind::ImportRefUnloaded.Define<Parse::NodeId>(
          {.ir_name = "import_ref", .is_lowered = false});

  ImportIRInstId import_ir_inst_id;
  EntityNameId entity_name_id;
};

// A imported entity that is loaded, and may be used.
struct ImportRefLoaded {
  static constexpr auto Kind = InstKind::ImportRefLoaded.Define<Parse::NodeId>(
      {.ir_name = "import_ref", .is_lowered = false});

  TypeId type_id;
  ImportIRInstId import_ir_inst_id;
  EntityNameId entity_name_id;
};

// Finalizes the initialization of `dest_id` from the initializer expression
// `src_id`, by performing a final copy from source to destination, for types
// whose initialization is not in-place.
struct InitializeFrom {
  // Note this Parse::NodeId is unused. InitializeFrom is only constructed by
  // reusing locations.
  // TODO: Figure out if there's a better way to handle this case.
  static constexpr auto Kind = InstKind::InitializeFrom.Define<Parse::NodeId>(
      {.ir_name = "initialize_from"});

  TypeId type_id;
  InstId src_id;
  InstId dest_id;
};

// An interface declaration.
struct InterfaceDecl {
  static constexpr auto Kind =
      InstKind::InterfaceDecl.Define<Parse::AnyInterfaceDeclId>(
          {.ir_name = "interface_decl", .is_lowered = false});

  TypeId type_id;
  // TODO: For a generic interface declaration, the name of the interface
  // declaration should become a parameterized entity name value.
  InterfaceId interface_id;
  // The declaration block, containing the interface name's qualifiers and the
  // interface's generic parameters.
  InstBlockId decl_block_id;
};

// A witness that a type implements an interface.
struct InterfaceWitness {
  static constexpr auto Kind = InstKind::InterfaceWitness.Define<Parse::NodeId>(
      {.ir_name = "interface_witness",
       .constant_kind = InstConstantKind::Conditional,
       .is_lowered = false});

  // Always the builtin witness type.
  TypeId type_id;
  InstBlockId elements_id;
};

// Accesses an element of an interface witness by index.
struct InterfaceWitnessAccess {
  static constexpr auto Kind =
      InstKind::InterfaceWitnessAccess.Define<Parse::NodeId>(
          {.ir_name = "interface_witness_access",
           .is_type = InstIsType::Maybe,
           .constant_kind = InstConstantKind::SymbolicOnly,
           .is_lowered = false});

  TypeId type_id;
  InstId witness_id;
  ElementIndex index;
};

// A literal integer value.
struct IntValue {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::IntValue.Define<Parse::NodeId>(
      {.ir_name = "int_value", .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  IntId int_id;
};

// An arbitrary-precision integer type, which is used as the type of integer
// literals and as the parameter type of `Core.Int` and `Core.Float`. This type
// only provides compile-time operations, and is represented as an empty type at
// runtime.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct IntLiteralType {
  static constexpr auto Kind =
      InstKind::IntLiteralType.Define<Parse::InvalidNodeId>(
          {.ir_name = "Core.IntLiteral",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// An integer type.
struct IntType {
  static constexpr auto Kind = InstKind::IntType.Define<Parse::InvalidNodeId>(
      {.ir_name = "int_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  IntKind int_kind;
  // TODO: Consider adding a more compact way of representing either a small
  // unsigned integer bit width or an inst_id.
  InstId bit_width_id;
};

// A name reference, with the value of the name. This only handles name
// resolution; the value may be used for reading or writing.
struct NameRef {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::NameRef.Define<Parse::NodeId>({.ir_name = "name_ref"});

  TypeId type_id;
  NameId name_id;
  InstId value_id;
};

// A namespace declaration.
struct Namespace {
  static constexpr auto Kind =
      InstKind::Namespace.Define<Parse::AnyNamespaceId>(
          {.ir_name = "namespace", .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  NameScopeId name_scope_id;
  // If the namespace was produced by an `import` line, the associated line for
  // diagnostics.
  AbsoluteInstId import_id;
};

// The type of namespace and imported package names.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct NamespaceType {
  static constexpr auto Kind =
      InstKind::NamespaceType.Define<Parse::InvalidNodeId>(
          {.ir_name = "<namespace>",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// A parameter for a function or other parameterized block, as exposed in the
// SemIR calling convention. The sub-kinds differ only in their expression
// category.
struct AnyParam {
  static constexpr InstKind Kinds[] = {InstKind::OutParam,
                                       InstKind::ValueParam};

  InstKind kind;
  TypeId type_id;
  RuntimeParamIndex runtime_index;

  // A name to associate with this Param in pretty-printed IR. This is not
  // necessarily unique, or even valid, and has no semantic significance.
  NameId pretty_name_id;
};

// An output parameter. See AnyParam for member documentation.
struct OutParam {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::OutParam.Define<Parse::NodeId>({.ir_name = "out_param"});

  TypeId type_id;
  RuntimeParamIndex runtime_index;
  NameId pretty_name_id;
};

// A by-value parameter. See AnyParam for member documentation.
struct ValueParam {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::ValueParam.Define<Parse::NodeId>({.ir_name = "value_param"});

  TypeId type_id;
  RuntimeParamIndex runtime_index;
  NameId pretty_name_id;
};

// A pattern that represents a parameter. It delegates to subpattern_id
// in pattern matching. The sub-kinds differ only in the expression category
// of the corresponding parameter inst.
struct AnyParamPattern {
  static constexpr InstKind Kinds[] = {InstKind::OutParamPattern,
                                       InstKind::ValueParamPattern};

  InstKind kind;
  TypeId type_id;
  InstId subpattern_id;
  RuntimeParamIndex runtime_index;
};

// A pattern that represents an output parameter.
struct OutParamPattern {
  static constexpr auto Kind =
      InstKind::OutParamPattern.Define<Parse::ReturnTypeId>(
          {.ir_name = "out_param_pattern", .is_lowered = false});

  TypeId type_id;
  InstId subpattern_id;
  RuntimeParamIndex runtime_index;
};

// A pattern that represents a by-value parameter.
struct ValueParamPattern {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::ValueParamPattern.Define<Parse::NodeId>(
          {.ir_name = "value_param_pattern", .is_lowered = false});

  TypeId type_id;
  InstId subpattern_id;
  RuntimeParamIndex runtime_index;
};

// Modifies a pointee type to be a pointer. This is tracking the `*` in
// `x: i32*`, where `pointee_id` is `i32` and `type_id` is `type`.
struct PointerType {
  static constexpr auto Kind =
      InstKind::PointerType.Define<Parse::PostfixOperatorStarId>(
          {.ir_name = "ptr_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  TypeId pointee_id;
};

struct Return {
  static constexpr auto Kind =
      InstKind::Return.Define<Parse::NodeIdOneOf<Parse::FunctionDefinitionId,
                                                 Parse::ReturnStatementId>>(
          {.ir_name = "return", .terminator_kind = TerminatorKind::Terminator});

  // This is a statement, so has no type.
};

// A `return expr;` statement.
struct ReturnExpr {
  static constexpr auto Kind =
      InstKind::ReturnExpr.Define<Parse::ReturnStatementId>(
          {.ir_name = "return", .terminator_kind = TerminatorKind::Terminator});

  // This is a statement, so has no type.
  InstId expr_id;
  // The return slot, if any. Invalid if we're not returning through memory.
  InstId dest_id;
};

// The return slot of a function declaration, as exposed in the function body.
// This acts as an output parameter, analogous to `BindName` for input
// parameters.
struct ReturnSlot {
  static constexpr auto Kind =
      InstKind::ReturnSlot.Define<Parse::NodeId>({.ir_name = "return_slot"});

  // The type of the value that will be stored in this slot (i.e. the return
  // type of the function).
  TypeId type_id;

  // The function return type as originally written by the user. For diagnostics
  // only; this has no semantic significance, and is not preserved across
  // imports.
  InstId type_inst_id;

  // The storage that will be initialized by the function.
  InstId storage_id;
};

// The return slot of a function declaration, as exposed to the function's
// callers. This acts as an output parameter, analogous to `BindingPattern`
// for input parameters.
struct ReturnSlotPattern {
  static constexpr auto Kind =
      InstKind::ReturnSlotPattern.Define<Parse::ReturnTypeId>(
          {.ir_name = "return_slot_pattern", .is_lowered = false});

  // The type of the value that will be stored in this slot (i.e. the return
  // type of the function).
  TypeId type_id;

  // The function return type as originally written by the user. For diagnostics
  // only; this has no semantic significance, and is not preserved across
  // imports.
  InstId type_inst_id;
};

// An `expr == expr` clause in a `where` expression or `require` declaration.
struct RequirementEquivalent {
  static constexpr auto Kind =
      InstKind::RequirementEquivalent.Define<Parse::RequirementEqualEqualId>(
          {.ir_name = "requirement_equivalent", .is_lowered = false});

  // No type since not an expression
  InstId lhs_id;
  InstId rhs_id;
};

// An `expr impls expr` clause in a `where` expression or `require` declaration.
struct RequirementImpls {
  static constexpr auto Kind =
      InstKind::RequirementImpls.Define<Parse::RequirementImplsId>(
          {.ir_name = "requirement_impls", .is_lowered = false});

  // No type since not an expression
  InstId lhs_id;
  InstId rhs_id;
};

// A `.M = expr` clause in a `where` expression or `require` declaration.
struct RequirementRewrite {
  static constexpr auto Kind =
      InstKind::RequirementRewrite.Define<Parse::RequirementEqualId>(
          {.ir_name = "requirement_rewrite", .is_lowered = false});

  // No type since not an expression
  InstId lhs_id;
  InstId rhs_id;
};

// Given an instruction with a constant value that depends on a generic
// parameter, selects a version of that instruction with the constant value
// corresponding to a particular specific.
//
// TODO: We only form these as the instruction referenced by a `NameRef`.
// Consider merging an `SpecificConstant` + `NameRef` into a new form of
// instruction in order to give a more compact representation.
struct SpecificConstant {
  // TODO: Can we make Parse::NodeId more specific?
  static constexpr auto Kind = InstKind::SpecificConstant.Define<Parse::NodeId>(
      {.ir_name = "specific_constant", .is_lowered = false});

  TypeId type_id;
  AbsoluteInstId inst_id;
  SpecificId specific_id;
};

// A specific instance of a generic function. This represents the callee in a
// call instruction that is calling a generic function, where the specific
// arguments of the function have been deduced.
//
// TODO: This value corresponds to the `(FunctionType as Call(...)).Op` function
// in the overloaded calls design. Eventually we should represent it more
// directly as a member of the `Call` interface.
struct SpecificFunction {
  static constexpr auto Kind = InstKind::SpecificFunction.Define<Parse::NodeId>(
      {.ir_name = "specific_function",
       .constant_kind = InstConstantKind::Conditional});

  // Always the builtin SpecificFunctionType.
  TypeId type_id;
  // The expression denoting the callee.
  InstId callee_id;
  // The specific instance of the generic callee that will be called, including
  // all the compile-time arguments.
  SpecificId specific_id;
};

// The type of specific functions.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct SpecificFunctionType {
  static constexpr auto Kind =
      InstKind::SpecificFunctionType.Define<Parse::InvalidNodeId>(
          {.ir_name = "<specific function>",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// Splices a block into the location where this appears. This may be an
// expression, producing a result with a given type. For example, when
// constructing from aggregates we may figure out which conversions are required
// late, and splice parts together.
struct SpliceBlock {
  static constexpr auto Kind =
      InstKind::SpliceBlock.Define<Parse::NodeId>({.ir_name = "splice_block"});

  TypeId type_id;
  InstBlockId block_id;
  InstId result_id;
};

// A literal string value.
struct StringLiteral {
  static constexpr auto Kind =
      InstKind::StringLiteral.Define<Parse::StringLiteralId>(
          {.ir_name = "string_literal",
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
  StringLiteralValueId string_literal_id;
};

// The type of string values and String literals.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct StringType {
  static constexpr auto Kind =
      InstKind::StringType.Define<Parse::InvalidNodeId>(
          {.ir_name = "String",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// Access to a struct type, with the index into the struct_id representation.
struct StructAccess {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::StructAccess.Define<Parse::NodeId>(
      {.ir_name = "struct_access"});

  TypeId type_id;
  InstId struct_id;
  ElementIndex index;
};

// Initializes a dest struct with the provided elements.
struct StructInit {
  static constexpr auto Kind =
      InstKind::StructInit.Define<Parse::NodeId>({.ir_name = "struct_init"});

  TypeId type_id;
  InstBlockId elements_id;
  InstId dest_id;
};

// A literal struct value, such as `{.a = 1, .b = 2}`.
struct StructLiteral {
  static constexpr auto Kind =
      InstKind::StructLiteral.Define<Parse::StructLiteralId>(
          {.ir_name = "struct_literal"});

  TypeId type_id;
  InstBlockId elements_id;
};

// The type of a struct.
struct StructType {
  static constexpr auto Kind =
      InstKind::StructType.Define<Parse::StructTypeLiteralId>(
          {.ir_name = "struct_type",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  StructTypeFieldsId fields_id;
};

// A struct value.
struct StructValue {
  static constexpr auto Kind = InstKind::StructValue.Define<Parse::NodeId>(
      {.ir_name = "struct_value",
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  InstBlockId elements_id;
};

// A temporary value.
struct Temporary {
  static constexpr auto Kind =
      InstKind::Temporary.Define<Parse::NodeId>({.ir_name = "temporary"});

  TypeId type_id;
  InstId storage_id;
  InstId init_id;
};

// Storage for a temporary value.
struct TemporaryStorage {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind = InstKind::TemporaryStorage.Define<Parse::NodeId>(
      {.ir_name = "temporary_storage"});

  TypeId type_id;
};

// Access to a tuple member. Versus `TupleIndex`, this handles access where
// the index was inferred rather than being specified as an expression,
// such as `var tuple: (i32, i32) = (0, 1)` needing to access the `i32` values
// for assignment.
struct TupleAccess {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::TupleAccess.Define<Parse::NodeId>({.ir_name = "tuple_access"});

  TypeId type_id;
  InstId tuple_id;
  ElementIndex index;
};

// Initializes the destination tuple with the given elements.
struct TupleInit {
  static constexpr auto Kind =
      InstKind::TupleInit.Define<Parse::NodeId>({.ir_name = "tuple_init"});

  TypeId type_id;
  InstBlockId elements_id;
  InstId dest_id;
};

// A literal tuple value.
struct TupleLiteral {
  static constexpr auto Kind =
      InstKind::TupleLiteral.Define<Parse::TupleLiteralId>(
          {.ir_name = "tuple_literal"});

  TypeId type_id;
  InstBlockId elements_id;
};

// The type of a tuple.
struct TupleType {
  static constexpr auto Kind = InstKind::TupleType.Define<Parse::InvalidNodeId>(
      {.ir_name = "tuple_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  TypeBlockId elements_id;
};

// A tuple value.
struct TupleValue {
  static constexpr auto Kind = InstKind::TupleValue.Define<Parse::NodeId>(
      {.ir_name = "tuple_value",
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  InstBlockId elements_id;
};

// Tracks expressions which are valid as types. This has a deliberately
// self-referential type.
//
// TODO: Annotate as a builtin.
struct TypeType {
  static constexpr auto Kind = InstKind::TypeType.Define<Parse::InvalidNodeId>(
      {.ir_name = "type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// The `not` operator, such as `not operand`.
struct UnaryOperatorNot {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::UnaryOperatorNot.Define<Parse::NodeId>({.ir_name = "not"});

  TypeId type_id;
  InstId operand_id;
};

// The type of an expression naming an unbound element of a class, such as
// `Class.field`. This can be used as the operand of a compound member access
// expression, such as `instance.(Class.field)`.
struct UnboundElementType {
  static constexpr auto Kind = InstKind::UnboundElementType.Define<
      Parse::NodeIdOneOf<Parse::BaseDeclId, Parse::BindingPatternId>>(
      {.ir_name = "unbound_element_type",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  // The class that a value of this type is an element of.
  TypeId class_type_id;
  // The type of the element.
  TypeId element_type_id;
};

// Converts from a value expression to an ephemeral reference expression, in
// the case where the value representation of the type is a pointer. For
// example, when indexing a value expression of array type, this is used to
// form a reference to the array object.
struct ValueAsRef {
  static constexpr auto Kind = InstKind::ValueAsRef.Define<Parse::IndexExprId>(
      {.ir_name = "value_as_ref"});

  TypeId type_id;
  InstId value_id;
};

// Converts an initializing expression to a value expression, in the case
// where the initializing representation is the same as the value
// representation.
struct ValueOfInitializer {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::ValueOfInitializer.Define<Parse::NodeId>(
          {.ir_name = "value_of_initializer"});

  TypeId type_id;
  InstId init_id;
};

// Tracks storage for a `var` declaration.
struct VarStorage {
  // TODO: Make Parse::NodeId more specific.
  static constexpr auto Kind =
      InstKind::VarStorage.Define<Parse::NodeId>({.ir_name = "var"});

  TypeId type_id;
  NameId name_id;
};

// The type of virtual function tables.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct VtableType {
  static constexpr auto Kind =
      InstKind::VtableType.Define<Parse::InvalidNodeId>(
          {.ir_name = "<vtable>",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// An `expr where requirements` expression.
struct WhereExpr {
  static constexpr auto Kind = InstKind::WhereExpr.Define<Parse::WhereExprId>(
      {.ir_name = "where_expr",
       .is_type = InstIsType::Always,
       .constant_kind = InstConstantKind::Conditional});

  TypeId type_id;
  // This is the `.Self` symbolic binding. Its type matches the left type
  // argument of the `where`.
  InstId period_self_id;
  InstBlockId requirements_id;
};

// The type of witnesses.
//
// Although this is a builtin, it may still evolve to a more standard type and
// be removed.
struct WitnessType {
  static constexpr auto Kind =
      InstKind::WitnessType.Define<Parse::InvalidNodeId>(
          {.ir_name = "<witness>",
           .is_type = InstIsType::Always,
           .constant_kind = InstConstantKind::Always});

  TypeId type_id;
};

// These concepts are an implementation detail of the library, not public API.
namespace Internal {

// HasNodeId is true if T has an associated parse node.
template <typename T>
concept HasNodeId = !std::same_as<typename decltype(T::Kind)::TypedNodeId,
                                  Parse::InvalidNodeId>;

// HasUntypedNodeId is true if T has an associated parse node which can be any
// kind of node.
template <typename T>
concept HasUntypedNodeId =
    std::same_as<typename decltype(T::Kind)::TypedNodeId, Parse::NodeId>;

// HasKindMemberAsField<T> is true if T has a `InstKind kind` field, as opposed
// to a `static constexpr InstKind::Definition Kind` member or no kind at all.
template <typename T>
concept HasKindMemberAsField = requires {
  { &T::kind } -> std::same_as<InstKind T::*>;
};

// HasTypeIdMember<T> is true if T has a `TypeId type_id` field.
template <typename T>
concept HasTypeIdMember = requires {
  { &T::type_id } -> std::same_as<TypeId T::*>;
};

}  // namespace Internal

}  // namespace Carbon::SemIR

#endif  // CARBON_TOOLCHAIN_SEM_IR_TYPED_INSTS_H_
