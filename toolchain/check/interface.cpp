// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/interface.h"

#include "toolchain/check/context.h"
#include "toolchain/check/eval.h"
#include "toolchain/check/generic.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/type.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

auto BuildAssociatedEntity(Context& context, SemIR::InterfaceId interface_id,
                           SemIR::InstId decl_id) -> SemIR::InstId {
  auto& interface_info = context.interfaces().Get(interface_id);
  if (!interface_info.is_being_defined()) {
    // This should only happen if the interface is erroneously defined more than
    // once.
    // TODO: Find a way to CHECK this.
    return SemIR::ErrorInst::SingletonInstId;
  }

  // The interface type is the type of `Self`.
  auto self_type_id =
      context.insts().Get(interface_info.self_param_id).type_id();

  // Register this declaration as declaring an associated entity.
  auto index = SemIR::ElementIndex(
      context.args_type_info_stack().PeekCurrentBlockContents().size());
  context.args_type_info_stack().AddInstId(decl_id);

  // Name lookup for the declaration's name should name the associated entity,
  // not the declaration itself.
  auto type_id = GetAssociatedEntityType(context, self_type_id);
  return AddInst<SemIR::AssociatedEntity>(
      context, context.insts().GetLocId(decl_id),
      {.type_id = type_id, .index = index, .decl_id = decl_id});
}

// Returns the `Self` binding for an interface, given a specific for the
// interface and a generic for an associated entity within it.
static auto GetSelfBinding(Context& context,
                           SemIR::SpecificId interface_specific_id,
                           SemIR::GenericId assoc_entity_generic_id)
    -> SemIR::InstId {
  const auto& generic = context.generics().Get(assoc_entity_generic_id);
  auto bindings = context.inst_blocks().Get(generic.bindings_id);
  auto interface_args_id =
      context.specifics().GetArgsOrEmpty(interface_specific_id);
  auto interface_args = context.inst_blocks().Get(interface_args_id);

  // The `Self` binding is the first binding after the interface's arguments.
  auto self_binding_id = bindings[interface_args.size()];

  // Check that we found the self binding. The binding might be a
  // `BindSymbolicName` or an `ImportRef` naming one.
  auto self_binding_const_inst_id =
      context.constant_values().GetConstantInstId(self_binding_id);
  auto bind_name_inst = context.insts().GetAs<SemIR::BindSymbolicName>(
      self_binding_const_inst_id);
  CARBON_CHECK(
      context.entity_names().Get(bind_name_inst.entity_name_id).name_id ==
          SemIR::NameId::SelfType,
      "Expected a Self binding, found {0}", bind_name_inst);

  return self_binding_id;
}

// Given a `Self` type and a witness that it implements an interface, along with
// that interface's `Self` binding, forms and returns a facet that can be used
// as the argument for that `Self` binding.
static auto GetSelfFacet(Context& context,
                         SemIR::SpecificId interface_specific_id,
                         SemIR::GenericId generic_id,
                         SemIR::TypeId self_type_id,
                         SemIR::InstId self_witness_id) -> SemIR::InstId {
  auto self_binding_id =
      GetSelfBinding(context, interface_specific_id, generic_id);
  auto self_binding = context.insts().Get(self_binding_id);
  auto self_facet_type_id = SemIR::GetTypeInSpecific(
      context.sem_ir(), interface_specific_id, self_binding.type_id());
  // Create a facet value to be the value of `Self` in the interface.
  // TODO: Pass this in instead of creating it here. The caller sometimes
  // already has a facet value.
  auto type_inst_id = context.types().GetInstId(self_type_id);
  auto self_value_const_id =
      TryEvalInst(context, SemIR::InstId::None,
                  SemIR::FacetValue{.type_id = self_facet_type_id,
                                    .type_inst_id = type_inst_id,
                                    .witness_inst_id = self_witness_id});
  return context.constant_values().GetInstId(self_value_const_id);
}

// Builds and returns the argument list from `interface_specific_id` with a
// value for the `Self` parameter of `generic_id` appended.
static auto GetGenericArgsWithSelfType(Context& context,
                                       SemIR::SpecificId interface_specific_id,
                                       SemIR::GenericId generic_id,
                                       SemIR::TypeId self_type_id,
                                       SemIR::InstId witness_inst_id,
                                       std::size_t reserve_args_size = 0)
    -> llvm::SmallVector<SemIR::InstId> {
  auto interface_args_id =
      context.specifics().GetArgsOrEmpty(interface_specific_id);
  auto interface_args = context.inst_blocks().Get(interface_args_id);

  llvm::SmallVector<SemIR::InstId> arg_ids;
  arg_ids.reserve(std::max(reserve_args_size, interface_args.size() + 1));

  // Start with the enclosing arguments from the interface.
  arg_ids.assign(interface_args.begin(), interface_args.end());

  // Add the `Self` argument.
  arg_ids.push_back(GetSelfFacet(context, interface_specific_id, generic_id,
                                 self_type_id, witness_inst_id));

  return arg_ids;
}

auto GetSelfSpecificForInterfaceMemberWithSelfType(
    Context& context, SemIRLoc loc, SemIR::SpecificId interface_specific_id,
    SemIR::GenericId generic_id, SemIR::TypeId self_type_id,
    SemIR::InstId witness_inst_id) -> SemIR::SpecificId {
  const auto& generic = context.generics().Get(generic_id);
  auto self_specific_args = context.inst_blocks().Get(
      context.specifics().Get(generic.self_specific_id).args_id);

  auto arg_ids = GetGenericArgsWithSelfType(
      context, interface_specific_id, generic_id, self_type_id, witness_inst_id,
      self_specific_args.size());

  // Take any trailing argument values from the self specific.
  // TODO: If these refer to outer arguments, for example in their types, we may
  // need to perform extra substitutions here.
  for (auto arg_id : self_specific_args.drop_front(arg_ids.size())) {
    arg_ids.push_back(context.constant_values().GetConstantInstId(arg_id));
  }

  return MakeSpecific(context, loc, generic_id, arg_ids);
}

auto GetTypeForSpecificAssociatedEntity(Context& context, SemIRLoc loc,
                                        SemIR::SpecificId interface_specific_id,
                                        SemIR::InstId decl_id,
                                        SemIR::TypeId self_type_id,
                                        SemIR::InstId self_witness_id)
    -> SemIR::TypeId {
  auto decl =
      context.insts().Get(context.constant_values().GetConstantInstId(decl_id));
  if (auto assoc_const = decl.TryAs<SemIR::AssociatedConstantDecl>()) {
    // Form a specific for the associated constant, and grab the type from
    // there.
    auto generic_id = context.associated_constants()
                          .Get(assoc_const->assoc_const_id)
                          .generic_id;
    auto arg_ids =
        GetGenericArgsWithSelfType(context, interface_specific_id, generic_id,
                                   self_type_id, self_witness_id);
    auto const_specific_id = MakeSpecific(context, loc, generic_id, arg_ids);
    return SemIR::GetTypeInSpecific(context.sem_ir(), const_specific_id,
                                    context.insts().Get(decl_id).type_id());
  } else if (auto fn = context.types().TryGetAs<SemIR::FunctionType>(
                 decl.type_id())) {
    // Form the type of the function within the interface, and attach the `Self`
    // type.
    auto interface_fn_type_id =
        SemIR::GetTypeInSpecific(context.sem_ir(), interface_specific_id,
                                 context.insts().Get(decl_id).type_id());
    auto self_facet_id =
        GetSelfFacet(context, interface_specific_id,
                     context.functions().Get(fn->function_id).generic_id,
                     self_type_id, self_witness_id);
    return GetFunctionTypeWithSelfType(
        context, context.types().GetInstId(interface_fn_type_id),
        self_facet_id);
  } else {
    CARBON_FATAL("Unexpected kind for associated constant {0}", decl);
  }
}

}  // namespace Carbon::Check
