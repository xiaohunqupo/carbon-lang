// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/impl_lookup.h"

#include "toolchain/check/deduce.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/check/generic.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/type_completion.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/impl.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

static auto FindAssociatedImportIRs(Context& context,
                                    SemIR::ConstantId type_const_id,
                                    SemIR::ConstantId interface_const_id)
    -> llvm::SmallVector<SemIR::ImportIRId> {
  llvm::SmallVector<SemIR::ImportIRId> result;

  // Add an entity to our result.
  auto add_entity = [&](const SemIR::EntityWithParamsBase& entity) {
    // We will look for impls in the import IR associated with the first owning
    // declaration.
    auto decl_id = entity.first_owning_decl_id;
    if (!decl_id.has_value()) {
      return;
    }
    if (auto ir_id = GetCanonicalImportIRInst(context, decl_id).ir_id;
        ir_id.has_value()) {
      result.push_back(ir_id);
    }
  };

  llvm::SmallVector<SemIR::InstId> worklist;
  worklist.push_back(context.constant_values().GetInstId(type_const_id));
  worklist.push_back(context.constant_values().GetInstId(interface_const_id));

  // Push the contents of an instruction block onto our worklist.
  auto push_block = [&](SemIR::InstBlockId block_id) {
    if (block_id.has_value()) {
      llvm::append_range(worklist, context.inst_blocks().Get(block_id));
    }
  };

  // Add the arguments of a specific to the worklist.
  auto push_args = [&](SemIR::SpecificId specific_id) {
    if (specific_id.has_value()) {
      push_block(context.specifics().Get(specific_id).args_id);
    }
  };

  while (!worklist.empty()) {
    auto inst_id = worklist.pop_back_val();

    // Visit the operands of the constant.
    auto inst = context.insts().Get(inst_id);
    auto [arg0_kind, arg1_kind] = inst.ArgKinds();
    for (auto [arg, kind] :
         {std::pair{inst.arg0(), arg0_kind}, {inst.arg1(), arg1_kind}}) {
      switch (kind) {
        case SemIR::IdKind::For<SemIR::InstId>: {
          if (auto id = SemIR::InstId(arg); id.has_value()) {
            worklist.push_back(id);
          }
          break;
        }
        case SemIR::IdKind::For<SemIR::InstBlockId>: {
          push_block(SemIR::InstBlockId(arg));
          break;
        }
        case SemIR::IdKind::For<SemIR::ClassId>: {
          add_entity(context.classes().Get(SemIR::ClassId(arg)));
          break;
        }
        case SemIR::IdKind::For<SemIR::InterfaceId>: {
          add_entity(context.interfaces().Get(SemIR::InterfaceId(arg)));
          break;
        }
        case SemIR::IdKind::For<SemIR::FacetTypeId>: {
          const auto& facet_type_info =
              context.facet_types().Get(SemIR::FacetTypeId(arg));
          for (const auto& impl : facet_type_info.impls_constraints) {
            add_entity(context.interfaces().Get(impl.interface_id));
            push_args(impl.specific_id);
          }
          break;
        }
        case SemIR::IdKind::For<SemIR::FunctionId>: {
          add_entity(context.functions().Get(SemIR::FunctionId(arg)));
          break;
        }
        case SemIR::IdKind::For<SemIR::SpecificId>: {
          push_args(SemIR::SpecificId(arg));
          break;
        }
        default: {
          break;
        }
      }
    }
  }

  // Deduplicate.
  llvm::sort(result, [](SemIR::ImportIRId a, SemIR::ImportIRId b) {
    return a.index < b.index;
  });
  result.erase(llvm::unique(result), result.end());

  return result;
}

// Returns true if a cycle was found and diagnosed.
static auto FindAndDiagnoseImplLookupCycle(
    Context& context,
    const llvm::SmallVector<Context::ImplLookupStackEntry>& stack,
    SemIR::LocId loc_id, SemIR::ConstantId type_const_id,
    SemIR::ConstantId interface_const_id) -> bool {
  // Deduction of the interface parameters can do further impl lookups, and we
  // need to ensure we terminate.
  //
  // https://docs.carbon-lang.dev/docs/design/generics/details.html#acyclic-rule
  // - We look for violations of the acyclic rule by seeing if a previous lookup
  //   had all the same type inputs.
  // - The `interface_const_id` encodes the entire facet type being looked up,
  //   including any specific parameters for a generic interface.
  //
  // TODO: Implement the termination rule, which requires looking at the
  // complexity of the types on the top of (or throughout?) the stack:
  // https://docs.carbon-lang.dev/docs/design/generics/details.html#termination-rule
  for (auto [i, entry] : llvm::enumerate(stack)) {
    if (entry.type_const_id == type_const_id &&
        entry.interface_const_id == interface_const_id) {
      auto facet_type_type_id =
          context.types().GetTypeIdForTypeConstantId(interface_const_id);
      CARBON_DIAGNOSTIC(ImplLookupCycle, Error,
                        "cycle found in search for impl of {0} for type {1}",
                        SemIR::TypeId, SemIR::TypeId);
      auto builder = context.emitter().Build(
          loc_id, ImplLookupCycle, facet_type_type_id,
          context.types().GetTypeIdForTypeConstantId(type_const_id));
      for (const auto& active_entry : llvm::drop_begin(stack, i)) {
        if (active_entry.impl_loc.has_value()) {
          CARBON_DIAGNOSTIC(ImplLookupCycleNote, Note,
                            "determining if this impl clause matches", );
          builder.Note(active_entry.impl_loc, ImplLookupCycleNote);
        }
      }
      builder.Emit();
      return true;
    }
  }
  return false;
}

// Gets the `SemIR::InterfaceId` for a facet type (as a constant value).
//
// The facet type requires only one `InterfaceId` right now. But in the future,
// a facet type may include more than a single interface. For now that is
// unhandled with a TODO.
static auto GetInterfaceIdFromConstantId(Context& context, SemIR::LocId loc_id,
                                         SemIR::ConstantId interface_const_id)
    -> SemIR::InterfaceId {
  // The `interface_const_id` is a constant value for some facet type. We do
  // this long chain of steps to go from that constant value to the
  // `FacetTypeId` found on the `FacetType` instruction of this constant value,
  // and finally to the `CompleteFacetType`.
  auto facet_type_inst_id =
      context.constant_values().GetInstId(interface_const_id);
  auto facet_type_inst =
      context.insts().GetAs<SemIR::FacetType>(facet_type_inst_id);
  auto facet_type_id = facet_type_inst.facet_type_id;
  auto complete_facet_type_id =
      context.complete_facet_types().TryGetId(facet_type_id);
  // The facet type will already be completed before coming here. If we're
  // converting from a concrete type to a facet type, the conversion step
  // requires everything to be complete before doing impl lookup.
  CARBON_CHECK(complete_facet_type_id.has_value());
  const auto& complete_facet_type =
      context.complete_facet_types().Get(complete_facet_type_id);

  if (complete_facet_type.required_interfaces.empty()) {
    // This should never happen - a FacetType either requires or is bounded by
    // some `.Self impls` clause. Otherwise you would just have `type` (aka
    // `TypeType` in the toolchain implementation) which is not a facet type.
    context.TODO(loc_id,
                 "impl lookup for a FacetType with no interface (using "
                 "`where .Self impls ...` instead?)");
    return SemIR::InterfaceId::None;
  }
  if (complete_facet_type.required_interfaces.size() > 1) {
    context.TODO(loc_id,
                 "impl lookup for a FacetType with more than one interface");
    return SemIR::InterfaceId::None;
  }
  return complete_facet_type.required_interfaces[0].interface_id;
}

static auto GetWitnessIdForImpl(Context& context, SemIR::LocId loc_id,
                                SemIR::ConstantId type_const_id,
                                SemIR::ConstantId interface_const_id,
                                SemIR::InterfaceId interface_id,
                                const SemIR::Impl& impl) -> SemIR::InstId {
  // If impl.constraint_id is not symbolic, and doesn't match the query, then
  // we don't need to proceed.
  auto impl_interface_const_id =
      context.constant_values().Get(impl.constraint_id);
  if (!impl_interface_const_id.is_symbolic() &&
      interface_const_id != impl_interface_const_id) {
    return SemIR::InstId::None;
  }

  // This is the (single) interface named in the query `interface_const_id`.
  // If the impl's interface_id differs from the query, then this impl can not
  // possibly provide the queried interface, and we don't need to proceed.
  // Unlike the early-out above comparing the `impl.constraint_id`, this also
  // elides looking at impls of generic interfaces where the interface itself
  // does not match the query.
  if (impl.interface.interface_id != interface_id) {
    return SemIR::InstId::None;
  }

  auto specific_id = SemIR::SpecificId::None;
  // This check comes first to avoid deduction with an invalid impl. We use an
  // error value to indicate an error during creation of the impl, such as a
  // recursive impl which will cause deduction to recurse infinitely.
  if (impl.witness_id == SemIR::ErrorInst::SingletonInstId) {
    return SemIR::InstId::None;
  }
  if (impl.generic_id.has_value()) {
    specific_id = DeduceImplArguments(context, loc_id, impl, type_const_id,
                                      interface_const_id);
    if (!specific_id.has_value()) {
      return SemIR::InstId::None;
    }
  }
  if (!context.constant_values().AreEqualAcrossDeclarations(
          SemIR::GetConstantValueInSpecific(context.sem_ir(), specific_id,
                                            impl.self_id),
          type_const_id)) {
    return SemIR::InstId::None;
  }
  if (!context.constant_values().AreEqualAcrossDeclarations(
          SemIR::GetConstantValueInSpecific(context.sem_ir(), specific_id,
                                            impl.constraint_id),
          interface_const_id)) {
    // TODO: An impl of a constraint type should be treated as implementing
    // the constraint's interfaces.
    return SemIR::InstId::None;
  }
  if (!impl.witness_id.has_value()) {
    // TODO: Diagnose if the impl isn't defined yet?
    return SemIR::InstId::None;
  }
  LoadImportRef(context, impl.witness_id);
  if (specific_id.has_value()) {
    // We need a definition of the specific `impl` so we can access its
    // witness.
    ResolveSpecificDefinition(context, loc_id, specific_id);
  }
  return context.constant_values().GetInstId(SemIR::GetConstantValueInSpecific(
      context.sem_ir(), specific_id, impl.witness_id));
}

auto LookupImplWitness(Context& context, SemIR::LocId loc_id,
                       SemIR::ConstantId type_const_id,
                       SemIR::ConstantId interface_const_id) -> SemIR::InstId {
  if (type_const_id == SemIR::ErrorInst::SingletonConstantId ||
      interface_const_id == SemIR::ErrorInst::SingletonConstantId) {
    return SemIR::ErrorInst::SingletonInstId;
  }
  auto import_irs =
      FindAssociatedImportIRs(context, type_const_id, interface_const_id);
  for (auto import_ir : import_irs) {
    // TODO: Instead of importing all impls, only import ones that are in some
    // way connected to this query.
    for (auto impl_index : llvm::seq(
             context.import_irs().Get(import_ir).sem_ir->impls().size())) {
      // TODO: Track the relevant impls and only consider those ones and any
      // local impls, rather than looping over all impls below.
      ImportImpl(context, import_ir, SemIR::ImplId(impl_index));
    }
  }

  if (FindAndDiagnoseImplLookupCycle(context, context.impl_lookup_stack(),
                                     loc_id, type_const_id,
                                     interface_const_id)) {
    return SemIR::ErrorInst::SingletonInstId;
  }

  auto interface_id =
      GetInterfaceIdFromConstantId(context, loc_id, interface_const_id);

  auto result_witness_id = SemIR::InstId::None;

  auto& stack = context.impl_lookup_stack();
  stack.push_back({
      .type_const_id = type_const_id,
      .interface_const_id = interface_const_id,
  });
  for (const auto& impl : context.impls().array_ref()) {
    stack.back().impl_loc = impl.definition_id;
    result_witness_id = GetWitnessIdForImpl(
        context, loc_id, type_const_id, interface_const_id, interface_id, impl);
    if (result_witness_id.has_value()) {
      // We found a matching impl, don't keep looking.
      break;
    }
  }
  stack.pop_back();

  return result_witness_id;
}

}  // namespace Carbon::Check
