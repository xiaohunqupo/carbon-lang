// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// This library contains functions to assist dumping objects to stderr during
// interactive debugging. Functions named `Dump` are intended for direct use by
// developers, and should use overload resolution to determine which will be
// invoked. The debugger should do namespace resolution automatically. For
// example:
//
// - lldb: `expr Dump(tokens, id)`
// - gdb: `call Dump(tokens, id)`

#ifndef CARBON_TOOLCHAIN_SEM_IR_DUMP_H_
#define CARBON_TOOLCHAIN_SEM_IR_DUMP_H_

#ifndef NDEBUG

#include "toolchain/sem_ir/file.h"

namespace Carbon::SemIR {

auto Dump(const File& file, ClassId class_id) -> void;
auto Dump(const File& file, ConstantId const_id) -> void;
auto Dump(const File& file, EntityNameId entity_name_id) -> void;
auto Dump(const File& file, FacetTypeId facet_type_id) -> void;
auto Dump(const File& file, FunctionId function_id) -> void;
auto Dump(const File& file, GenericId generic_id) -> void;
auto Dump(const File& file, ImplId impl_id) -> void;
auto Dump(const File& file, InstBlockId inst_block_id) -> void;
auto Dump(const File& file, InstId inst_id) -> void;
auto Dump(const File& file, InterfaceId interface_id) -> void;
auto Dump(const File& file, NameId name_id) -> void;
auto Dump(const File& file, NameScopeId name_scope_id) -> void;
auto Dump(const File& file, CompleteFacetTypeId complete_facet_type_id) -> void;
auto Dump(const File& file, SpecificId specific_id) -> void;
auto Dump(const File& file, StructTypeFieldsId struct_type_fields_id) -> void;
auto Dump(const File& file, TypeBlockId type_block_id) -> void;
auto Dump(const File& file, TypeId type_id) -> void;

}  // namespace Carbon::SemIR

#endif  // NDEBUG

#endif  // CARBON_TOOLCHAIN_SEM_IR_DUMP_H_
