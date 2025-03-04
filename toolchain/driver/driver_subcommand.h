// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_DRIVER_DRIVER_SUBCOMMAND_H_
#define CARBON_TOOLCHAIN_DRIVER_DRIVER_SUBCOMMAND_H_

#include "common/command_line.h"
#include "common/ostream.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "toolchain/driver/driver_env.h"
#include "toolchain/install/install_paths.h"

namespace Carbon {

// The result of a driver run.
struct DriverResult {
  // Overall success result.
  bool success;

  // Per-file success results. May be empty if files aren't individually
  // processed.
  llvm::SmallVector<std::pair<std::string, bool>> per_file_success = {};
};

// A subcommand for the driver.
class DriverSubcommand {
 public:
  explicit DriverSubcommand(CommandLine::CommandInfo info) : info_(info) {}

  // Adds the subcommand to the main command, assigning `selected_command` when
  // the subcommand is in use.
  auto AddTo(CommandLine::CommandBuilder& b,
             DriverSubcommand** selected_subcommand) -> void {
    b.AddSubcommand(
        info_, [this, selected_subcommand](CommandLine::CommandBuilder& sub_b) {
          BuildOptionsAndSetAction(sub_b, selected_subcommand);
        });
  }

  // Adds command line options.
  virtual auto BuildOptions(CommandLine::CommandBuilder& b) -> void = 0;

  // Adds command line options and registers the `Run` method to be called.
  //
  // For more complex subcommands that need to control the action this method
  // can be overridden and bypass the simple API above.
  //
  // TODO: This isn't the most elegant way to manage this. There is probably a
  // better factoring / organization of the driver subcommand infrastructure
  // that bakes in the needed flexibility here, but that was deferred for a
  // future refactoring.
  virtual auto BuildOptionsAndSetAction(CommandLine::CommandBuilder& b,
                                        DriverSubcommand** selected_subcommand)
      -> void {
    BuildOptions(b);
    b.Do([this, selected_subcommand] { *selected_subcommand = this; });
  }

  // Runs the command.
  virtual auto Run(DriverEnv& driver_env) -> DriverResult = 0;

 protected:
  // Diagnoses and returns false if currently fuzzing.
  //
  // This should be used in subcommands to check and diagnose rather than
  // entering them during fuzzing when they use external libraries that we can't
  // keep fuzz-clean.
  auto DisableFuzzingExternalLibraries(DriverEnv& driver_env,
                                       llvm::StringRef name) -> bool;

 private:
  // Subcommand information.
  CommandLine::CommandInfo info_;
};

}  // namespace Carbon

#endif  // CARBON_TOOLCHAIN_DRIVER_DRIVER_SUBCOMMAND_H_
