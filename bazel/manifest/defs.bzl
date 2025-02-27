# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

"""Rule for producing a manifest for a filegroup."""

def _get_files(ctx):
    files = []
    for src in ctx.attr.srcs:
        files.extend([f.path for f in src[DefaultInfo].files.to_list()])
        files.extend([
            f.path
            for f in src[DefaultInfo].default_runfiles.files.to_list()
        ])

    if ctx.attr.strip_package_dir:
        package_dir = ctx.label.package + "/"
        files_stripped = [f.removeprefix(package_dir) for f in files]
    else:
        files_stripped = files

    return files_stripped

def _manifest(ctx):
    out = ctx.actions.declare_file(ctx.label.name)
    files = _get_files(ctx)
    ctx.actions.write(out, "\n".join(files) + "\n")

    return [
        DefaultInfo(
            files = depset(direct = [out]),
            runfiles = ctx.runfiles(files = [out]),
        ),
    ]

# Produces the manifest as a series of lines.
manifest = rule(
    implementation = _manifest,
    attrs = {
        "srcs": attr.label_list(allow_files = True, mandatory = True),
        "strip_package_dir": attr.bool(default = False),
    },
)

def _manifest_as_cpp(ctx):
    out = ctx.actions.declare_file(ctx.label.name)
    files = _get_files(ctx)
    lines = [
        "// Auto-generated by manifest_as_cpp.",
        "const char* {0}[] = {{".format(ctx.attr.var_name),
    ]
    lines += [
        "    \"{0}\",".format(file)
        for file in files
    ]
    lines += [
        "    nullptr,",
        "};",
    ]
    ctx.actions.write(out, "\n".join(lines) + "\n")

    return [
        DefaultInfo(
            files = depset(direct = [out]),
            runfiles = ctx.runfiles(files = [out]),
        ),
    ]

# Produces the manifest as a nullptr-terminated `const char* var_name[]`.
# Use with `extern const char* var_name[];`.
manifest_as_cpp = rule(
    implementation = _manifest_as_cpp,
    attrs = {
        "srcs": attr.label_list(allow_files = True, mandatory = True),
        "strip_package_dir": attr.bool(default = False),
        "var_name": attr.string(mandatory = True),
    },
)
