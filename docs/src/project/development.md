# Development

cl-yaml-kit requires SBCL. The Nix flake defines outputs for `x86_64-linux`
and `aarch64-darwin`; CI runs on `x86_64-linux`. Run commands from the
repository root.

## Nix

`nix develop` provides SBCL and the pinned runtime and test dependencies on
`CL_SOURCE_REGISTRY`:

```sh
nix develop
```

Run the repository checks with:

```sh
nix flake check
```

This gate runs the test suite, formatting and documentation checks, the
coverage gate, and paredit-lint. See [Testing](development/testing.md) and
[Coverage](development/coverage.md) for those checks. [Benchmarking](development/benchmarking.md)
is a separate workflow.

## Manual setup

Install SBCL and Git. On macOS, install GNU coreutils to use `gtimeout` for
bounded test commands. Create a source-registry directory and check out the
versions pinned by `flake.nix`:

```sh
mkdir -p "$HOME/lisp-deps"
git clone --branch v2.2.0 https://github.com/nerima-lisp/cl-regex-kit "$HOME/lisp-deps/cl-regex-kit"
git clone --branch v0.6.0 https://github.com/nerima-lisp/cl-codec-kit "$HOME/lisp-deps/cl-codec-kit"
git clone --branch v1.1.1 https://github.com/nerima-lisp/cl-parser-kit "$HOME/lisp-deps/cl-parser-kit"
git clone --branch v1.4.0 https://github.com/nerima-lisp/cl-weave "$HOME/lisp-deps/cl-weave"
git clone --branch v1.5.0 https://github.com/nerima-lisp/cl-prolog-kit "$HOME/lisp-deps/cl-prolog-kit"
git clone --branch v1.2.0 https://github.com/nerima-lisp/cl-json-kit "$HOME/lisp-deps/cl-json-kit"
git clone --branch data-2022-01-17 https://github.com/yaml/yaml-test-suite "$HOME/yaml-test-suite"

export CL_SOURCE_REGISTRY="$HOME/lisp-deps//"
export YAML_TEST_SUITE="$HOME/yaml-test-suite"
```

Make any transitive ASDF systems available through the same registry. Use the
[test instructions](development/testing.md) to run the suite.
