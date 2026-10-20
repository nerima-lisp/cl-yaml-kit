# Testing

## Test suite

Run the suite with the flake-pinned fixture:

```sh
nix run .#test
```

The Nix app sets `YAML_TEST_SUITE` to the pinned fixture. The manual test
runner requires that variable and errors if it is unset. Configure the source
registry and fixture as described in
[Development](../development.md), then run:

```sh
timeout -k 30s 1200s sbcl --script run-tests.lisp
```

On macOS, replace `timeout` with `gtimeout` from GNU coreutils. The runner
checks compilation warnings, registers the repository as an ASDF source tree,
and invokes `asdf:test-system`. The suite runs unit tests and the conformance
stages. A run that selects no unit tests fails.

The unit runner accepts cl-weave's `CL_WEAVE_*` variables, including
`CL_WEAVE_REPORTER`, `CL_WEAVE_TEST_FILTER`, and
`CL_WEAVE_TEST_TIMEOUT_MS`. Each test has a 120-second timeout unless the
timeout variable sets another value.

The test suite uses the `yaml-test-suite` fixture. The reader stage compares
event signatures; the loader stages compare constructed values; the emitter
stage compares emitted text; and the dumper stage checks semantic round trips.
See [Conformance](../../reference/conformance.md) for the limits of those
comparisons.

## Mutation testing

Run the mutation checks with:

```sh
nix run .#mutation
```

The runner starts a fresh SBCL process for each mutant. Set
`CL_YAML_MUTATION_AREA` to `reader`, `loader`, or `dumper` to select one area;
leave it unset to run all areas. Each mutant runs the tests selected for its
function. Surviving, errored, or timed-out mutants fail the command. A stale
equivalence rule also fails it.

Without Nix, use the source registry and fixture from
[Development](../development.md). Configure a writable ASDF cache for the
isolated child processes, then run:

```sh
mkdir -p "$HOME/.cache/cl-yaml-kit-mutation"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t \"$HOME/.cache/cl-yaml-kit-mutation/\") :inherit-configuration)"
CL_YAML_MUTATION_AREA=reader \
  timeout -k 30s 3600s sbcl --dynamic-space-size 4096 --script scripts/run-mutation.lisp
```

On macOS, use `gtimeout` in place of `timeout`.
