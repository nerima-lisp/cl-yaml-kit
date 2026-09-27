# Development

The supported workflow is Nix: `flake.nix` is one `cl-nix-forge`
`mkPackageFlake` call, so the packages, checks, apps, dev shell, and
formatter below are its standard outputs plus one custom benchmark app.
Run every command from the repository root.

## Nix

```sh
nix flake check          # test suite, formatting gate, docs build
nix develop              # shell with the dependencies on the source registry
nix run .#test           # the test suite
nix run .#benchmark      # the deterministic benchmark
nix build .#docs         # the documentation site
nix fmt                  # format the Nix files
```

`nix flake check` is the repository gate. It builds the test suite, the
treefmt formatting gate, and the documentation site, and fails on any of
them. The suite runs against the `yaml-test-suite` checkout pinned in
`flake.nix`: `packageArgs` passes its store path into the package
derivation as `YAML_TEST_SUITE`, and the test check inherits that
environment. The docs build runs `mkdocs build --strict`, so a broken
internal link fails the gate instead of warning.

`nix develop` opens a shell with SBCL and the runtime and test dependencies
on `CL_SOURCE_REGISTRY`. The check-enabled package derivation backs the
shell, which is why the test framework is on the registry. Iterate with:

```sh
sbcl --script run-tests.lisp
```

The shell and the apps do not set `YAML_TEST_SUITE`. The conformance
stages require it, so export it before running the suite outside the flake
check:

```sh
export YAML_TEST_SUITE="$HOME/yaml-test-suite"   # data-2022-01-17 checkout
```

`nix run .#test` runs `run-tests.lisp` from the flake's pinned source tree
with the dependencies on the registry; it drives the same runner the check
does. `nix run .#benchmark` runs `benchmark/run.lisp`, a deterministic
corpus benchmark that prints one TSV row per case and operation with
throughput, allocation, and GC time. `nix build .#docs` builds the
documentation site. `nix fmt` rewrites the Nix files to the treefmt
configuration; the formatting check fails when they are not.

## Without Nix

Nix is the supported path. This procedure is the fallback for a checkout
where `nix develop` is unavailable. `run-tests.lisp` registers the
repository itself, so only two things need setting up: the dependencies on
the source registry, and the conformance fixture.

The runtime dependencies are `cl-regex-kit` and `cl-codec-kit`; the test
system adds `cl-weave` and `cl-json-kit` (see `cl-yaml-kit.asd`). Check
them out under one directory and register that directory as a source
registry tree:

```sh
mkdir -p ~/lisp-deps
git clone --branch v2.1.1 https://github.com/nerima-lisp/cl-regex-kit ~/lisp-deps/cl-regex-kit
git clone --branch v0.5.0 https://github.com/nerima-lisp/cl-codec-kit ~/lisp-deps/cl-codec-kit
git clone --branch v1.3.0 https://github.com/nerima-lisp/cl-weave ~/lisp-deps/cl-weave
git clone --branch v1.2.0 https://github.com/nerima-lisp/cl-json-kit ~/lisp-deps/cl-json-kit
git clone --branch data-2022-01-17 https://github.com/yaml/yaml-test-suite ~/yaml-test-suite

export CL_SOURCE_REGISTRY="$HOME/lisp-deps//"
export YAML_TEST_SUITE="$HOME/yaml-test-suite"
```

The tags match the pins in `flake.nix`. These systems have dependencies of
their own; ASDF reports each missing system by name, so add whatever it
names to `~/lisp-deps` the same way. Any other way of putting the systems
on the registry, Quicklisp or one source registry entry per directory,
works the same.

Then run the suite from the repository root:

```sh
sbcl --script run-tests.lisp
```

## The test suite

`run-tests.lisp` registers the repository directory as a source registry
tree and runs `asdf:test-system "cl-yaml-kit"`, which loads
`cl-yaml-kit/test` and runs the cl-weave unit tests followed by the five
conformance stages. It exits 0 only when everything passed.

The fixture is a requirement, not an option. With `YAML_TEST_SUITE` unset,
the conformance stage signals an error instead of skipping its cases, and
`scripts/run-coverage.lisp` refuses to start.

Each test has a per-test timeout of 120 seconds by default;
`CL_YAML_TEST_TIMEOUT_MS` overrides it.

The stages compare different representations, on purpose:

- The reader stage parses `test.event` into event signatures and compares
  them with `equal`. A signature carries the event kind and the fields
  that matter: explicit document markers, collection flow style, anchors,
  tags, scalar style, and scalar value.
- The loader stage compares `in.json` as a JSON data model, one JSON
  value per document, with object key order and whitespace ignored while
  null, false, integer, and float values stay distinct. `out.yaml` is
  compared semantically after dumping.
- The `emitter-isolated` stage feeds the parsed events to the emitter and
  requires `string=` equality with `emit.yaml`, the suite's libyaml-based
  emitter fixture. Whitespace, line breaks, quoting, indentation, and flow
  or block style are all part of that contract; a meaning-only comparison
  would let representation differences through.

## Coverage

`scripts/run-coverage.lisp` runs the whole suite under SB-COVER, so the
unit tests and the conformance stages count toward the same totals.
Coverage is reset after the systems are loaded, so the numbers reflect
only the suite run. It needs `YAML_TEST_SUITE` like the plain run, writes
all three artifacts even when the suite fails, and exits non-zero unless
the suite passed:

```sh
sbcl --noinform --script scripts/run-coverage.lisp
```

The artifacts default under `/tmp/cl-yaml-kit-coverage/`: the coverage
data file, an HTML report restricted to the source directory, and the
tab-separated per-file summary the gate reads. `COVERAGE_OUTPUT`,
`COVERAGE_REPORT_DIRECTORY`, and `COVERAGE_SUMMARY` move them;
`COVERAGE_SOURCE_DIRECTORY` (default `src/`) selects the files measured
and reported.

`scripts/check-coverage.pl` turns the summary into the gate. It prints a
Markdown table and exits 0 when every file meets both thresholds, 1 when
a file is below one, and 2 when no report could be produced. The interface
is `--input TSV`, `--min-line PERCENT`, and `--min-branch PERCENT`; each
setting falls back to `COVERAGE_SUMMARY`, `CL_YAML_COVERAGE_MIN_LINE`, or
`CL_YAML_COVERAGE_MIN_BRANCH`, then to the default. Both thresholds
default to 100. A file whose total for a kind is zero prints `n/a` and is
exempt from that kind.

```sh
perl scripts/check-coverage.pl
```
