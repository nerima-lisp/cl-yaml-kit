# Development

The Nix workflow below is available on `x86_64-linux`. `flake.nix` uses one
`cl-nix-forge` `mkPackageFlake` call, so its packages, checks, apps, dev shell,
and formatter are the standard outputs plus custom benchmark and mutation apps.
On macOS, use the manual setup and commands in [Without Nix](#without-nix).
Run every command from the repository root.

## Nix

```sh
nix flake check          # test suite, formatting, docs build, coverage gate
nix develop              # shell with the dependencies on the source registry
nix run .#test           # the test suite
nix run .#benchmark      # the deterministic benchmark
nix run .#mutation       # mutation testing over the selected functions
nix build .#docs         # the documentation site
nix fmt                  # format the Nix files
```

`nix flake check` is the repository gate. It builds the test suite, the
treefmt formatting gate, the documentation site, and the coverage check, and
fails on any of them. The suite runs against the `yaml-test-suite` checkout pinned in
`flake.nix`: `packageArgs` passes its store path into the package
derivation as `YAML_TEST_SUITE`, and the test check inherits that
environment. The docs build runs `mkdocs build --strict`, so a broken
internal link fails the gate instead of warning.

`nix develop` opens a shell with SBCL and the runtime and test dependencies
on `CL_SOURCE_REGISTRY`. The check-enabled package derivation backs the
shell, which is why the test framework is on the registry. The test app also
sets `YAML_TEST_SUITE` to the flake input. Iterate with:

```sh
timeout -k 30s 1200s sbcl --script run-tests.lisp
```

The development shell does not set `YAML_TEST_SUITE`. The conformance stages
require it, so export it before running the suite outside the flake check:

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

Use this procedure on macOS or where `nix develop` is unavailable. The flake
currently provides Nix outputs only on `x86_64-linux`. Install SBCL and Git
first. `run-tests.lisp` registers the repository itself, so you then need to
set up two project inputs: dependencies on the source registry and the
conformance fixture. Install GNU coreutils before following the bounded
command examples (`brew install coreutils` on macOS); its timeout command is
`gtimeout` there. Replace `timeout` with `gtimeout` in the command examples on
macOS.

The runtime dependencies are `cl-regex-kit` and `cl-codec-kit`; the test
system adds `cl-weave` and `cl-json-kit` (see `cl-yaml-kit.asd`). Check
them out under one directory and register that directory as a source
registry tree:

```sh
mkdir -p ~/lisp-deps
git clone --branch v2.2.0 https://github.com/nerima-lisp/cl-regex-kit ~/lisp-deps/cl-regex-kit
git clone --branch v0.6.0 https://github.com/nerima-lisp/cl-codec-kit ~/lisp-deps/cl-codec-kit
git clone --branch v1.4.0 https://github.com/nerima-lisp/cl-weave ~/lisp-deps/cl-weave
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
timeout -k 30s 1200s sbcl --script run-tests.lisp
```

## The test suite

`run-tests.lisp` registers the repository directory as a source registry
tree and runs `asdf:test-system "cl-yaml-kit"`, which loads
`cl-yaml-kit/test` and runs the cl-weave unit tests followed by the five
conformance stages. It exits 0 only when everything passed.

The fixture is a requirement, not an option. With `YAML_TEST_SUITE` unset,
the conformance stage signals an error instead of skipping its cases, and
`scripts/run-coverage.lisp` refuses to start.

The unit run takes its options from cl-weave's `CL_WEAVE_*` environment
variables, such as `CL_WEAVE_REPORTER`, `CL_WEAVE_TEST_FILTER`, and
`CL_WEAVE_TEST_TIMEOUT_MS`. Each test has a per-test timeout of 120 seconds
unless `CL_WEAVE_TEST_TIMEOUT_MS` sets one, and a run that selects no tests
fails.

## Performance

`benchmark/run.lisp` uses `cl-weave:measure` for warmup, repeated samples,
iterations, and median elapsed time. The corpus is declared in one table and
covers reader (`parse-events`), loader (`parse`), and dumper (`emit`). The
public cl-weave benchmark API does not expose allocation measurements, so the
benchmark additionally records SBCL `get-bytes-consed`; GC/setup elapsed time
is retained as an auxiliary value.

The following run used a Mac16,6 with 16 CPUs and SBCL 2.6.0:

| case | stage | input bytes | MiB/s | consed bytes/op |
| --- | --- | ---: | ---: | ---: |
| large-block-mapping | reader | 10,752 | 7.857 | 2,228,864 |
| large-block-mapping | loader | 10,752 | 6.990 | 2,229,547 |
| large-block-mapping | dumper | 10,752 | 4.644 | 1,680,725 |
| large-block-sequence | reader | 6,656 | 11.215 | 1,273,685 |
| large-block-sequence | loader | 6,656 | 10.982 | 1,259,733 |
| large-block-sequence | dumper | 6,656 | 5.449 | 977,877 |
| deep-nesting | reader | 1,011,895 | 172.294 | 2,781,696 |
| deep-nesting | loader | 1,011,895 | 112.460 | 3,535,232 |
| deep-nesting | dumper | 1,011,895 | 72.313 | 17,448,053 |
| long-plain-scalar | reader | 65,543 | 47.971 | 896,405 |
| long-plain-scalar | loader | 65,543 | 49.886 | 896,747 |
| long-plain-scalar | dumper | 65,543 | 7.341 | 6,375,755 |
| long-double-quoted-scalar | reader | 65,545 | 34.843 | 896,107 |
| long-double-quoted-scalar | loader | 65,545 | 49.297 | 896,747 |
| long-double-quoted-scalar | dumper | 65,545 | 7.454 | 6,375,755 |
| long-block-literal | reader | 68,004 | 46.028 | 1,841,611 |
| long-block-literal | loader | 68,004 | 51.966 | 1,785,205 |
| long-block-literal | dumper | 68,004 | 2.973 | 6,243,813 |
| flow-collection-heavy | reader | 22,275 | 6.455 | 6,938,795 |
| flow-collection-heavy | loader | 22,275 | 3.401 | 8,232,747 |
| flow-collection-heavy | dumper | 22,275 | 3.363 | 5,273,813 |
| anchor-alias-heavy | reader | 2,222 | 8.979 | 461,141 |
| anchor-alias-heavy | loader | 2,222 | 7.383 | 504,704 |
| anchor-alias-heavy | dumper | 2,222 | 3.629 | 333,909 |
| realistic-config-1mb | reader | 1,048,765 | 11.252 | 40,496,853 |
| realistic-config-1mb | loader | 1,048,765 | 6.147 | 73,222,341 |
| realistic-config-1mb | dumper | 1,048,765 | 3.825 | 140,208,880 |

An sb-sprof run on the 1 MiB configuration identified scanner plain-scalar
work (`scan-plain-scalar`), cl-regex-kit matching during scalar construction,
and serializer traversal plus string-output allocation as the dominant paths.
The local changes made from that profile are bounded: plain-scalar boundary
checks avoid a temporary list, serializer traversal reuses its immutable mark,
and the emitter keeps a two-event FIFO without repeated `nconc` and `length`
scans. The reader before/after benchmark was noisy rather than uniformly
faster, so it is not presented as a universal speedup; the serializer and
emitter changes are intended to reduce allocation and queue overhead on the
corresponding hot paths. Re-running on an otherwise idle machine is expected
to produce different elapsed-time and allocation values.

The cl-regex-kit v2.1.1 to v2.2.0 loader comparison, using the same five-sample
benchmark settings, was:

| case | v2.1.1 MiB/s | v2.2.0 MiB/s | change |
| --- | ---: | ---: | ---: |
| flow-collection-heavy | 1.068 | 1.945 | +82.1% |
| realistic-config-1mb | 1.341 | 1.475 | +10.0% |

The loader hotspot is a specific call chain:
`parse -> construct -> %construct-scalar -> resolve-plain-scalar-tag ->
cl-regex-kit:full-match-p`. `full-match-p` correctly enforces a full-string
match, but the profile shows the Pike VM and match-result bookkeeping even
when only a boolean answer is needed. This is a suitable upstream
cl-regex-kit improvement, such as a boolean full-range matcher that skips
capture/result construction; replacing it locally with `is-match-p` would
change the full-match semantics and is therefore not safe.

The five stages compare different representations, on purpose:

- The reader stage parses `test.event` into event signatures and compares
  them with `equal`. A signature carries the event kind and the fields
  that matter: explicit document markers, collection flow style, anchors,
  tags, scalar style, and scalar value.
- The loader-isolated stage compares constructed values from `test.event` with
  `in.json`; the loader-e2e stage parses `in.yaml` through the public loader
  and compares the same JSON data model. Object key order and whitespace are
  ignored while null, false, integer, and float values stay distinct.
- The `emitter-isolated` stage feeds the parsed events to the emitter and
  requires `string=` equality with `emit.yaml`, the suite's libyaml-based
  emitter fixture. Whitespace, line breaks, quoting, indentation, and flow
  or block style are all part of that contract; a meaning-only comparison
  would let representation differences through.
- The `dumper-e2e` stage dumps the value represented by `in.json`, parses the
  result, and compares the resulting value with the input model. Its
  `out.yaml` file is used to decide whether the fixture applies; it is not the
  semantic comparison target.

## Mutation testing

`nix run .#mutation` runs `scripts/run-mutation.lisp` with a 3600-second
outer timeout. It sends TERM at the limit and KILL 30 seconds later. Without
Nix, use the same source registry as the test suite:

```sh
export CL_SOURCE_REGISTRY="$HOME/lisp-deps//"
export YAML_TEST_SUITE=/path/to/yaml-test-suite
mkdir -p "$HOME/.cache/cl-yaml-kit-mutation"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t \"$HOME/.cache/cl-yaml-kit-mutation/\") :inherit-configuration)"
CL_YAML_MUTATION_AREA=reader \
  timeout -k 30s 3600s sbcl --dynamic-space-size 4096 --script scripts/run-mutation.lisp
```

`CL_YAML_MUTATION_AREA` may be `reader`, `loader`, or `dumper`; omit it to
measure all three areas. Every target must select at least one test. The
script starts a fresh SBCL child for each mutant with cl-weave's
`run-isolated`. Each child loads `cl-yaml-kit/test`, evaluates the mutated
definition, and runs the selected tests under a fixed 5000 ms process timeout.
A timed-out child is reported as `timeout` with its operator and mutation path;
a child error is `errored`. Both statuses fail the mutation gate. A surviving
mutant counts as equivalent only when its function and mutation path match an
entry in the script's equivalence table. Within the selected area, an entry is
stale when its function/path is absent or its mutant is killed. A timeout or
child error leaves equivalence unresolved.

The script exits non-zero when any mutant is `survived`, `errored`, or `timeout`,
or when an entry is stale.

The Nix app exports the pinned `YAML_TEST_SUITE` fixture and sets
`ASDF_OUTPUT_TRANSLATIONS` to a writable cache shared by its children. The
manual command does the same above because each isolated child gets a fresh
`HOME`.

Each target reports its selected test count and mutation results. The final line
reports totals, the fixed per-mutant timeout, and elapsed seconds for that run.

Equivalent exclusions are listed in `scripts/run-mutation.lisp` with their
mutation paths and behavioral reasons. The script fails if an exclusion no
longer matches a surviving mutant.

## Coverage

`scripts/run-coverage.lisp` compiles `cl-yaml-kit` with SB-COVER
instrumentation in a dedicated ASDF cache, runs the whole suite, and reads
per-file expression and branch statistics with cl-weave's
`coverage-file-statistics`. Unit tests and conformance stages count toward
the same totals. It needs `YAML_TEST_SUITE` like the plain run:

```sh
export CL_SOURCE_REGISTRY="$HOME/lisp-deps//"
export YAML_TEST_SUITE=/path/to/yaml-test-suite
timeout -k 30s 1200s sbcl --dynamic-space-size 4096 --non-interactive \
  --load scripts/run-coverage.lisp
```

The artifacts default under `coverage/`: the cl-weave coverage data file and
an HTML report restricted to `src/`. `COVERAGE_OUTPUT`,
`COVERAGE_REPORT_DIRECTORY`, and `COVERAGE_ASDF_CACHE` move them.

`scripts/coverage-gate.lisp` (the `cl-yaml-kit/coverage-gate` system, tested
with the suite) turns the statistics into the gate, so `checks.coverage` in
`flake.nix`, and therefore the CI `nix flake check`, enforce it. The gate
uses an exact per-file ratchet: every measured source file's expression and
branch shortfalls must match its recorded allowance. A regression in any file
fails, and an improvement requires lowering that file's allowance.

The production exclusion list is empty. The nine candidate compile/load-time
forms did not appear as complete uncovered expressions in SB-COVER, so none
has a reproducible basis for exemption. Add a rule only when a fixture proves
SB-COVER reports that complete top-level form as unexecuted. The gate never
exempts branches or nested expressions, prints any excused and remaining
uncovered forms with their positions, and fails on an unused exclusion rule.

`scripts/coverage-allowance.sexp` records exact expression and branch
shortfalls for every measured source file. The gate requires the measured file
set and each file's counts to match the allowance exactly. When a change
closes a coverage gap, lower that file's allowance in the same change.
