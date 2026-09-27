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
sbcl --script run-tests.lisp
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
git clone --branch v2.2.0 https://github.com/nerima-lisp/cl-regex-kit ~/lisp-deps/cl-regex-kit
git clone --branch v0.6.0 https://github.com/nerima-lisp/cl-codec-kit ~/lisp-deps/cl-codec-kit
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

Run the expanded mutation measurement with the same dependency and fixture
variables as the test suite:

```sh
export CL_SOURCE_REGISTRY='(:source-registry (:tree "/tmp/cl-yaml-kit-env/deps/") :inherit-configuration)'
export YAML_TEST_SUITE=/path/to/yaml-test-suite
export CL_YAML_DEPS=/path/to/cl-weave
CL_YAML_MUTATION_AREA=reader \
  sbcl --dynamic-space-size 4096 --non-interactive \
  --load scripts/run-mutation.lisp
```

`CL_YAML_MUTATION_AREA` may be `reader`, `loader`, or `dumper`; omit it to
measure all three areas. Every target must select at least one test. The
The script passes a fixed 5000 ms timeout to cl-weave for the complete
per-mutant callback, including evaluating the mutated definition and running
its selected tests. A timeout is reported as `errored` by cl-weave and
therefore fails the mutation gate; it is not silently counted as killed. The
script exits non-zero when any non-equivalent mutant is `survived` or
`errored`.

The verified result table is:

| area | functions | mutants | killed | survived | errored | equivalent exclusions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| reader | 12 | 50 | 50 | 0 | 0 | 0 |
| loader | 7 | 43 | 41 | 0 | 0 | 2 |
| dumper | 7 | 66 | 66 | 0 | 0 | 0 |

The two loader exclusions are documented in `scripts/run-mutation.lisp` with
their mutation paths and behavioral reasons: the anchor slot is initialized
at document start before use, and the stream-event branch return value is
ignored by the event source. They are not counted as survived mutants.

## Coverage

`scripts/run-coverage.lisp` compiles the systems with SB-COVER instrumentation
in a dedicated ASDF cache, then uses cl-weave's coverage API to run the whole
suite and save expression and branch statistics. Unit tests and conformance
stages count toward the same totals. It needs `YAML_TEST_SUITE` like the plain
run and exits non-zero unless the suite passed:

```sh
export CL_SOURCE_REGISTRY='(:source-registry (:tree "/tmp/cl-yaml-kit-env/deps/") :inherit-configuration)'
export YAML_TEST_SUITE=/path/to/yaml-test-suite
perl -e '$SIG{ALRM}=sub{kill 9,$$}; alarm 2400; exec @ARGV' \
  sbcl --dynamic-space-size 4096 --non-interactive \
  --load scripts/run-coverage.lisp
```

The artifacts default under `scripts/cl-yaml-kit-coverage/`: the cl-weave
coverage data file, an HTML report restricted to `src/`, and the tab-separated
per-file summary read by the gate. `COVERAGE_OUTPUT`,
`COVERAGE_REPORT_DIRECTORY`, `COVERAGE_SUMMARY`, and `COVERAGE_ASDF_CACHE` move
the artifacts or the dedicated compilation cache. `COVERAGE_SOURCE_DIRECTORY`
(default `src/`) selects the files measured and reported.

`scripts/check-coverage.pl` turns the summary into the gate. The coverage
runner invokes it after writing the summary, so `checks.coverage` in
`flake.nix`, and therefore the CI `nix flake check`, enforce the same gate.
The gate requires 100% expression and branch coverage after applying the
single data file `scripts/coverage-exclusions.sexp`. That file names only
form categories; the checker reads the source forms and derives their current
spans, so ordinary line movement does not require editing the exclusions.
Each category has a reason and a small reproduction under
`scripts/coverage-reproductions/`. These are SB-COVER limitations such as
compile-time package/declaration forms, `defstruct` metadata, and definition
headers. Executable function bodies are not excluded.

The checker prints a Markdown table and exits 0 when every file meets both
thresholds, 1 when a non-excluded residual remains, and 2 when no report
could be produced. The interface is `--input TSV`, `--min-line PERCENT`,
`--min-branch PERCENT`, `--source-root DIRECTORY`, and `--exclusions FILE`;
the first three settings also have the existing environment fallbacks. Both
thresholds default to 100. A file whose total for a kind is zero prints
`n/a` and is exempt from that kind.

```sh
perl scripts/check-coverage.pl \
  --input scripts/cl-yaml-kit-coverage/per-file.tsv \
  --source-root src \
  --exclusions scripts/coverage-exclusions.sexp
```
