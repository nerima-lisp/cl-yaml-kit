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
| large-block-mapping | reader | 10,752 | 11.355 | 3,533,696 |
| large-block-mapping | loader | 10,752 | 9.442 | 3,633,024 |
| large-block-mapping | dumper | 10,752 | 7.496 | 1,446,784 |
| large-block-sequence | reader | 6,656 | 8.308 | 2,012,117 |
| large-block-sequence | loader | 6,656 | 5.755 | 2,063,147 |
| large-block-sequence | dumper | 6,656 | 8.107 | 772,523 |
| deep-nesting | reader | 1,011,895 | 196.943 | 4,309,291 |
| deep-nesting | loader | 1,011,895 | 160.489 | 5,331,669 |
| deep-nesting | dumper | 1,011,895 | 155.900 | 22,950,101 |
| long-plain-scalar | reader | 65,543 | 26.827 | 5,614,165 |
| long-plain-scalar | loader | 65,543 | 41.560 | 5,623,339 |
| long-plain-scalar | dumper | 65,543 | 16.921 | 1,549,909 |
| long-double-quoted-scalar | reader | 65,545 | 47.899 | 1,251,285 |
| long-double-quoted-scalar | loader | 65,545 | 32.371 | 1,207,893 |
| long-double-quoted-scalar | dumper | 65,545 | 28.157 | 1,549,909 |
| long-block-literal | reader | 68,004 | 63.770 | 2,477,824 |
| long-block-literal | loader | 68,004 | 62.600 | 2,501,504 |
| long-block-literal | dumper | 68,004 | 21.383 | 1,236,800 |
| flow-collection-heavy | reader | 22,275 | 6.324 | 10,181,803 |
| flow-collection-heavy | loader | 22,275 | 0.852 | 30,774,955 |
| flow-collection-heavy | dumper | 22,275 | 1.664 | 12,983,893 |
| anchor-alias-heavy | reader | 2,222 | 7.332 | 636,288 |
| anchor-alias-heavy | loader | 2,222 | 5.219 | 717,611 |
| anchor-alias-heavy | dumper | 2,222 | 5.533 | 295,765 |
| realistic-config-1mb | reader | 1,048,765 | 8.758 | 351,318,571 |
| realistic-config-1mb | loader | 1,048,765 | 0.842 | 914,112,363 |
| realistic-config-1mb | dumper | 1,048,765 | 1.291 | 451,291,797 |

An sb-sprof run on the 1 MiB configuration identified scanner plain-scalar
work (`scan-plain-scalar`), cl-regex-kit matching during scalar construction,
and serializer traversal plus string-output allocation as the dominant paths.
No source optimization was applied: the profile did not isolate a safe local
change with a measured before/after gain, and the strongest candidates cross
the scanner, dependency, and emitter interfaces. The benchmark migration and
new corpus improve measurement coverage, but this change does not claim a
runtime improvement. Re-running on an otherwise idle machine is expected to
produce different elapsed-time and allocation values.

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

The artifacts default under `/tmp/cl-yaml-kit-coverage/`: the cl-weave
coverage data file, an HTML report restricted to `src/`, and the tab-separated
per-file summary read by the gate. `COVERAGE_OUTPUT`,
`COVERAGE_REPORT_DIRECTORY`, `COVERAGE_SUMMARY`, and `COVERAGE_ASDF_CACHE` move
the artifacts or the dedicated compilation cache. `COVERAGE_SOURCE_DIRECTORY`
(default `src/`) selects the files measured and reported.

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
