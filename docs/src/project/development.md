# Development

Run `nix flake check` for the repository gate. The direct Common Lisp test
entry point is `run-tests.lisp`; it loads `cl-yaml-kit/test` and runs cl-weave.
The conformance gate requires the pinned `yaml-test-suite` fixture. Enter
`nix develop` or run `nix run .#test`; both set `YAML_TEST_SUITE` to the
flake-provided fixture. The local non-Nix harness command is:

```sh
CL_YAML_TEST_TIMEOUT_MS=5000 /tmp/cl-yaml-kit-env/run-tests.sh 2400
```

The test run fails when the fixture is absent; it never silently skips
conformance cases. At the reader stage, the harness parses `test.event` into
event signatures and compares those signatures with `equal`; this is an event
meaning comparison (option (b)), not raw string matching (option (a)). The
signatures include event kinds and relevant fields such as explicit document
markers, collection flow style, anchors, tags, scalar style, and scalar value.
`in.json` is compared as a JSON/YAML data model at the loader stage, `out.yaml`
is compared semantically after dumping, and `emit.yaml` is a separate emitter
expectation. The loader reader accepts one or more consecutive JSON values in
`in.json`, because multi-document YAML cases use one JSON value per document.
JSON object key order and whitespace are ignored, while YAML null, false,
integer, and floating-point values remain distinct.

The `emitter-isolated` stage feeds the parsed `test.event` events to the
emitter and requires `string=` equality with `emit.yaml`. `emit.yaml` is the
yaml-test-suite's libyaml-based emitter fixture, so whitespace, line breaks,
quoting, indentation, and flow/block style are part of this emitter contract.
Comparing only parsed YAML meaning would allow representation differences and
would no longer test that contract. An event-to-emit-to-parse round trip may be
useful as a supplementary diagnostic, but it is not a replacement for the
independent text comparison.

Each registered test receives a 120-second default per-test timeout from
`cl-weave`; the documented command overrides it to 5000 ms with
`CL_YAML_TEST_TIMEOUT_MS=5000`. `/tmp/cl-yaml-kit-env/run-tests.sh 2400` also
sets the fixture and applies a 2400-second process-level alarm, so a deadlock
cannot hold the job indefinitely. The flake check and development shell export
`YAML_TEST_SUITE` from the pinned `data-2022-01-17` fixture input.

`scripts/run-coverage.lisp` measures the whole test suite under SB-COVER
instrumentation, so the cl-weave unit tests and the five conformance stages
count towards the same totals. There is no test-name filter, and the entry
point is the one the plain test run reaches through `asdf:test-system`. The
counters are reset after the systems are compiled and loaded, because the
instrumented load would otherwise be counted as execution. The producer
command is:

```sh
perl -e '$SIG{ALRM}=sub{kill 9,$$}; alarm 2400; exec @ARGV' sbcl --noinform --script scripts/run-coverage.lisp
```

The `perl` wrapper applies the process-level alarm in place of the `timeout`
command this shell does not provide, and 2400 seconds is enough for the full
suite. Export `YAML_TEST_SUITE` as for the plain test run, because a missing
fixture is a hard error here too. Pass `--script` on its own. On the SBCL in
this environment, adding `--non-interactive` makes the interpreter print its
banner and exit 0 without evaluating the script, which reads as a clean run
that measured nothing.

Three artifacts are written, all under `/tmp/cl-yaml-kit-coverage/` by
default: the SB-COVER data file `cl-yaml-kit.coverage`, an HTML report under
`report/` restricted to the source directory, and the tab-separated per-file
summary `per-file.tsv` whose columns are `file`, `line-covered`,
`line-total`, `branch-covered`, `branch-total`, and `uncovered-lines`. The
producer exits 0 only when the suite passed, and it writes all three artifacts
on a failing suite as well, so the per-file numbers stay readable for a tree
that is not green.

`scripts/check-coverage.pl` turns that summary into the gate:

```sh
perl -e '$SIG{ALRM}=sub{kill 9,$$}; alarm 120; exec @ARGV' perl scripts/check-coverage.pl
```

It prints a Markdown table of file, line percentage, branch percentage, and
uncovered lines, a totals row, and a final gate line, and the exit code
carries the verdict. Exit 0 means every file met both thresholds, exit 1 means
the table was printed and every offending file is named, and exit 2 means no
report could be produced, from a bad option or from a missing, empty, or
malformed summary. The interface is `--input TSV`, `--min-line PERCENT`, and
`--min-branch PERCENT`, with no positional arguments, and each setting takes
its value from the option first, then from `COVERAGE_SUMMARY`,
`CL_YAML_COVERAGE_MIN_LINE`, or `CL_YAML_COVERAGE_MIN_BRANCH`, then from the
default. Both thresholds default to 100, so any file short of full line or
branch coverage fails the gate. A file whose total for a kind is zero prints
`n/a` and is exempt, which is why the branch column of `src/data.lisp` and
`src/parser-states.lisp` reads `n/a`. `COVERAGE_OUTPUT`,
`COVERAGE_REPORT_DIRECTORY`, and `COVERAGE_SUMMARY` move the producer's three
artifacts, and each defaults under `/tmp`, while `COVERAGE_SOURCE_DIRECTORY`
defaults to `src/` and selects both the files summarised and the ones in the
HTML report.
