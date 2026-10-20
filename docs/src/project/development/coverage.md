# Coverage

Run `nix flake check` to run the coverage gate with the repository checks.
Without Nix, configure the source registry and `YAML_TEST_SUITE` as described
in [Development](../development.md), then run:

```sh
timeout -k 30s 1200s sbcl --dynamic-space-size 4096 --non-interactive \
  --load scripts/run-coverage.lisp
```

On macOS, replace `timeout` with `gtimeout` from GNU coreutils.

`scripts/run-coverage.lisp` compiles the library with SB-COVER instrumentation,
runs the test suite, and writes coverage data and an HTML report under
`coverage/` by default. Set these variables to change individual paths:

| Variable | Path |
| --- | --- |
| `COVERAGE_OUTPUT` | Coverage data file |
| `COVERAGE_REPORT_DIRECTORY` | HTML report directory |
| `COVERAGE_ASDF_CACHE` | ASDF cache directory for output translations |

The gate sums expression and branch coverage across all measured `src/` files.
Each aggregate must reach 90%. Per-file statistics and uncovered locations
appear in the report for diagnosis; they do not have separate thresholds.
Rules in `scripts/coverage-exclusions.sexp` name a top-level form head and a
reason. A rule excludes an expression only when its uncovered span matches
that complete top-level form; nested expressions and branches remain in the
gate. Empty or mismatched coverage data, duplicate measurements, and stale
exclusion rules fail the gate.
