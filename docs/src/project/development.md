# Development

Run `nix flake check` for the repository gate. The direct Common Lisp test
entry point is `run-tests.lisp`; it loads `cl-yaml-kit/test` and runs cl-weave.
The conformance gate requires the pinned `yaml-test-suite` fixture. Enter
`nix develop` or run `nix run .#test`; both set `YAML_TEST_SUITE` to the
flake-provided fixture. A direct invocation must set the variable explicitly:

```sh
YAML_TEST_SUITE=/path/to/yaml-test-suite \
  perl -e '$SIG{ALRM}=sub { exit 124 }; alarm 600; exec @ARGV' -- \
  sbcl --non-interactive --load run-tests.lisp
```

The test run fails when the fixture is absent; it never silently skips
conformance cases. `test.event` is compared at the reader stage, `in.json`
is compared as a JSON/YAML data model at the loader stage, `out.yaml` is
compared semantically after dumping, and `emit.yaml` is a separate emitter
expectation. JSON object key order and whitespace are ignored, while YAML
null, false, integer, and floating-point values remain distinct.

Each registered test receives a 120-second default per-test timeout from
`cl-weave`; set `CL_YAML_TEST_TIMEOUT_MS` to change it. The outer `perl`
alarm is also required for local and CI commands so a process-level deadlock
cannot hold the job indefinitely. The flake check and development shell export
`YAML_TEST_SUITE` from the pinned `data-2022-01-17` fixture input.
