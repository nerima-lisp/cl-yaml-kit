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
