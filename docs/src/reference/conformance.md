# Conformance

The conformance stages use the yaml-test-suite fixture pinned by `flake.nix`.
Run them with the [test suite](../project/development/testing.md). They cover
reader events, loaded values, emitted text, and dumper round trips; they do not
establish full YAML 1.2.2 conformance.

The dumper end-to-end stage runs only when a fixture contains `out.yaml`; it
does not read that file's contents. It loads values from `in.yaml`, emits and
parses them, then compares the round trip with the loaded values. Some dumper
cases with non-scalar mapping keys are explicitly excluded because the loader
cannot construct the value model needed for round-trip comparison. The
excluded case IDs are `4fj6`, `6bfj`, `kk5p`, `lx3p`, `m5dy`, `rzp5`, `sbg9`,
`x38w`, and `xw4d`.
