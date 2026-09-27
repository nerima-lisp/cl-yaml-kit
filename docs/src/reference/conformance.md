# Conformance

The following values were measured with the yaml-test-suite
`data-2022-01-17` fixture using the conformance command in
[Development](../project/development.md). The pinned fixture is also used by
the repository's `nix flake check` gate.

| Stage | Passed | Applicable | Skipped | Excluded |
| --- | ---: | ---: | ---: | ---: |
| Reader | 333 | 333 | 0 | 0 |
| Loader isolated | 231 | 231 | 0 | 0 |
| Loader end-to-end | 231 | 231 | 0 | 0 |
| Emitter isolated | 31 | 31 | 0 | 0 |
| Dumper end-to-end | 204 | 213 | 0 | 9 |

These measurements are not a claim of full YAML 1.2.2 conformance. The
Dumper end-to-end stage dumps the value represented by `in.json`, parses the
result, and compares the resulting value with the input model. Its `out.yaml`
file determines whether the fixture applies; it is not the semantic comparison
target.

The nine excluded Dumper cases contain non-scalar mapping keys. The loader
reports `YAML composition; non-scalar mapping key`, so these inputs cannot be
constructed in the library's value model and cannot be used for a dumper
round-trip comparison:

- `4fj6`: non-scalar mapping key cannot be represented in the value model.
- `6bfj`: non-scalar mapping key cannot be represented in the value model.
- `kk5p`: non-scalar mapping key cannot be represented in the value model.
- `lx3p`: non-scalar mapping key cannot be represented in the value model.
- `m5dy`: non-scalar mapping key cannot be represented in the value model.
- `rzp5`: non-scalar mapping key cannot be represented in the value model.
- `sbg9`: non-scalar mapping key cannot be represented in the value model.
- `x38w`: non-scalar mapping key cannot be represented in the value model.
- `xw4d`: non-scalar mapping key cannot be represented in the value model.
