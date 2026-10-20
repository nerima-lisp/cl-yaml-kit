# Writing YAML

`emit` represents Common Lisp values as YAML. `NIL` becomes an empty sequence
(`[]`), while `+yaml-null+` becomes YAML null (`null`). The default loader
reads `[]` as an empty vector, not `NIL`.

Supported values include strings, characters, integers, floats of any Common
Lisp float format, lists, vectors, `yaml-mapping` values, hash tables, `T`,
`+yaml-null+`, and `+yaml-false+`. Ratios and complex numbers are unsupported
values.
An unsupported value signals `yaml-emit-error`; it is never silently converted
with `princ-to-string`.

Use `write-yaml` when writing directly to a stream:

```lisp
(yaml-kit:write-yaml '("example" 1.5d0) *standard-output*)
```

This writes:

```yaml
- example
- 1.5
```

Both functions accept `:indent`, `:default-flow-style`, and
`:explicit-document-start`. `:indent` is the number of spaces used for
collection indentation; use a nonnegative integer. `:default-flow-style` may be
`:block` or `:flow`. Set `:explicit-document-start` to true to emit `---`.

`emit-events` writes YAML from an event list. It accepts `:indent` and
`:explicit-document-start` and returns the resulting string.

Strings that the core schema would resolve as YAML numbers, booleans, or null
are quoted so that their string type is preserved. Plain style is used only
when the scalar-analysis rules and core-schema resolution agree that it is safe;
otherwise the emitter selects single-quoted, double-quoted, literal, or
folded style as appropriate. Values represented as typed Lisp numbers or
sentinels use plain YAML scalars with their corresponding implicit schema type.

Use `+yaml-false+` for YAML false. Shared or cyclic compound objects are
emitted with anchors and aliases.
