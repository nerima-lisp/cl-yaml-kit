# Writing YAML

`emit` represents Common Lisp values as YAML. `NIL` is represented as an empty
sequence (`[]`), while `+yaml-null+` is represented as YAML null (`null`).
The two values are therefore distinct and round-trip to different Common Lisp
values.

Supported values include strings, characters, numbers, lists, vectors,
`yaml-mapping` values, hash tables, `T`, `+yaml-null+`, and `+yaml-false+`.
An unsupported value signals `yaml-emit-error`; it is never silently converted
with `princ-to-string`.

Use `write-yaml` when the result should be written directly to a stream:

```lisp
(yaml-kit:write-yaml value stream)
```

Both functions accept `:indent`, `:default-flow-style`, and
`:explicit-document-start`. `indent` controls collection indentation and
`default-flow-style` may be `:block` or `:flow`. Set
`explicit-document-start` to true to emit `---`.

Strings that could resolve as YAML numbers, booleans, or null are quoted so
that their string type is preserved. Plain style is used only when the
scalar-analysis rules and the loader's schema resolver agree that it is safe;
otherwise the emitter selects single-quoted, double-quoted, literal, or
folded style as appropriate. Values represented as typed Lisp numbers or
sentinels use plain YAML scalars with their corresponding implicit schema type.

`NIL` means an empty sequence and is emitted as `[]`. Use `+yaml-null+` for a
YAML null value and `+yaml-false+` for YAML false. Shared or cyclic compound
objects are emitted with anchors and aliases.
