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

Strings that could resolve as YAML numbers, booleans, or null are quoted so
that their string type is preserved. Values represented as typed Lisp numbers
or sentinels use plain YAML scalars with their corresponding implicit schema
type.
