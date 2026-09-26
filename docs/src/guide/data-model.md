# Loader data model

The loader composes YAML events into a representation graph and then
constructs Common Lisp values from that graph.

By default, mappings become `equal` hash tables and sequences become simple
vectors. Use `:mapping-type :alist` or `:mapping-type :yaml-mapping` for the
other mapping representations, and `:sequence-type :list` for lists.

Mappings reject duplicate keys by default. Select `:duplicate-key-policy
:first` or `:last` to retain the first or last value. Hash-table mappings use
`equal` for key identity. Alists use the same `equal` identity for duplicate
key handling.

Scalar null and false values are represented by `+yaml-null+` and
`+yaml-false+`; true is `t`. Integers are integers and floating-point values
are double-floats, including `.inf` and `.nan`.

Non-scalar mapping keys are rejected with `yaml-compose-error`. This keeps the
default hash-table representation well-defined and avoids silently choosing a
structural equality policy for sequences and nested mappings.

Aliases preserve node sharing. Resource limits apply during composition to
bound depth, node count, scalar size, and alias references.
