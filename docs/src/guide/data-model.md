# Loader data model

The loader composes YAML events into a representation graph and then
constructs Common Lisp values from that graph. `parse` and `read-yaml` return
the first document; `parse-all` returns a list of constructed values, one per
document. See the [API reference](../reference/api.md) for the single table of
default options and resource limits.

## YAML-to-Lisp value mapping

The following table describes the constructed values for the standard YAML
tags. A quoted scalar remains a string unless a compatible explicit scalar tag
specifies another type; implicit typing applies to plain scalars according to
the selected schema.

| YAML value | Common Lisp value |
| --- | --- |
| String (`!!str`) | A Lisp string. |
| Null (`!!null`) | `+yaml-null+`; this is distinct from `nil`. |
| False (`!!bool`) | `+yaml-false+`; this is distinct from `nil`. |
| True (`!!bool`) | `t`. |
| Integer (`!!int`) | A Lisp integer. The core schema accepts decimal, `0o` octal, and `0x` hexadecimal notation, with an optional sign. |
| Floating point (`!!float`) | A double-float, including `.inf`, `-.inf`, and `.nan`, when the scalar remains a float after construction. |
| Sequence (`!!seq`) | A simple vector by default, or a proper list when `:sequence-type :list` is selected. |
| Mapping (`!!map`) | An `equal` hash table by default; see the mapping options below. |
| Alias | The aliased representation node is reused, so aliases preserve sharing. |

For example, under the default options, `[null, false, true, 42, 1.5]`
becomes a vector containing `+yaml-null+`, `+yaml-false+`, `t`, the integer
`42`, and the double-float `1.5d0`.

## Mapping and sequence representations

`:mapping-type` accepts the following values:

| Option | Result and key behavior |
| --- | --- |
| `:hash-table` (default) | An `equal` hash table. Construction signals `yaml-compose-error` for keys that are conses, non-string vectors, or `yaml-mapping` values; other constructed keys are used as hash-table keys. |
| `:alist` | A proper alist of `(key . value)` pairs in input order. Keys may be non-scalar, and duplicate detection uses `equal`. |
| `:yaml-mapping` | An ordered `yaml-mapping` whose entries are `(key . value)` pairs. Entries, including duplicate keys and non-scalar keys, are preserved in input order. |

`:sequence-type` accepts `:vector` (the default) or `:list`. Lists are proper
lists, and an empty YAML sequence becomes `nil`; an empty YAML mapping remains
an empty value of the selected mapping representation.

`:duplicate-key-policy` accepts `:error` (the default), `:first`, or `:last`.
For hash tables and alists, duplicate keys are compared with `equal`:

* `:error` signals `yaml-compose-error`.
* `:first` keeps the first value and ignores later values.
* `:last` replaces the value associated with the first occurrence.

`:yaml-mapping` preserves every entry and does not apply this duplicate-key
filtering. Consequently, use `:hash-table` or `:alist` when duplicate-key
rejection or first/last selection is required.

## Schema and implicit scalar typing

`:schema` controls resolution of unquoted (plain) scalars. It accepts
`:failsafe`, `:json`, or `:core`:

| Schema | Implicitly recognized values |
| --- | --- |
| `:failsafe` | No typed plain scalars; plain values are strings. |
| `:json` | Lowercase `null`, `true`, and `false`; JSON decimal integers and floating-point values. Leading-zero integers such as `01` remain strings. |
| `:core` (default) | YAML core null forms `~`, `null`, `Null`, `NULL`, and the empty plain scalar; case variants of `true`/`false`; signed decimal, `0o` octal, and `0x` hexadecimal integers; decimal floats, exponent notation, `.inf`/`-.inf`, and `.nan` with case variants. |

Explicit scalar tags take precedence over implicit resolution when they are
compatible. Quoted and block scalars remain strings unless an explicit scalar
tag requests another scalar type. An incompatible explicit collection or
scalar tag signals `yaml-compose-error`.

## Resource limits

The default limits and the entry points that use them are listed in the [API
reference](../reference/api.md). A limit can be changed by passing its keyword
to the relevant entry point; exceeding one signals
`yaml-resource-limit-error`. Limits are enforced at the parser or composer
stage listed in the reference. The raw-input length limit is checked before
decoding. Composer limits are checked before constructing final Lisp values.
Aliases reuse nodes but still count toward the alias-reference and node
limits when encountered.
