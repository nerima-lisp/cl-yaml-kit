# Loader data model

The loader composes YAML events into a representation graph and then
constructs Common Lisp values from that graph. `parse` and `read-yaml` return
the first document; `parse-all` returns a list of constructed values, one per
document. The default options are `:schema :core`, `:mapping-type
:hash-table`, `:sequence-type :vector`, and `:duplicate-key-policy :error`.

## YAML-to-Lisp value mapping

The following table describes the constructed values for the standard YAML
tags. A quoted scalar is always a string; implicit typing applies to plain
scalars according to the selected schema.

| YAML value | Common Lisp value |
| --- | --- |
| String (`!!str`) | A Lisp string. |
| Null (`!!null`) | `+yaml-null+`; this is distinct from `nil`. |
| False (`!!bool`) | `+yaml-false+`; this is distinct from `nil`. |
| True (`!!bool`) | `t`. |
| Integer (`!!int`) | A Lisp integer. The core schema accepts decimal, `0o` octal, and `0x` hexadecimal notation, with an optional sign. |
| Floating point (`!!float`) | A double-float, including `.inf`, `-.inf`, and `.nan`. |
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
| `:hash-table` (default) | An `equal` hash table. Keys must be scalar Lisp values, strings, or other values accepted by this representation; conses, non-string vectors, and `yaml-mapping` values are rejected with `yaml-compose-error`. |
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

Non-scalar keys are therefore representation-dependent. They are valid in the
composed YAML graph and are accepted by `:alist` and `:yaml-mapping`, but the
`:hash-table` constructor rejects a cons, a non-string vector, or a
`yaml-mapping` key with `yaml-compose-error`. This explicit rejection avoids
silently choosing a structural equality policy for keys that an `equal` hash
table cannot represent as intended.

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

Composition enforces the following limits by default. A limit can be changed
by passing the corresponding keyword to `compose`, `compose-all`, `parse`,
`parse-all`, or `read-yaml`; exceeding one signals `yaml-resource-limit-error`.

| Keyword | Default | What is counted or bounded |
| --- | ---: | --- |
| `:max-input-length` | `104857600` | Maximum reader input length (100 MiB) for `parse`, `parse-all`, `compose`, and `compose-all`. |
| `:max-depth` | `1000` | Maximum nested sequence/mapping depth during composition. |
| `:max-scalar-length` | `16777216` | Maximum length of an individual scalar value. |
| `:max-nodes` | `1000000` | Maximum representation nodes, including nodes reached through alias events. |
| `:max-alias-expansions` | `100000` | Maximum number of alias references encountered. |

Limits are checked while composing, before the representation graph is
constructed into final Lisp values. Aliases reuse nodes but still count toward
the alias-reference and node limits when encountered.
