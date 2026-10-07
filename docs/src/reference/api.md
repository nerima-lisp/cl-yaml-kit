# API Reference

The public package is `yaml-kit`. All symbols below are exported by
`src/package.lisp`. Optional arguments shown with a default use the default
implemented by the source.

## Reading and composing

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `parse input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First constructed document | `yaml-parse-error`, `yaml-compose-error`, `yaml-resource-limit-error` |
| `parse-all input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of constructed documents | Same as `parse` |
| `read-yaml input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | One constructed document | Same as `parse` |
| `parse-events input &rest keys` | Event list | `yaml-parse-error`, `yaml-resource-limit-error` |
| `map-events handler input &key max-input-length max-depth max-scalar-length` | `nil` | `yaml-parse-error`, `yaml-resource-limit-error` |
| `compose-events events &rest keys` | First representation graph | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose-all-events events &key max-depth max-scalar-length max-nodes max-alias-expansions` | Representation graphs | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose input &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First representation node | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose-all input &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of representation nodes | Same as `compose` |

The exported composition entry points are `compose`, `compose-all`,
`compose-events`, and `compose-all-events`.

`input` may be a string, octet vector accepted by the reader, or stream where
the implementation permits it. Octet vectors use cl-codec-kit's `:auto`
encoding detection, including BOM-less UTF-16 and UTF-32 null-pattern
detection. Invalid octet sequences are reported as `yaml-parse-error`. The
defaults are centralized here:

| Option or limit | Default | Applies to |
| --- | ---: | --- |
| `:schema` | `:core` | `parse`, `parse-all`, `read-yaml` |
| `:mapping-type` | `:hash-table` | `parse`, `parse-all`, `read-yaml` |
| `:sequence-type` | `:vector` | `parse`, `parse-all`, `read-yaml` |
| `:duplicate-key-policy` | `:error` | `parse`, `parse-all`, `read-yaml` |
| `:max-input-length` | `104857600` | loaders and event parsing |
| `:max-depth` | `1000` | loaders and composers |
| `:max-scalar-length` | `16777216` | loaders, composers, and event parsing |
| `:max-nodes` | `1000000` | loaders and composers |
| `:max-alias-expansions` | `100000` | loaders and composers |
| event-parser `:max-depth` | `1000` | `map-events`, `parse-events` |
| `:indent` | `2` | `emit`, `write-yaml`, `emit-events` |
| `:default-flow-style` | `:block` | `emit`, `write-yaml` |
| `:explicit-document-start` | `nil` | `emit`, `write-yaml`, `emit-events` |

`read-yaml` accepts the same string, octet-vector, and stream inputs as the
other reader entry points; its first argument is not restricted to streams.
The limit values are upper bounds. Exceeding one signals
`yaml-resource-limit-error`. For strings and character streams,
`:max-input-length` counts characters. For octet vectors, it counts bytes and
is checked before decoding. Character streams are read only until the first
character beyond the limit, so a non-terminating stream cannot bypass the
bound.

Event sources passed to `compose-events` and `compose-all-events` must emit a
complete stream with ordered stream and document start/end events. Those APIs
do not accept `:max-input-length`; raw input limits are enforced by the reader
entry points. Re-defining an anchor is valid YAML and updates the anchor table,
so a later alias refers to the most recent definition.

The loader is supported on SBCL only. Constructed YAML numeric scalars are
either Common Lisp integers or `double-float` values. Ratios and complex
numbers are not YAML scalar result types; for example, `1/2` remains a string.
Floating-point special values are implemented in an isolated SBCL-specific
source file.

## Emitting

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `emit value &key indent default-flow-style explicit-document-start` | YAML string | `yaml-emit-error` |
| `write-yaml value stream &key indent default-flow-style explicit-document-start` | `value` | `yaml-emit-error` |
| `emit-events events &key indent explicit-document-start` | YAML string | `yaml-emit-error` |

The defaults for these options are in the table above.

## Format-preserving editing

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `edit-source source path value &key document operation` | Edited source in the input type | `yaml-parse-error`, `yaml-format-edit-path-error`, `yaml-format-edit-anchor-error`, `yaml-format-edit-structure-error` |
| `yaml-format-edit-path-error-path`, `yaml-format-edit-path-error-document`, `yaml-format-edit-path-error-operation` | `(condition)` | Path error details |
| `yaml-format-edit-structure-error-path` | `(condition)` | Structure error path |
| `yaml-format-edit-anchor-error-path`, `yaml-format-edit-anchor-error-anchor` | `(condition)` | Anchor error details |

`path` is a proper list of string keys and zero-based integer sequence indexes.
`document` is zero-based. `operation` is `:set` by default; use `:delete` to
remove a mapping entry or sequence item. The editor parses both the original and
edited source with `parse-all`, then verifies that the requested semantic change
is the only value change before returning it. A syntactically valid edit that
changes an alias-resolved value or any other unrelated value signals
`yaml-format-edit-structure-error`.

The supported `:set` form replaces an untagged, non-empty, single-line scalar.
The replacement value must also be scalar. Scalar replacement in a flow
collection quotes strings containing flow indicators when necessary. `:set`
does not replace mappings, sequences, empty values, block scalars, or multiline
scalars; those cases signal `yaml-format-edit-structure-error`.

The supported additions are mapping entries and sequence items in block
collections. Mapping additions require a string key. Sequence additions are
append-only: the path index must be the next index. Additions to flow
collections, additions to mappings or sequences beginning after `- ` on the
same line, and non-tail sequence additions signal
`yaml-format-edit-structure-error`. The editor also rejects deletion from flow
collections, deletion of a collection's only child, and deletion of inline
sequence mappings or sequences.

Inserted scalar fragments are emitted in flow style. If an emitter fragment
would span multiple lines, the editor uses a double-quoted fragment with YAML
escapes. This applies to inserted keys and values whose content contains a
document marker, comment indicator, sequence indicator, or line break.

For safety, edits through tagged nodes, anchors, aliases, duplicate mapping
keys, or merge-key mappings are rejected. The document root cannot be deleted.
Integer path components under mappings are rejected with
`yaml-format-edit-path-error`. Unsupported source encodings and edited text
that does not parse signal a `yaml-kit-error` subtype. Within the supported
subset, comments, key order, source text outside the edit, line endings, final
newline state, and the input octet encoding including a byte-order mark are
preserved. Replacing a quoted scalar does not promise to retain its original
quote style; the replacement uses the emitter's selected safe style.

## Sentinel and mapping values

| Symbol | Lambda list or value | Returns |
| --- | --- | --- |
| `+yaml-null+`, `+yaml-false+` | constants | Opaque null and false markers |
| `yaml-null-p`, `yaml-false-p` | `(value)` | Boolean identity test |
| `make-yaml-mapping` | `(&optional entries)` | New ordered mapping |
| `yaml-mapping-p` | `(object)` | Boolean |
| `yaml-mapping-entries` | `(mapping)` | Mapping entry alist |

## Marks, tokens, and scanners

| Symbol | Lambda list or access | Result |
| --- | --- | --- |
| `mark` | structure type | Mark structure |
| `make-mark` | `(line column offset)` | Mark |
| `mark-line`, `mark-column`, `mark-offset` | `(mark)` | Nonnegative integer |
| `token` | structure type | Token structure |
| `make-token` | `(kind start-mark end-mark &key value handle suffix style major minor)` | Validated token |
| `token-kind`, `token-start-mark`, `token-end-mark`, `token-value`, `token-handle`, `token-suffix`, `token-style`, `token-major`, `token-minor` | `(token)` | Corresponding token field |
| `make-scanner` | `(simple-array character (*))` | Scanner |
| `scanner-peek-token`, `scanner-next-token` | `(scanner)` | Next token or `nil` |

## Nodes

`make-scalar-node` accepts the base node keywords `:tag :anchor :style
:start-mark :end-mark` plus `:value`. `make-sequence-node` additionally accepts
`:items`; `make-mapping-node` additionally accepts `:pairs`. The predicates
`scalar-node-p`, `sequence-node-p`, and `mapping-node-p` accept one object and
return a boolean. `node-tag`, `node-anchor`, `node-style`, `node-start-mark`,
`node-end-mark`, `scalar-node-value`, `sequence-node-items`, and
`mapping-node-pairs` accept the corresponding node and return its field.

`scalar-node`, `sequence-node`, and `mapping-node` are the structure types.
## Events

The constructors all accept the base keywords `:start-mark :end-mark`.
`make-document-start-event` adds `:explicit-p :version :tag-directives`;
`make-document-end-event` adds `:explicit-p`; sequence and mapping start
constructors add `:anchor :tag :implicit-p :style`; the scalar constructor
adds `:anchor :tag :value :plain-implicit-p :quoted-implicit-p :style`; and
`make-alias-event` adds `:anchor`.

The exported event constructors are `make-stream-start-event`,
`make-stream-end-event`, `make-document-start-event`,
`make-document-end-event`, `make-sequence-start-event`,
`make-sequence-end-event`, `make-mapping-start-event`,
`make-mapping-end-event`, `make-scalar-event`, and `make-alias-event`.

The event types are `stream-start-event`, `stream-end-event`,
`document-start-event`, `document-end-event`, `sequence-start-event`,
`sequence-end-event`, `mapping-start-event`, `mapping-end-event`,
`scalar-event`, and `alias-event`. Their `*-p` predicates accept one object.
`event-start-mark` and `event-end-mark` accept any event. The remaining event
readers accept their named event and return the corresponding constructor
field: `document-start-event-explicit-p`, `document-start-event-version`,
`document-start-event-tag-directives`, `document-end-event-explicit-p`,
`sequence-start-event-anchor`, `sequence-start-event-tag`,
`sequence-start-event-implicit-p`, `sequence-start-event-style`,
`mapping-start-event-anchor`, `mapping-start-event-tag`,
`mapping-start-event-implicit-p`, `mapping-start-event-style`,
`scalar-event-anchor`, `scalar-event-tag`, `scalar-event-value`,
`scalar-event-plain-implicit-p`, `scalar-event-quoted-implicit-p`,
`scalar-event-style`, and `alias-event-anchor`.

## Conditions

`yaml-kit-error` is the base condition. `yaml-parse-error`,
`yaml-compose-error`, `yaml-emit-error`, and `yaml-resource-limit-error` are
its exported subtypes. Their readers and defaults are specified in
[Conditions](conditions.md).

The exported event predicates are `stream-start-event-p`,
`stream-end-event-p`, `document-start-event-p`, `document-end-event-p`,
`sequence-start-event-p`, `sequence-end-event-p`, `mapping-start-event-p`,
`mapping-end-event-p`, `scalar-event-p`, and `alias-event-p`.

The exported condition readers are `yaml-parse-error-line`,
`yaml-parse-error-column`, `yaml-parse-error-offset`,
`yaml-parse-error-context`, `yaml-compose-error-mark`,
`yaml-compose-error-context`, `yaml-compose-error-cause`,
`yaml-emit-error-mark`, `yaml-emit-error-context`, and
`yaml-emit-error-cause`.
