# API Reference

The public package is `yaml-kit`. All symbols below are exported by
`src/package.lisp`. Optional arguments shown with a default use the default
implemented by the source.

Each `/k` entry requires a function-valued `k` and signals `type-error` before
processing if `k` is not a function. It returns only the primary value returned
by `k`.

## Reading and composing

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `parse input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First constructed document | `yaml-parse-error`, `yaml-compose-error`, `yaml-resource-limit-error` |
| `parse/k input k &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the first constructed document | Same as `parse` |
| `parse-all input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of constructed documents | Same as `parse` |
| `parse-all/k input k &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the document list | Same as `parse-all` |
| `read-yaml input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First constructed document | Same as `parse` |
| `read-yaml/k stream k &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the first constructed document | Same as `read-yaml` |
| `parse-events input &rest keys` | Event list; forwards `:max-input-length`, `:max-depth`, and `:max-scalar-length` to `map-events` | `yaml-parse-error`, `yaml-resource-limit-error` |
| `parse-events/k input k &key max-input-length max-depth max-scalar-length` | Primary value returned by `k`; calls `k` once with the event list | Same as `parse-events` |
| `map-events handler input &key max-input-length max-depth max-scalar-length` | `nil`; calls `handler` with each event in source order | `yaml-parse-error`, `yaml-resource-limit-error` |
| `map-events/k handler input k &key max-input-length max-depth max-scalar-length` | Primary value returned by `k`; calls `k` once with `nil` after all events | Same as `map-events` |
| `comment-spans input &key max-input-length` | Ordered list of `(start . end)` comment spans | `yaml-parse-error`, `yaml-resource-limit-error` |
| `comment-spans/k input k &key max-input-length` | Primary value returned by `k`; calls `k` once with the comment-span list | Same as `comment-spans` |
| `compose-events events &rest keys` | First representation node (root of the graph); `events` is a function that accepts a handler and sends each event to it | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose-events/k event-source k &key max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the first node or `nil` | Same as `compose-events` |
| `compose-all-events events &key max-depth max-scalar-length max-nodes max-alias-expansions first-only document-handler` | Varies with `:first-only` and `:document-handler`; see below. `events` is a function that accepts a handler and sends each event to it | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose-all-events/k event-source k &key max-depth max-scalar-length max-nodes max-alias-expansions first-only document-handler` | Primary value returned by `k`; calls `k` once with the result described for `compose-all-events` | Same as `compose-all-events` |
| `compose input &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First representation node (root of the graph) | `yaml-parse-error`, `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose/k input k &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the first node or `nil` | Same as `compose` |
| `compose-all input &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of representation nodes, one root per document | Same as `compose` |
| `compose-all/k input k &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the node list | Same as `compose-all` |

`parse`, `read-yaml`, and `compose` operate on only the first document in a raw
input stream. They stop parsing or composing after that document; use
`parse-all` or `compose-all` to process every document in the stream.

An empty raw YAML input has no document. `compose`, `compose-all`, and
`parse-all` return `nil`; `parse` and `read-yaml` signal
`yaml-compose-error` because there is no node to construct.

`compose-all-events` returns a list of roots by default, or a one-element list
with the first root when `:first-only` is true. Its `:document-handler` receives
one root representation node for each completed document, and its return value
is ignored. With `:document-handler` and no `:first-only`, the composer calls
the handler for every document and returns `nil`. With both options, it calls
the handler only for the first document and returns a one-element list, or
`nil` if no document exists.

`parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `map-events`,
`parse-events`, and `comment-spans` accept strings, octet vectors, and character
streams. Octet vectors use
cl-codec-kit's `:auto` encoding detection, including BOM-less UTF-16 and UTF-32
null-pattern detection. Invalid octet sequences are reported as
`yaml-parse-error`.

| Option or limit | Default | Applies to |
| --- | ---: | --- |
| `:schema` | `:core` | `parse`, `parse-all`, `read-yaml` |
| `:mapping-type` | `:hash-table` | `parse`, `parse-all`, `read-yaml` |
| `:sequence-type` | `:vector` | `parse`, `parse-all`, `read-yaml` |
| `:duplicate-key-policy` | `:error` | `parse`, `parse-all`, `read-yaml` |
| `:max-input-length` | `104857600` | `parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `map-events`, `parse-events`, `comment-spans` |
| `:max-depth` | `1000` | `parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `compose-events`, `compose-all-events`, `map-events`, `parse-events` |
| `:max-scalar-length` | `16777216` | `parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `compose-events`, `compose-all-events`, `map-events`, `parse-events` |
| `:max-nodes` | `1000000` | `parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `compose-events`, `compose-all-events` |
| `:max-alias-expansions` | `100000` | `parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `compose-events`, `compose-all-events` |
| `:first-only` | `nil` | `compose-all-events` |
| `:document-handler` | `nil` | `compose-all-events` |
| `:indent` | `2` | `emit`, `write-yaml`, `emit-events` |
| `:default-flow-style` | `:block` | `emit`, `write-yaml` |
| `:explicit-document-start` | `nil` | `emit`, `write-yaml`, `emit-events` |

The accepted values and effects of `:schema`, `:mapping-type`,
`:sequence-type`, and `:duplicate-key-policy` are described in the
[data model guide](../guide/data-model.md). `:default-flow-style` accepts
`:block` or `:flow`; see [Writing YAML](../guide/writing.md).

`map-events` and `parse-events` apply `:max-depth` while parsing events;
`compose-events` and `compose-all-events` apply it while building representation
graphs. The raw-input functions `parse`, `parse-all`, `read-yaml`, `compose`,
and `compose-all` pass the same `:max-depth` value to both stages.

The limit values are upper bounds. Exceeding one signals
`yaml-resource-limit-error`. For strings and character streams,
`:max-input-length` counts characters. For octet vectors, it counts bytes and
is checked before decoding. Character streams are read only until the first
character beyond the limit, so a non-terminating stream cannot bypass the
bound.

`comment-spans` returns one span for each comment recognized by the YAML lexer,
in source order. Offsets are zero-based character positions in the decoded
input. The start is inclusive and the end is exclusive. A span includes `#`
and the comment text but excludes the following line break. A `#` inside a
quoted scalar, block scalar content, or plain scalar text without preceding
separation (for example, `foo#bar`) is not reported as a comment.

### Event composition

An event source is a function that receives a handler and sends events to it;
the source's return value is ignored.
A complete event stream starts with `stream-start-event`, contains zero or
more documents, and ends with `stream-end-event`. Each document contains
`document-start-event`, one root node, and
`document-end-event`; collection roots include matching start and end events.
To pass the list returned by `parse-events` to `compose-events`, wrap it in a
function that calls the supplied handler for each event:

```lisp
(let ((events (yaml-kit:parse-events "key: value")))
  (yaml-kit:compose-events
   (lambda (handler)
     (dolist (event events)
       (funcall handler event)))))
```

`compose-events` always stops after the first `document-end-event`.
`compose-all-events` stops there when `:first-only` is true. In either case,
the source can return without sending `stream-end-event`. With `:first-only`
false, `compose-all-events` consumes the complete stream and requires
`stream-end-event`. A non-empty source that returns before sending that event
signals `yaml-compose-error`. A source that sends no events produces no
document and composition returns `nil`.

These composition APIs do not accept `:max-input-length`; raw-input limits are
enforced by entry points such as `parse-events`. Re-defining an anchor is valid
YAML and updates the anchor table, so a later alias refers to the most recent
definition.

## Runtime and scalar values

This library is supported on SBCL only. Constructed YAML numeric scalars are
either Common Lisp integers or `double-float` values. Ratios and complex
numbers are not YAML scalar result types; for example, `1/2` remains a string.

## Emitting

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `emit value &key indent default-flow-style explicit-document-start` | YAML string | `yaml-emit-error` |
| `emit/k value k &key indent default-flow-style explicit-document-start` | Primary value returned by `k`; calls `k` once with the YAML string | Same as `emit` |
| `write-yaml value stream &key indent default-flow-style explicit-document-start` | `value` | `yaml-emit-error` |
| `write-yaml/k value stream k &key indent default-flow-style explicit-document-start` | Primary value returned by `k`; calls `k` once with the written value | Same as `write-yaml` |
| `emit-events events &key indent explicit-document-start` | YAML string; accepts an event list | `yaml-emit-error` |
| `emit-events/k events k &key indent explicit-document-start` | Primary value returned by `k`; calls `k` once with the YAML string | Same as `emit-events` |

## Format-preserving editing

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `edit-source source path value &key (document 0) (operation :set)` | Edited source in the input type | `yaml-parse-error`, `yaml-emit-error`, `yaml-format-edit-path-error`, `yaml-format-edit-anchor-error`, `yaml-format-edit-structure-error` |
| `edit-source/k source path value &key (document 0) (operation :set) (k nil)` | Primary value returned by `k`; calls `k` once with the edited source | Same as `edit-source`, plus `type-error` if `k` is omitted or not a function |

Readers for the edit conditions are listed in the [conditions reference](conditions.md).

`path` is a non-empty proper list of string keys and zero-based integer sequence
indexes, ordered from the document root toward the selected node. For example,
`("user" "name")` selects `name` under the top-level `user` key.
`document` is zero-based. `operation` is `:set` by default; use `:delete` to
remove a mapping entry or sequence item. With `:set`, a path whose final
component is absent adds a mapping entry when that component is a string, or
appends a sequence item when the component is the next zero-based index.
All earlier path components must exist; missing intermediate components are
not created and signal `yaml-format-edit-structure-error`.
`operation :delete` requires the path to exist. The positional `value` is still
required for `:delete`, but is ignored. The editor parses both the original and
edited source with `parse-all`, then verifies that the requested semantic
change is the only value change before returning it. A syntactically valid edit
that changes an alias-resolved value or any other unrelated value signals
`yaml-format-edit-structure-error`.

`source` must be a string or an octet vector; streams are not accepted. Other
input types signal `yaml-format-edit-path-error`. Octet vectors are decoded
using the detected source encoding and returned in that encoding after editing.

When the path exists, `:set` replaces an untagged scalar written on one line in
plain, single-quoted, or double-quoted style. Quoted empty scalars such as `''`
and `""` are supported. Implicit empty values such as `key:` or `-`, block
scalars, and multiline scalars signal `yaml-format-edit-structure-error`.
Replacement values may be strings, characters, integers, any Common Lisp float
(`short-float`, `single-float`, `double-float`, or `long-float`), `t`,
`+yaml-null+`, or `+yaml-false+`. `nil`, mappings, and sequences are not scalar
replacement values. `:set` does not replace mappings or sequences.

Additions are supported for mapping entries and sequence items in block
collections. Mapping additions require a string key. Sequence additions are
append-only: the path index must be the next index. Additions to flow
collections, additions to collections whose opening appears on the same line
as a sequence entry (for example, the mapping in `- key: value` or nested
sequence in `- - item`), and non-tail sequence additions signal
`yaml-format-edit-structure-error`. The editor also rejects deletion from flow
collections, deletion of a collection's only child, and deletion from mappings
or sequences whose opening appears on the same line as a sequence entry, such
as the mapping in `- key: value` or nested sequence in `- - item`.

Added mapping keys and values and sequence items are serialized by the emitter
with `:default-flow-style :flow`; their surrounding block collection keeps its
block layout. Added values use the scalar types listed for `:set`; collection
values signal `yaml-format-edit-structure-error`. A string value containing a line break is
written as a double-quoted scalar with YAML escapes. Replacement strings in
flow collections are single-quoted when their content requires protection from
flow indicators.

Each path component's selected value must be untagged and cannot be
anchored or an alias. This applies to intermediate values used to reach a
nested value and to the final value being edited. Mapping keys with an anchor
or alias are also rejected. Tagged-node edits signal
`yaml-format-edit-structure-error`; edits involving anchors or aliases signal
`yaml-format-edit-anchor-error`.
Traversed mappings with duplicate keys or merge keys are also rejected. The
document root cannot be replaced or deleted.
Integer path components under mappings are rejected with
`yaml-format-edit-path-error`. Unsupported source encodings signal
`yaml-parse-error`; edited text that does not parse signals
`yaml-format-edit-structure-error`. Within the supported subset, comments, key
order, source text outside the edit, line endings, final newline state, and the
input octet encoding including a byte-order mark are preserved. Replacing a
quoted scalar does not promise to retain its original quote style; the
replacement uses the emitter's selected safe style.

## Sentinel and mapping values

| Symbol | Lambda list or value | Returns |
| --- | --- | --- |
| `+yaml-null+`, `+yaml-false+` | constants | Opaque null and false markers |
| `yaml-null-p`, `yaml-false-p` | `(value)` | Boolean identity test |
| `make-yaml-mapping` | `(&optional entries)` | New ordered mapping |
| `yaml-mapping-p` | `(object)` | Boolean |
| `yaml-mapping-entries` | `(mapping)` | Mapping entry alist |

Pass `entries` as an ordered association list of key/value conses. The
constructor shallow-copies the outer list and preserves the supplied entry
conses and their order.

## Tokens

| Symbol | Lambda list or access | Result |
| --- | --- | --- |
| `token` | structure type | Token structure |
| `make-token` | `(kind start-span end-span &key value handle suffix style major minor)` | Validated token |
| `token-kind`, `token-start-span`, `token-end-span`, `token-value`, `token-handle`, `token-suffix`, `token-style`, `token-major`, `token-minor` | `(token)` | Corresponding token field |

Token kinds are keywords: `:stream-start`, `:stream-end`,
`:version-directive`, `:tag-directive`, `:document-start`, `:document-end`,
`:block-sequence-start`, `:block-mapping-start`, `:block-end`,
`:flow-sequence-start`, `:flow-sequence-end`, `:flow-mapping-start`,
`:flow-mapping-end`, `:block-entry`, `:flow-entry`, `:key`, `:value`,
`:scalar`, `:anchor`, `:tag`, and `:alias`. `token-value` contains scalar
text, an anchor or alias name, or a tag-directive prefix, as applicable.
`token-handle` and `token-suffix` carry tag data; `token-style` carries a
scalar style; `token-major` and `token-minor` carry a YAML version. The start
and end fields hold `cl-parser-kit:span` values or `nil`. Fields not used by a
token kind are `nil`.

## Nodes

`make-scalar-node` accepts the base node keywords `:tag :anchor :style
:start-span :end-span` plus `:value`. `make-sequence-node` additionally accepts
`:items`; `make-mapping-node` additionally accepts `:pairs`. The predicates
`scalar-node-p`, `sequence-node-p`, and `mapping-node-p` accept one object and
return a boolean. `node-tag`, `node-anchor`, `node-style`, `node-start-span`,
`node-end-span`, `scalar-node-value`, `sequence-node-items`, and
`mapping-node-pairs` accept the corresponding node and return its field.

`scalar-node`, `sequence-node`, and `mapping-node` are the structure types.
`node-tag` and `node-anchor` return their tag or anchor strings, or `nil`.
`mapping-node-pairs` returns an ordered list of `(key . value)` pairs; repeated
keys remain present.

## Events

The constructors all accept the base keywords `:start-span :end-span`.
`make-document-start-event` adds `:explicit-p :version :tag-directives`;
`make-document-end-event` adds `:explicit-p`; sequence and mapping start
constructors add `:anchor :tag :implicit-p :style`; the scalar constructor
adds `:anchor :tag :value :plain-implicit-p :quoted-implicit-p :style`; and
`make-alias-event` adds `:anchor`.

Collection style is `:block` or `:flow`; `implicit-p` reports whether a
collection tag can be omitted. Scalar style is `:plain`, `:single-quoted`,
`:double-quoted`, `:literal`, or `:folded`. `plain-implicit-p` and
`quoted-implicit-p` report whether a scalar tag can be omitted with the
corresponding style.

The exported event constructors are `make-stream-start-event`,
`make-stream-end-event`, `make-document-start-event`,
`make-document-end-event`, `make-sequence-start-event`,
`make-sequence-end-event`, `make-mapping-start-event`,
`make-mapping-end-event`, `make-scalar-event`, and `make-alias-event`.

The event types are `stream-start-event`, `stream-end-event`,
`document-start-event`, `document-end-event`, `sequence-start-event`,
`sequence-end-event`, `mapping-start-event`, `mapping-end-event`,
`scalar-event`, and `alias-event`.
The exported event predicates are `stream-start-event-p`, `stream-end-event-p`,
`document-start-event-p`, `document-end-event-p`, `sequence-start-event-p`,
`sequence-end-event-p`, `mapping-start-event-p`, `mapping-end-event-p`,
`scalar-event-p`, and `alias-event-p`.
`event-start-span` and `event-end-span` accept any event. The remaining event
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

`yaml-kit-error` is the base condition and stores a `cl-parser-kit:diagnostic`,
available through `yaml-kit-error-diagnostic`. Its report is formatted by
`cl-parser-kit:diagnostic->string`. The `cl-parser-kit` type and readers are
not re-exported by `yaml-kit`; use `cl-parser-kit:diagnostic-message` and
`cl-parser-kit:diagnostic-span` to read its message and span. `yaml-parse-error`,
`yaml-compose-error`, `yaml-emit-error`, `yaml-resource-limit-error`,
`yaml-format-edit-path-error`, `yaml-format-edit-structure-error`, and
`yaml-format-edit-anchor-error` are its exported subtypes. Their readers and
defaults are specified in [Conditions](conditions.md). Exported readers are
`yaml-kit-error-diagnostic`, `yaml-parse-error-context`,
`yaml-compose-error-context`, `yaml-compose-error-cause`,
`yaml-emit-error-context`, `yaml-emit-error-cause`,
`yaml-format-edit-path-error-path`, `yaml-format-edit-path-error-document`,
`yaml-format-edit-path-error-operation`,
`yaml-format-edit-structure-error-path`,
`yaml-format-edit-anchor-error-path`, and `yaml-format-edit-anchor-error-anchor`.

