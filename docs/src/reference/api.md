# API Reference

`yaml-kit` is the public package. Its library symbols listed here are exported
as declared in `src/package.lisp`. Names qualified with `cl-parser-kit:` remain
in that dependency's package and are not re-exported by `yaml-kit`. Optional
arguments shown with a default use the default implemented by the source.

Each `/k` entry requires a function-valued `k` and signals `type-error` before
processing if `k` is not a function. Each row states where `k` is passed and
what value it receives.

## Reading and composing

An event source is a function that receives a handler and sends events to it.
The non-`/k` signatures call this function `events`; the `/k` signatures call it
`event-source`.

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `parse input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First constructed document | `yaml-parse-error`, `yaml-compose-error`, `yaml-resource-limit-error` |
| `parse/k input k &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the first constructed document | Same as `parse` |
| `parse-all input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of constructed documents | Same as `parse` |
| `parse-all/k input k &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the document list | Same as `parse-all` |
| `read-yaml stream &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First constructed document | Same as `parse` |
| `read-yaml/k stream k &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | Primary value returned by `k`; calls `k` once with the first constructed document | Same as `read-yaml` |
| `parse-events input &rest keys` | Event list; forwards `:max-input-length`, `:max-depth`, and `:max-scalar-length` to `map-events` | `yaml-parse-error`, `yaml-resource-limit-error` |
| `parse-events/k input k &key max-input-length max-depth max-scalar-length` | Primary value returned by `k`; calls `k` once with the event list | Same as `parse-events` |
| `map-events handler input &key max-input-length max-depth max-scalar-length` | `nil`; calls a function-valued `handler` with each event in source order and ignores its return values | `yaml-parse-error`, `yaml-resource-limit-error` |
| `map-events/k handler input k &key max-input-length max-depth max-scalar-length` | Primary value returned by `k`; calls `k` once with `nil` after all events | Same as `map-events` |
| `comment-spans input &key max-input-length` | Ordered list of `(start . end)` comment spans | `yaml-parse-error`, `yaml-resource-limit-error` |
| `comment-spans/k input k &key max-input-length` | Primary value returned by `k`; calls `k` once with the comment-span list | Same as `comment-spans` |
| `compose-events events &rest options` | First representation node (root of the graph); `events` is a function that accepts a handler and sends each event to it | `yaml-compose-error`, `yaml-resource-limit-error` |
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

An empty, whitespace-only, or comment-only raw YAML input has no document.
`compose`, `compose-all`, and
`parse-all` return `nil`; `parse` and `read-yaml` signal
`yaml-compose-error` because there is no node to construct.

`parse` and `read-yaml` return constructed Common Lisp values. `compose` returns
a representation graph, whose nodes retain tags, anchors, styles, source spans,
and ordered mapping pairs, including duplicate keys. See [API Structures](api-structures.md).

`compose-all-events` returns:

| `:document-handler` | `:first-only` | Return value | Handler calls |
| --- | --- | --- | --- |
| `nil` | `nil` | List of all roots | None |
| `nil` | true | One-element list with the first root, or `nil` if there is no document | None |
| Function | `nil` | `nil` | Once for each completed document |
| Function | true | One-element list with the first root, or `nil` if there is no document | Once for the first document only |

The handler's return value is ignored. `compose-events` also accepts
`:document-handler`, calls it with the first root if one exists, and returns
that root or `nil` if there is no document.

`parse`, `parse-all`, `read-yaml`, `compose`, `compose-all`, `map-events`,
`parse-events`, and `comment-spans` accept strings, octet vectors, and character
streams. Their `/k` variants accept the same input types. Although
`read-yaml` names its argument `stream`, it accepts all three input types.
Octet vectors use cl-codec-kit's `:auto` encoding detection, including
BOM-less UTF-16 and UTF-32 null-pattern detection. Invalid octet sequences are
reported as `yaml-parse-error`.

Rows that use `&rest keys` take the applicable keyword options listed below.

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
| `:document-handler` | `nil` | `compose-events`, `compose-all-events`, `compose-all-events/k` |
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

`compose-events`, `compose-all-events`, and their `/k` variants accept `nil` for
`:max-depth`, `:max-scalar-length`, `:max-nodes`, and `:max-alias-expansions` to
disable those limits. The limit values are upper bounds. Exceeding one signals
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

The event source's return value is ignored.
A complete event stream starts with `stream-start-event`, contains zero or
more documents, and ends with `stream-end-event`. Each document contains
`document-start-event`, one root node, and
`document-end-event`; collection roots include matching start and end events.
To pass the list returned by `parse-events` to `compose-events`, wrap it in a
function that calls the supplied handler for each event:

    (let ((events (yaml-kit:parse-events "key: value")))
      (yaml-kit:compose-events
       (lambda (handler)
         (dolist (event events)
           (funcall handler event)))))

`compose-events` always stops after the first `document-end-event`.
`compose-all-events` stops there when `:first-only` is true. In either case,
the source can return without sending the remaining stream events, including
`stream-end-event`. With `:first-only` false, `compose-all-events` consumes the
complete stream and requires `stream-end-event`. A non-empty source that
returns before sending that event signals `yaml-compose-error`. An empty source
produces no document and composition returns `nil`.

These composition APIs do not accept `:max-input-length`; raw-input limits are
enforced by entry points such as `parse-events`. Within a document,
re-defining an anchor is valid YAML and updates that document's anchor table.
A later alias in the same document refers to the most recent definition.

## Runtime and scalar values

This library is supported on SBCL only. Constructed YAML numeric scalars are
either Common Lisp integers or `double-float` values. Ratios and complex
numbers are not YAML scalar result types; for example, `1/2` remains a string.

## Emitting

See [Writing YAML](../guide/writing.md) for supported Common Lisp value types
and their YAML representation behavior.

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
| `edit-source source path value &key (document 0) (operation :set)` | Simple string or octet vector in the source encoding | `yaml-parse-error`, `yaml-compose-error`, `yaml-resource-limit-error`, `yaml-emit-error`, `yaml-format-edit-path-error`, `yaml-format-edit-anchor-error`, `yaml-format-edit-structure-error` |
| `edit-source/k source path value &key (document 0) (operation :set) (k nil)` | Primary value returned by `k`; calls `k` once with the edited source | Same as `edit-source`, plus `type-error` if `k` is omitted or not a function |

`edit-source` uses the parser's default resource limits and can signal
`yaml-resource-limit-error` while reading the source. See the
[format-preserving editing reference](api-format-editing.md) for path rules,
supported edits, encoding behavior, and preservation limits. Edit-condition
readers are listed in the [conditions reference](conditions.md).

## Sentinel and mapping values

| Symbol | Lambda list or value | Returns |
| --- | --- | --- |
| `+yaml-null+`, `+yaml-false+` | constants | Opaque null and false markers |
| `yaml-null-p`, `yaml-false-p` | `(value)` | Boolean identity test |
| `make-yaml-mapping` | `(&optional entries)` | New ordered mapping |
| `yaml-mapping-p` | `(object)` | Boolean |
| `yaml-mapping-entries` | `(mapping)` | Mapping entry alist |

See [structures](api-structures.md) for marker and ordered mapping behavior.

## Tokens

| Symbol | Lambda list or access | Result |
| --- | --- | --- |
| `token` | structure type | Token structure |
| `make-token` | `(kind start-span end-span &key value handle suffix style major minor)` | Token with field types checked |
| `token-kind` | `(token)` | Kind keyword |
| `token-start-span`, `token-end-span` | `(token)` | Source spans |
| `token-value`, `token-handle`, `token-suffix` | `(token)` | Token text fields |
| `token-style`, `token-major`, `token-minor` | `(token)` | Style or version fields |

`make-token` checks that `kind` is a keyword and that each field has its
declared type. It does not check that `kind` is one of the token kinds listed in
[API Structures](api-structures.md) or that the fields match that kind.

## Nodes

| Symbol | Lambda list or access | Result |
| --- | --- | --- |
| `scalar-node` | structure type | Scalar representation node |
| `sequence-node` | structure type | Sequence representation node |
| `mapping-node` | structure type | Mapping representation node |
| `make-scalar-node` | keyword constructor | Scalar node |
| `make-sequence-node` | keyword constructor | Sequence node |
| `make-mapping-node` | keyword constructor | Mapping node |
| `scalar-node-p` | `(object)` | Boolean type test |
| `sequence-node-p` | `(object)` | Boolean type test |
| `mapping-node-p` | `(object)` | Boolean type test |
| `node-tag` | `(node)` | Tag |
| `node-anchor` | `(node)` | Anchor |
| `node-style` | `(node)` | Style |
| `node-start-span` | `(node)` | Start span |
| `node-end-span` | `(node)` | End span |
| `scalar-node-value` | `(scalar-node)` | Scalar text |
| `sequence-node-items` | `(sequence-node)` | Sequence items |
| `mapping-node-pairs` | `(mapping-node)` | Ordered key/value pairs |

See [structures](api-structures.md) for constructor keywords, node fields, and
ordered mapping semantics.

## Events

| Symbol group | Exported symbols |
| --- | --- |
| Types | `stream-start-event`, `stream-end-event`, `document-start-event`, `document-end-event`, `sequence-start-event`, `sequence-end-event`, `mapping-start-event`, `mapping-end-event`, `scalar-event`, `alias-event` |
| Constructors | `make-stream-start-event`, `make-stream-end-event`, `make-document-start-event`, `make-document-end-event`, `make-sequence-start-event`, `make-sequence-end-event`, `make-mapping-start-event`, `make-mapping-end-event`, `make-scalar-event`, `make-alias-event` |
| Predicates | `stream-start-event-p`, `stream-end-event-p`, `document-start-event-p`, `document-end-event-p`, `sequence-start-event-p`, `sequence-end-event-p`, `mapping-start-event-p`, `mapping-end-event-p`, `scalar-event-p`, `alias-event-p` |
| Span readers | `event-start-span`, `event-end-span` |
| Field readers | `document-start-event-explicit-p`, `document-start-event-version`, `document-start-event-tag-directives`, `document-end-event-explicit-p`, `sequence-start-event-anchor`, `sequence-start-event-tag`, `sequence-start-event-implicit-p`, `sequence-start-event-style`, `mapping-start-event-anchor`, `mapping-start-event-tag`, `mapping-start-event-implicit-p`, `mapping-start-event-style`, `scalar-event-anchor`, `scalar-event-tag`, `scalar-event-value`, `scalar-event-plain-implicit-p`, `scalar-event-quoted-implicit-p`, `scalar-event-style`, `alias-event-anchor` |

See [structures](api-structures.md) for event constructor keywords and field
meaning.

## Conditions

`yaml-kit-error` is the base condition and stores a diagnostic. The
`cl-parser-kit` type and readers are not re-exported by `yaml-kit`; use
`cl-parser-kit:diagnostic-message` and `cl-parser-kit:diagnostic-span` to read
its message and span. `yaml-parse-error`,
`yaml-compose-error`, `yaml-emit-error`, `yaml-resource-limit-error`,
`yaml-format-edit-path-error`, `yaml-format-edit-structure-error`, and
`yaml-format-edit-anchor-error` are its exported subtypes. Their exported
readers and defaults are specified in [Conditions](conditions.md). The
resource-limit condition has no exported detail readers. Exported readers are
`yaml-kit-error-diagnostic`, `yaml-parse-error-context`,
`yaml-compose-error-context`, `yaml-compose-error-cause`,
`yaml-emit-error-context`, `yaml-emit-error-cause`,
`yaml-format-edit-path-error-path`, `yaml-format-edit-path-error-document`,
`yaml-format-edit-path-error-operation`,
`yaml-format-edit-structure-error-path`,
`yaml-format-edit-anchor-error-path`, and `yaml-format-edit-anchor-error-anchor`.

