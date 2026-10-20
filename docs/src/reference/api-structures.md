# API Structures

The API reference lists every exported structure and accessor. This page
describes their fields and constructor behavior. For parser and composer
functions that consume these values, see the [API reference](api.md).

## Null and false markers

`+yaml-null+` and `+yaml-false+` are opaque values for YAML null and false.
They are distinct from `nil` and the Common Lisp value `t`. Use `yaml-null-p`
and `yaml-false-p` to test a value by identity.

## Ordered mappings

`make-yaml-mapping` accepts an optional association list of key/value conses.
It copies the outer list and keeps the supplied conses in their original order.
`yaml-mapping-entries` returns that ordered list, including repeated keys.
`yaml-mapping-p` tests whether a value is a `yaml-mapping` structure.

## Tokens

`make-token` takes `kind`, `start-span`, and `end-span`, followed by optional
`value`, `handle`, `suffix`, `style`, `major`, and `minor` fields. It checks
field types: `kind` must be a keyword; spans must be `cl-parser-kit:span` values
or `nil`; text fields must be simple strings or `nil`; `style` must be `nil` or
a scalar style; and version fields must be fixnums or `nil`.
The constructor does not restrict `kind` to the scanner's token kinds. It also
does not check that fields are appropriate for a given kind. Callers that
construct tokens are responsible for that consistency.

The scanner uses these token kinds:

| Category | Kinds |
| --- | --- |
| Stream and document | `:stream-start`, `:stream-end`, `:document-start`, `:document-end` |
| Directives | `:version-directive`, `:tag-directive` |
| Block collections | `:block-sequence-start`, `:block-mapping-start`, `:block-end`, `:block-entry` |
| Flow collections | `:flow-sequence-start`, `:flow-sequence-end`, `:flow-mapping-start`, `:flow-mapping-end`, `:flow-entry` |
| Node properties and content | `:key`, `:value`, `:scalar`, `:anchor`, `:tag`, `:alias` |

`token-value` holds scalar text, an anchor or alias name, or a tag-directive
prefix, depending on the token kind. For a tag directive, `token-handle` holds
the handle and `token-value` holds the prefix. For a tag token,
`token-handle` and `token-suffix` hold the handle and suffix. `token-style`
holds a scalar style. `token-major` and `token-minor` hold a YAML version. Span
fields can be `nil` on caller-created tokens; scanner tokens carry source
spans.

`yaml-kit` exports token structures and fields, but no public scanner returns
them and no public parser consumes caller-created tokens. Token-by-token
parsing has no public entry point; use event APIs for parser output and
composition.

Scalar styles are `:plain`, `:single-quoted`, `:double-quoted`, `:literal`, and
`:folded`.

## Representation nodes

The node constructors accept the inherited fields `:tag`, `:anchor`, `:style`,
`:start-span`, and `:end-span`. `make-scalar-node` also accepts `:value`,
`make-sequence-node` accepts `:items`, and `make-mapping-node` accepts `:pairs`.

`scalar-node-p`, `sequence-node-p`, and `mapping-node-p` test node types.
`node-tag`, `node-anchor`, `node-style`, `node-start-span`, and `node-end-span`
read the inherited fields. Tags and anchors are simple strings or `nil`.
Styles are `nil`, a scalar style, or a collection style. Spans are
`cl-parser-kit:span` values or `nil`.

`scalar-node-value` returns the scalar string. `sequence-node-items` returns
the items list. `mapping-node-pairs` returns the pairs list; parser-created
pairs are ordered key/value conses, and duplicate keys remain present in this
representation. Parsing a mapping into a Common Lisp mapping type applies the
selected duplicate-key policy described in the
[data model guide](../guide/data-model.md).

## Events

Each event constructor accepts the inherited `:start-span` and `:end-span`
keywords. These fields default to `nil` and hold spans or `nil`.

| Event | Additional constructor fields |
| --- | --- |
| `stream-start-event`, `stream-end-event` | None |
| `document-start-event` | `:explicit-p`, `:version`, `:tag-directives` |
| `document-end-event` | `:explicit-p` |
| `sequence-start-event`, `mapping-start-event` | `:anchor`, `:tag`, `:implicit-p`, `:style` |
| `sequence-end-event`, `mapping-end-event` | None |
| `scalar-event` | `:anchor`, `:tag`, `:value`, `:plain-implicit-p`, `:quoted-implicit-p`, `:style` |
| `alias-event` | `:anchor` |

The sequence and mapping start events use `:block` or `:flow` for `:style`.
Their `:implicit-p` field states whether the collection tag can be omitted.
Scalar events use one of the scalar styles listed above. The two scalar
implicit fields state whether a tag can be omitted for the corresponding plain
or quoted style.

The event predicates test the corresponding event type. `event-start-span` and
`event-end-span` read the inherited span fields. Each event-specific reader
returns the matching constructor field; for example,
`scalar-event-value` reads `:value` from a scalar event and
`alias-event-anchor` reads `:anchor` from an alias event.
