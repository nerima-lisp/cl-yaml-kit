# Format-Preserving Editing

`edit-source` changes one value or removes one entry while retaining the
surrounding source text. See the [API reference](api.md) for its signature and
the [conditions reference](conditions.md) for error readers.

## Source and path

`source` must be a string or an octet vector. A string source returns as a
simple string. An octet vector is decoded using its detected encoding and
returned as octets in that encoding, preserving a byte-order mark when present.
Streams and other input types signal `yaml-format-edit-path-error`. Invalid
octet encodings and malformed YAML signal `yaml-parse-error`. A representation
graph that cannot be composed can signal `yaml-compose-error`.

`path` is a non-empty proper list from the document root to the target. Use
string components for mapping keys and non-negative integer components for
zero-based sequence indexes. `document` is a non-negative zero-based index and
defaults to `0`. `operation` defaults to `:set`; it also accepts `:delete`.
Invalid paths, document indexes, or operations signal
`yaml-format-edit-path-error`. A missing document also signals that condition.

All path components before the final component must exist. The editor does not
create missing intermediate mappings or sequences. An integer component under
a mapping signals `yaml-format-edit-path-error`.

## Set a scalar

When the path exists, `:set` replaces an untagged, single-line scalar written
in plain, single-quoted, or double-quoted style. Quoted empty scalars such as
`''` and `""` are supported. Implicit empty values, block scalars, multiline
scalars, mappings, and sequences cannot be replaced and signal
`yaml-format-edit-structure-error`.

Replacement values can be strings, characters, integers, Common Lisp floats,
`t`, `+yaml-null+`, or `+yaml-false+`. `nil`, mappings, and sequences are not
scalar replacement values. Strings used as replacement values or added mapping
keys cannot contain surrogate code points from U+D800 through U+DFFF, U+FFFE,
or U+FFFF; such edits signal `yaml-format-edit-structure-error`.

The final path value and every value traversed to reach it must be untagged and
cannot be anchored or an alias. Mapping keys with an anchor or alias are also
rejected. A path through a tagged value signals
`yaml-format-edit-structure-error`; a path involving anchors or aliases signals
`yaml-format-edit-anchor-error`. Traversed mappings with duplicate keys or
merge keys are unsupported.

## Add or delete an entry

When `:set` targets a final path component that does not exist, it adds a
mapping entry or sequence item. Mapping additions require a string key.
Sequence additions must use the next zero-based index, so additions are
append-only. The parent must be a block collection. Additions to flow
collections are unsupported.

`:delete` requires an existing path. Its positional `value` argument is
required but ignored. Deletion from flow collections and deletion of a
collection's only child signal `yaml-format-edit-structure-error`.

The editor also rejects additions to or deletions from a collection opened on
the same line as a sequence entry, such as the mapping in `- key: value`.
These edits signal `yaml-format-edit-structure-error`.

The editor verifies the semantic result after applying the text edit. If an
edit changes a value outside the requested path, including a value resolved
through an alias, it signals `yaml-format-edit-structure-error`.
Text that fails YAML parsing after the edit also signals
`yaml-format-edit-structure-error`.

## Source preservation and limits

For supported edits, the editor preserves comments, key order, line endings,
the final newline state, and source text outside the edit. Replacing a quoted
scalar can change its quote style. The emitter selects a safe style for the
replacement; replacement strings in flow collections use single quotes when
their content requires protection from flow indicators. A replacement string
that contains a line break is emitted as a double-quoted scalar with YAML
escapes.

The editor parses the source with the parser's default resource limits, so an
oversized or deeply nested source can signal `yaml-resource-limit-error`.
