# Conditions

All exported conditions inherit from `yaml-kit-error`, which inherits from
`error`.

## Hierarchy and slots

| Condition | Readers and defaults | When it is signalled |
| --- | --- | --- |
| `yaml-parse-error` | `yaml-parse-error-line` `nil`, `yaml-parse-error-column` `nil`, `yaml-parse-error-offset` 0, `yaml-parse-error-context` `"YAML"` | Scanning or parsing invalid input |
| `yaml-compose-error` | `yaml-compose-error-mark` `nil`, `yaml-compose-error-context` `"YAML composition"`, `yaml-compose-error-cause` `nil` | Invalid event stream, alias, tag, scalar, or mapping graph |
| `yaml-emit-error` | `yaml-emit-error-mark` `nil`, `yaml-emit-error-context` `"YAML emission"`, `yaml-emit-error-cause` `nil` | Unsupported Lisp value or invalid emission state |
| `yaml-resource-limit-error` | No detail or mark readers are exported | An input, depth, scalar, node, or alias limit is exceeded |
| `yaml-format-edit-path-error` | `yaml-format-edit-path-error-path` `nil`, `yaml-format-edit-path-error-document` `0`, `yaml-format-edit-path-error-operation` `nil` | Invalid or missing edit path, document, or operation |
| `yaml-format-edit-structure-error` | `yaml-format-edit-structure-error-path` `nil` | Unsupported edit to the YAML structure |
| `yaml-format-edit-anchor-error` | `yaml-format-edit-anchor-error-path` `nil`, `yaml-format-edit-anchor-error-anchor` `nil` | Edit path includes an anchored node, alias, or anchored mapping key |

The `yaml-compose-error-cause` and `yaml-emit-error-cause` readers return the
cause value supplied by the failing stage, or `nil` when it supplies none. A
cause may be an underlying condition or diagnostic text. The condition report
includes a cause when available.

`yaml-compose-error-mark` and `yaml-emit-error-mark` return a `mark` or `nil`.
A mark contains line, column, and offset fields read with `mark-line`,
`mark-column`, and `mark-offset`. Resource-limit `~A` reports include the limit
name, configured limit, actual value, context, and mark coordinates when
available.

The table gives slot defaults for conditions created without explicit values.
Scanner errors set the offending character's mark, with zero-based line,
column, and offset values. When the parser raises an error without a current
token, it sets fallback line, column, and offset values to `1`, `1`, and `0`;
these fallback values are not source coordinates. Scanner marks start at
`(0, 0, 0)` and advance the offset with each character.

## Resource limits

See the [API reference](api.md) for default limits and their entry points. Set
a keyword to a numeric value to change the bound. For
`compose-events` and `compose-all-events`, `:max-depth`, `:max-scalar-length`,
`:max-nodes`, and `:max-alias-expansions` accept `nil` to disable that limit.
Raw-input entry points use their numeric defaults when a limit keyword is
omitted; pass a number when overriding one.

## Handling failures

```lisp
(handler-case
    (yaml-kit:parse "key: [")
  (yaml-kit:yaml-parse-error (condition)
    (list (yaml-kit:yaml-parse-error-line condition)
          (yaml-kit:yaml-parse-error-column condition)
          (yaml-kit:yaml-parse-error-context condition))))
```

Catch `yaml-kit-error` when the caller does not need to distinguish parsing,
composition, emission, and resource failures. Formatting a condition with `~A`
prints its report, which includes context and, where available, mark
coordinates. Conditions do not expose a separate input snippet.
