# Conditions

All exported conditions inherit from `yaml-kit-error`, which inherits from
`error`. The source condition definitions contain the slots and readers below;
`package.lisp` exports the parse-error readers and the compose/emit location
and cause readers. Readers not listed as exports are implementation details.

## Hierarchy and slots

| Condition | Readers and defaults | When it is signalled |
| --- | --- | --- |
| `yaml-kit-error` | `yaml-kit-error-message` (not exported), default `nil` | Base for library failures |
| `yaml-parse-error` | `yaml-parse-error-line` `nil`, `yaml-parse-error-column` `nil`, `yaml-parse-error-offset` 0, `yaml-parse-error-context` `"YAML"` | Scanning or parsing invalid input |
| `yaml-compose-error` | `yaml-compose-error-mark` `nil`, `yaml-compose-error-context` `"YAML composition"`, `yaml-compose-error-cause` `nil` | Invalid event stream, alias, tag, scalar, or mapping graph |
| `yaml-emit-error` | `yaml-emit-error-mark` `nil`, `yaml-emit-error-context` `"YAML emission"`, `yaml-emit-error-cause` `nil` | Unsupported Lisp value or invalid emission state |
| `yaml-resource-limit-error` | Resource-limit readers are not exported; defaults are `"resource"`, `nil`, `nil`, `nil`, `"YAML"` for limit name, limit, actual, mark, and context | An input, depth, scalar, node, or alias limit is exceeded |

The mark readers return a `mark` or `nil`. A mark contains line, column, and
offset fields read with `mark-line`, `mark-column`, and `mark-offset`. Parse
Scanner-derived parse errors use zero-based line, column, and offset values.
When the parser has no token mark, its fallback line and column are `1` and its
offset is `0`. Compose, emit, and resource errors may carry the mark at which
the operation detected the failure.

## Resource limits

The default limits and their entry points are listed in the [API
reference](api.md). Set a keyword to a smaller or larger value to change the
bound. A `nil` bound is accepted by the composer, but the loader supplies its
numeric default.
Exceeding a bound signals `yaml-resource-limit-error`; the condition names the
limit and records the configured and actual values.

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
composition, emission, and resource failures. The report methods include the
context and, where available, mark coordinates. Conditions do not expose a
separate input snippet or path accessor in the current public API.
