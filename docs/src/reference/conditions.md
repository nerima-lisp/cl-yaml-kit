# Conditions

All exported error subtypes inherit from `yaml-kit-error`, which inherits from
`error`. Each condition stores a `cl-parser-kit:diagnostic`, available through
`yaml-kit-error-diagnostic`.

## Condition readers

| Condition | Exported readers and defaults | When it is signalled |
| --- | --- | --- |
| `yaml-kit-error` | `yaml-kit-error-diagnostic`, default diagnostic message `"YAML error"` | Base type for library errors |
| `yaml-parse-error` | `yaml-parse-error-context`, default `"YAML"` | Invalid input or parse failure |
| `yaml-compose-error` | `yaml-compose-error-context`, default `"YAML composition"`; `yaml-compose-error-cause`, default `nil` | Invalid event stream or representation graph |
| `yaml-emit-error` | `yaml-emit-error-context`, default `"YAML emission"`; `yaml-emit-error-cause`, default `nil` | Unsupported output value or emission failure |
| `yaml-resource-limit-error` | No resource detail readers are exported | An input, depth, scalar, node, or alias limit is exceeded |
| `yaml-format-edit-path-error` | `yaml-format-edit-path-error-path`, default `nil`; `yaml-format-edit-path-error-document`, default `0`; `yaml-format-edit-path-error-operation`, default `nil` | Invalid edit request, missing document, or path |
| `yaml-format-edit-structure-error` | `yaml-format-edit-structure-error-path`, default `nil` | Unsupported edit to the YAML structure |
| `yaml-format-edit-anchor-error` | `yaml-format-edit-anchor-error-path`, default `nil`; `yaml-format-edit-anchor-error-anchor`, default `nil` | Edit involves an anchored node, alias, or anchored mapping key |

`yaml-kit-error-diagnostic` returns a `cl-parser-kit:diagnostic`. Read its text
with `cl-parser-kit:diagnostic-message` and its optional span with
`cl-parser-kit:diagnostic-span`. A span provides start and end offsets through
`cl-parser-kit:span-start` and `cl-parser-kit:span-end`, and line and column
coordinates through `cl-parser-kit:span-start-line`,
`cl-parser-kit:span-start-column`, `cl-parser-kit:span-end-line`, and
`cl-parser-kit:span-end-column`.

The standard condition report formats the diagnostic with
`cl-parser-kit:diagnostic->string`. The context and cause readers expose
separate condition slots; the report does not append them. A compose or emit
cause is `nil` when the failing stage supplied none.

`yaml-resource-limit-error` has no exported readers for its limit name, bound,
or actual value. Its diagnostic message describes the exceeded limit. When
present, `cl-parser-kit:diagnostic-span` returns its source span. Diagnostics
created for resource-limit errors include the limit name, bound, actual value,
and context in `cl-parser-kit:diagnostic-data`.

## Reading a parse diagnostic

```lisp
(handler-case
    (yaml-kit:parse "key: [")
  (yaml-kit:yaml-parse-error (condition)
    (let* ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition))
           (span (cl-parser-kit:diagnostic-span diagnostic)))
      (list (cl-parser-kit:diagnostic-message diagnostic)
            (and span (cl-parser-kit:span-start-line span))
            (and span (cl-parser-kit:span-start-column span)))))
```

Use the condition subtype when the caller needs to distinguish parsing,
composition, emission, resource-limit, and format-edit failures. Catch
`yaml-kit-error` when the diagnostic is sufficient.

## Resource limits

See the [API reference](api.md) for the default limits and the entry points
that accept each limit keyword. `compose-events` and `compose-all-events` also
accept `nil` for `:max-depth`, `:max-scalar-length`, `:max-nodes`, and
`:max-alias-expansions` to disable that limit. Raw-input entry points use their
numeric defaults when a limit keyword is omitted.
