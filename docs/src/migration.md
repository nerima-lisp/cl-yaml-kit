# Migration from v0.2.1 to v0.3.0

This guide treats the main snapshot at commit `4daf11fd0aed4c3d5b6366b60d4156832e8f6a55` as the v0.3.0 target and compares it with the v0.2.1 tag at commit `91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e`. It identifies source-level changes that callers need to address.

The code blocks are illustrative fragments. They show API substitutions and omit surrounding application code, so they are not complete programs to run as written.

## Marks become spans and diagnostics

Tokens, representation nodes, and events now expose spans instead of marks. The mark type and its constructor and accessors are no longer exported. Replace the corresponding `*-start-mark` and `*-end-mark` accessors with their `*-span` forms. When constructing a token, pass spans in the existing start and end positions; for node and event constructors, replace `:start-mark` and `:end-mark` initargs with `:start-span` and `:end-span`.

```lisp
;; v0.2.1
(let ((mark (yaml-kit:event-start-mark event)))
  (list (yaml-kit:mark-line mark)
        (yaml-kit:mark-column mark)
        (yaml-kit:mark-offset mark)))

;; main
(let ((span (yaml-kit:event-start-span event)))
  (list (cl-parser-kit:span-start-line span)
        (cl-parser-kit:span-start-column span)
        (cl-parser-kit:span-start span)))
```

The old scanner counted line and column from zero. The new scanner creates spans with one-based line and column values while retaining the character offset from zero. Spans use half-open start and end offsets measured in decoded characters. If callers depend on zero-based line and column values, subtract one from the span's start line and column. Keep the start offset unchanged. [v0.2.1 `src/scanner-state.lisp:14-17,62-73`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/scanner-state.lisp#L14-L17) [main `src/scanner-state.lisp:68-83`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/scanner-state.lisp#L68-L83)

The token and node definitions make the same field change. [v0.2.1 `src/tokens.lisp:4-10`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/tokens.lisp#L4-L10), [main `src/tokens.lisp:4-10`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/tokens.lisp#L4-L10); [v0.2.1 `src/nodes.lisp:4-11`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/nodes.lisp#L4-L11), [main `src/nodes.lisp:4-11`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/nodes.lisp#L4-L11); [v0.2.1 `src/events.lisp:4-14`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/events.lisp#L4-L14), [main `src/events.lisp:4-8`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/events.lisp#L4-L8).

Parse errors also moved their location from separate condition slots to a diagnostic. Read the condition's diagnostic, then inspect its span. A diagnostic can have no span, so check for `nil` before reading span fields. [v0.2.1 `src/conditions.lisp:11-28`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/conditions.lisp#L11-L28), [main `src/conditions.lisp:11-40`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/conditions.lisp#L11-L40)

```lisp
;; v0.2.1
(yaml-kit:yaml-parse-error-line condition)

;; main
(let* ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition))
       (span (cl-parser-kit:diagnostic-span diagnostic)))
  (when span
    (cl-parser-kit:span-start-line span)))
```


## Scanner functions are no longer public

The exported `make-scanner`, `scanner-peek-token`, and `scanner-next-token` functions were removed. The main package still exports event-level `map-events` and `parse-events`, but these expose events rather than the scanner's token-by-token interface. Use them only when event-level processing meets the caller's needs; there is no exported token-scanning replacement in the current API.

```lisp
;; v0.2.1
(let ((scanner (yaml-kit:make-scanner input)))
  (yaml-kit:scanner-peek-token scanner))

;; main, when an event stream is sufficient
(yaml-kit:map-events #'handle-event input)
```

The removed exports are listed in the v0.2.1 package definition. The current package exports `map-events` and `parse-events` as event APIs. [v0.2.1 `src/package.lisp:13-23`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/package.lisp#L13-L23), [main `src/package.lisp:5-10`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/package.lisp#L5-L10), [main `src/parser-entry.lisp:49-62`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/parser-entry.lisp#L49-L62)

## Implicit flow-sequence mappings emit corrected events

In v0.2.1, an implicit mapping inside a flow sequence could emit two
`mapping-end-event`s. For `[a:]`, it also omitted the empty value
`scalar-event`. This affects `parse-events` and `map-events` consumers that
compensated for the old output. Update those handlers to expect one
`mapping-end-event` for `[a:]`, `[? a]`, `[a, ? b]`, and `[?]`; `[a:]` now
includes its empty value event.

For `[a:]`, `[? a]`, and `[?]`, `parse-events` emits the YAML 1.2 event order:

```text
stream-start-event
document-start-event
sequence-start-event
mapping-start-event
scalar-event
scalar-event
mapping-end-event
sequence-end-event
document-end-event
stream-end-event
```

`[a, ? b]` also emits a `scalar-event` for `a` immediately after
`sequence-start-event`, before the implicit mapping. Empty keys and values are
represented by scalar events with an empty value. PR #17 fixed the parser in
commit [`8f79893ed5b1cb0c171f61cc6d1707443667451d`](https://github.com/nerima-lisp/cl-yaml-kit/commit/8f79893ed5b1cb0c171f61cc6d1707443667451d), and tests assert the event
types for all four inputs. [v0.2.1 `src/parser-flow.lisp:13-14`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/parser-flow.lisp#L13-L14), [v0.2.1 `src/parser-flow.lisp:47-56`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/parser-flow.lisp#L47-L56), [fix `src/parser-flow.lisp:51-63`](https://github.com/nerima-lisp/cl-yaml-kit/blob/8f79893ed5b1cb0c171f61cc6d1707443667451d/src/parser-flow.lisp#L51-L63), [main `t/parser-state-test.lisp:105-136`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/parser-state-test.lisp#L105-L136)

## Continuation APIs are available

The current package adds continuation variants for comment extraction, parsing, event parsing, composition, emission, and source editing. Use the matching `/k` function when the operation result should be passed directly to a callback.

| Operation | New function |
| --- | --- |
| Comments | `comment-spans/k` |
| Reading and construction | `parse/k`, `parse-all/k`, `read-yaml/k`, `compose/k`, `compose-all/k` |
| Event parsing and composition | `parse-events/k`, `map-events/k`, `compose-events/k`, `compose-all-events/k` |
| Emission | `emit/k`, `write-yaml/k`, `emit-events/k` |
| Format-preserving editing | `edit-source/k` |

For example, call `parse/k` with the input and a function that accepts the parsed document.

```lisp
;; v0.2.1
(consume (yaml-kit:parse input))

;; main
(yaml-kit:parse/k input #'consume)
```

Every `/k` callback's secondary values are discarded; each wrapper returns only the callback's primary value. If callers need several results, return one aggregate value such as a list or structure from the callback.

```lisp
(defun consume (document)
  (list document :metadata))

(yaml-kit:parse/k input #'consume)
```

The exports are defined in the package file. The reading wrappers call their callback with the ordinary operation result, the event and composer wrappers do the same for their results, the writer wrappers pass the emitted output or original value as documented by each function, and `edit-source/k` explicitly documents the primary-value rule. [main `src/package.lisp:5-15,50-51`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/package.lisp#L5-L15); [main `src/loader.lisp:88-177`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/loader.lisp#L88-L177); [main `src/parser-entry.lisp:64-99`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/parser-entry.lisp#L64-L99); [main `src/composer.lisp:192-200,240-256`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/composer.lisp#L192-L200); [main `src/dumper.lisp:27-84`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/dumper.lisp#L27-L84); [main `src/format-preserving-edit.lisp:106-118`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/format-preserving-edit.lisp#L106-L118)

## Condition types remain, but their data changed

The public condition type names remain `yaml-kit-error`, `yaml-parse-error`, `yaml-compose-error`, `yaml-emit-error`, `yaml-resource-limit-error`, `yaml-format-edit-path-error`, `yaml-format-edit-structure-error`, and `yaml-format-edit-anchor-error`. Their slots and initargs are not all compatible with v0.2.1.

| Condition data | v0.2.1 | Main | Migration action |
| --- | --- | --- | --- |
| Base error message | `:message` | `:diagnostic` containing a `cl-parser-kit:diagnostic` | Construct a diagnostic with `:message` and optional `:span`, then pass it as `:diagnostic`. |
| Parse location | `:line`, `:column`, and `:offset` slots and readers | Location is carried by the diagnostic span; `:context` remains | Replace the coordinate readers with `yaml-kit-error-diagnostic` and span accessors. |
| Compose and emit location | `:mark` initarg and `yaml-compose-error-mark` or `yaml-emit-error-mark`; `:context` and `:cause` remain | Diagnostic replaces the mark and message fields; `:context` and `:cause` remain | Pass `:diagnostic` with its span and keep using context and cause where needed. |
| Resource limit location and details | `:mark` initarg; the limit name, limit, actual, and context readers are package-private | The mark slot is gone; the same detail readers remain package-private, and the public diagnostic carries span and data | Use the public `yaml-kit-error-diagnostic` accessor, then read `cl-parser-kit:diagnostic-span` or `cl-parser-kit:diagnostic-data`. |
| Format edit error message | Inherited `:message`; path, document, operation, or anchor fields are condition-specific | Inherited `:diagnostic`; the condition-specific fields remain | Pass a diagnostic for the message and continue reading the relevant path or anchor field. |

The old and current condition definitions show the initarg changes. [v0.2.1 `src/package.lisp:48-60`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/package.lisp#L48-L60), [main `src/package.lisp:52-64`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/package.lisp#L52-L64); [v0.2.1 `src/conditions.lisp:11-74`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/conditions.lisp#L11-L74), [main `src/conditions.lisp:21-81`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/conditions.lisp#L21-L81)

For example, a parse error that previously passed separate coordinates and a message now passes a diagnostic.

```lisp
;; v0.2.1
(error 'yaml-kit:yaml-parse-error
       :line line :column column :offset offset
       :context context :message message)

;; main
(error 'yaml-kit:yaml-parse-error
       :context context
       :diagnostic (cl-parser-kit:make-diagnostic
                    :message message :span span))
```

## Parser and composer resource limit messages have a new format

For an unmarked depth-limit error with the default context, the v0.2.1 composer condition report includes the context and a parenthesized limit detail. In main, parser and composer resource-limit errors use a diagnostic message that names the limit directly. The strings below show the complete v0.2.1 report and the main diagnostic message for that case. [v0.2.1 `src/composer.lisp:12-15`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/composer.lisp#L12-L15), [v0.2.1 `src/conditions.lisp:69-85`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/conditions.lisp#L69-L85), [main `src/conditions.lisp:61-81`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/conditions.lisp#L61-L81), [main `t/parser-test.lisp:86-90`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/t/parser-test.lisp#L86-L90)

```text
v0.2.1 condition report:
YAML resource limit exceeded: YAML (depth limit 0, actual 1)

main diagnostic message:
YAML resource limit "depth" exceeded: limit 0, actual 1
```

The limit name, limit, actual value, and context are not exported condition readers in main. Read the public diagnostic data instead:

```lisp
(let* ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition))
       (data (cl-parser-kit:diagnostic-data diagnostic)))
  (list (getf data :limit-name)
        (getf data :limit)
        (getf data :actual)
        (getf data :context)))
```

Replace exact report-string comparisons with a check for the `yaml-resource-limit-error` condition type and comparisons of `:limit-name`, `:limit`, and `:actual` in the public diagnostic data. Read `:context` there too if needed. The condition class and diagnostic accessor are public; the similarly named detail-reader functions are not. [main `src/package.lisp:52-60`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/package.lisp#L52-L60), [main `src/conditions.lisp:61-81`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/conditions.lisp#L61-L81)

## Source edits reject additional scalar values

The current format-preserving edit renderer explicitly rejects surrogate code points U+D800 through U+DFFF and U+FFFE and U+FFFF in replacement strings. It signals `yaml-format-edit-structure-error` with the edit path. The v0.2.1 fragment builder shown below checks composite values but does not contain this specific character check. That source alone does not establish whether every such value succeeded after later emission or encoding steps.

```lisp
;; v0.2.1
(when (or (consp value) (yaml-mapping-p value)
          (and (arrayp value) (not (stringp value)))
          (hash-table-p value))
  (%format-edit-structure-error nil "only scalar values can be set"))
(emit value :default-flow-style :flow)

;; main
(%format-edit-check-printable-value value path)
```

Validate or normalize replacement strings before editing if they may contain these characters. If handling the library condition, inspect `yaml-format-edit-structure-error-path` to locate the rejected value. Current tests cover both surrogate boundaries and both noncharacters. [v0.2.1 `src/format-preserving-edit-render.lisp:56-72`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/format-preserving-edit-render.lisp#L56-L72), [main `src/format-preserving-edit-render.lisp:59-76`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/format-preserving-edit-render.lisp#L59-L76), [main `t/format-preserving-edit-errors-test.lisp:131-150`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/t/format-preserving-edit-errors-test.lisp#L131-L150)

## Other confirmed diagnostic text changes

| Case | v0.2.1 | Main | Migration action |
| --- | --- | --- | --- |
| Invalid YAML byte input | The decoder detail is stored as the parse error message under context `invalid YAML input`. | The diagnostic message starts with `invalid YAML input: ` followed by the decoder detail. | Match the condition type or inspect the diagnostic instead of matching the full rendered text. |
| Invalid source encoding during `edit-source` | The decoder or encoder detail is stored as the parse error message under context `invalid source encoding`. | The diagnostic message starts with `invalid source encoding: ` followed by the detail. | Apply the same diagnostic-based handling as invalid YAML byte input. |
| Incompatible YAML major version | The scanner reports `found incompatible YAML document`. | The scanner reports `found incompatible YAML major version`. | Update exact-message assertions and use the diagnostic message for display. |
| Duplicate key in alist construction | The compose error cause says `non-scalar mapping key`. | The diagnostic and cause say `duplicate mapping key`. | Update exact-message assertions; the new message names the duplicate-key failure. |

These messages are constructed at the cited error paths. [v0.2.1 `src/parser-entry.lisp:30-39`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/parser-entry.lisp#L30-L39), [main `src/parser-entry.lisp:30-47`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/parser-entry.lisp#L30-L47); [v0.2.1 `src/format-preserving-edit-source.lisp:63-69`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/format-preserving-edit-source.lisp#L63-L69), [main `src/format-preserving-edit-source.lisp:73-83`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/format-preserving-edit-source.lisp#L73-L83); [v0.2.1 `src/parser.lisp:24-34`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/parser.lisp#L24-L34), [main `src/scanner-directives.lisp:40-46`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/scanner-directives.lisp#L40-L46); [v0.2.1 `src/constructor-collections.lisp:24-39`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/constructor-collections.lisp#L24-L39), [main `src/constructor-collections.lisp:24-39`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/constructor-collections.lisp#L24-L39)

## Complete public export difference

The lists below are the complete export additions and removals between the two compared package definitions. This summary includes the changes described above so callers can check for exported-symbol references in one place. [v0.2.1 `src/package.lisp:4-60`](https://github.com/nerima-lisp/cl-yaml-kit/blob/91ea1ea94de590eb6521e9c2d4ccd6b2619ba57e/src/package.lisp#L4-L60), [main `src/package.lisp:4-64`](https://github.com/nerima-lisp/cl-yaml-kit/blob/4daf11fd0aed4c3d5b6366b60d4156832e8f6a55/src/package.lisp#L4-L64)

| Added exports | Removed exports |
| --- | --- |
| `token-start-span`, `token-end-span`, `node-start-span`, `node-end-span`, `event-start-span`, `event-end-span` | `mark`, `make-mark`, `mark-line`, `mark-column`, `mark-offset`; `token-start-mark`, `token-end-mark`, `node-start-mark`, `node-end-mark`, `event-start-mark`, `event-end-mark` |
| `comment-spans/k`, `parse/k`, `parse-all/k`, `read-yaml/k`, `parse-events/k`, `map-events/k`, `compose/k`, `compose-all/k`, `compose-events/k`, `compose-all-events/k`, `emit/k`, `write-yaml/k`, `emit-events/k`, `edit-source/k` | `make-scanner`, `scanner-peek-token`, `scanner-next-token` |
| `yaml-kit-error-diagnostic` | `yaml-parse-error-line`, `yaml-parse-error-column`, `yaml-parse-error-offset`, `yaml-compose-error-mark`, `yaml-emit-error-mark` |
