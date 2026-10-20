# Getting Started

## Load the system

Make the repository and its dependencies visible to ASDF, then load the
system. See [Development](project/development.md) for Nix and manual setup
instructions.

```lisp
(asdf:load-system "cl-yaml-kit")
```

The runtime dependencies are `cl-regex-kit`, `cl-parser-kit`, and
`cl-codec-kit`. The test system additionally depends on `cl-weave`,
`cl-prolog-kit/weave`, and `cl-json-kit`.

## Parse values

`parse` reads the first YAML document. Mappings default to hash tables and
sequences to vectors. It accepts a string, an octet vector, or a character
stream; the examples here use strings. The Core schema resolves plain `null`,
booleans, integers, and floats; use `:schema :json` or `:schema :failsafe` to
select the other implemented schemas.

```lisp
(let* ((input (format nil "name: Ada~%roles: [admin, reviewer]"))
       (value (yaml-kit:parse input)))
  (list (gethash "name" value)
        (coerce (gethash "roles" value) 'list)))
;; => ("Ada" ("admin" "reviewer"))
```

`parse-all` returns a list containing one value for each document:

```lisp
(yaml-kit:parse-all (format nil "--- one~%--- two~%"))
;; => ("one" "two")
```

Use `:mapping-type :alist` or `:mapping-type :yaml-mapping`, and
`:sequence-type :list` when those representations are required. Duplicate
mapping keys signal `yaml-compose-error` by default; `:duplicate-key-policy`
accepts `:first` or `:last`.

## Emit values

`emit` returns YAML text. Proper lists and non-string vectors become sequences;
hash tables and `yaml-mapping` values become mappings. Strings become YAML
scalars. `write-yaml` writes the same output to an existing stream and returns
the original value.

```lisp
(yaml-kit:emit #(1 2 3))
;; =>
;; - 1
;; - 2
;; - 3

(with-output-to-string (stream)
  (yaml-kit:write-yaml '("a" "b") stream))
;; =>
;; - a
;; - b
```

The sentinels `+yaml-null+` and `+yaml-false+` represent YAML null and false.
Test them with `yaml-null-p` and `yaml-false-p`.

## Work with events

`map-events` calls a handler for each parser event and returns `nil`.
`parse-events` collects the same events into a list. `emit-events` accepts an
event list.

```lisp
(length (yaml-kit:parse-events "answer: 42"))
;; => 8

(yaml-kit:emit-events (yaml-kit:parse-events "answer: 42"))
;; =>
;; %TAG !! tag:yaml.org,2002:
;; %TAG ! !
;; ---
;; answer: 42
```

The parsed document-start event includes the default `!!` and `!` tag
directives, so `emit-events` writes them before the document.

## Handle errors

Malformed input signals `yaml-parse-error`; representation-graph failures
signal `yaml-compose-error`; unsupported output signals `yaml-emit-error`.
Resource exhaustion signals `yaml-resource-limit-error`. These conditions
inherit from `yaml-kit-error`. Use `yaml-kit-error-diagnostic` to inspect the
message and optional span; resource-limit details are available in the
diagnostic data. See
[Conditions](reference/conditions.md).
