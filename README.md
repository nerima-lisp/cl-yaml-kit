# cl-yaml-kit

`cl-yaml-kit` is a Common Lisp YAML reader and writer. It follows the YAML
1.2.2 scanner, parser, and emitter pipeline. The loader supports the Core,
JSON, and Failsafe schemas.

SBCL is required. Octet input uses automatic UTF-8, UTF-16, and UTF-32
detection, including BOM-less input.

## Install

Load the `cl-yaml-kit` ASDF system from a directory on `CL_SOURCE_REGISTRY`.
See [Getting Started](docs/src/getting-started.md) for runtime and test
dependencies.

```lisp
(asdf:load-system "cl-yaml-kit")
```

## Use

```lisp
(gethash "name" (yaml-kit:parse "name: Ada"))
;; => "Ada"

(yaml-kit:parse-all
 (format nil "--- one~%--- two"))
;; => ("one" "two")

(yaml-kit:emit (list 1 2 3))
;; =>
;; - 1
;; - 2
;; - 3

(yaml-kit:map-events
 (lambda (event) (declare (ignore event)))
 "answer: 42")
;; => NIL
```

See [`docs/src/getting-started.md`](docs/src/getting-started.md) for input
types, schemas, mappings, events, and error handling.

## Development

See [`docs/src/project/development.md`](docs/src/project/development.md) for
the development environment and test commands.

## License

MIT. See [`LICENSE`](LICENSE).
