# cl-yaml-kit

`cl-yaml-kit` is a Common Lisp implementation of YAML 1.2.2. The reader and
writer are based on the libyaml scanner/parser/emitter design. The loader
supports the Core, JSON, and Failsafe schemas.

The supported implementation is SBCL. Runtime dependencies are cl-regex-kit
v2.2.0 and cl-codec-kit v0.6.0. Octet input uses automatic UTF-8, UTF-16, and
UTF-32 detection, including BOM-less input.

## Install

Load the `cl-yaml-kit` ASDF system from a directory on `CL_SOURCE_REGISTRY`.
The runtime dependencies are `cl-regex-kit` and `cl-codec-kit`. The test system
also uses `cl-weave` and `cl-json-kit`.

```lisp
(asdf:load-system "cl-yaml-kit")
```

## Use

```lisp
(gethash "name"
         (yaml-kit:parse
          (let ((text "name: Ada"))
            (make-array (length text) :element-type 'character
                        :initial-contents text))))
;; => "Ada"

(yaml-kit:parse-all
 (let ((text (format nil "--- one~%--- two")))
   (make-array (length text) :element-type 'character :initial-contents text)))
;; => ("one" "two")

(yaml-kit:emit (list 1 2 3))
;; =>
;; - 1
;; - 2
;; - 3

(yaml-kit:map-events
 (lambda (event) (declare (ignore event)))
 (let ((text "answer: 42"))
   (make-array (length text) :element-type 'character :initial-contents text)))
;; => NIL
```

See [`docs/src/getting-started.md`](docs/src/getting-started.md) for input
types, schemas, mappings, events, and error handling.

## Development

See [`docs/src/project/development.md`](docs/src/project/development.md) for
the development environment and test commands.

## License

MIT. See [`LICENSE`](LICENSE).
