# Architecture

cl-yaml-kit follows the YAML 1.2.2 pipeline in both directions.

```text
characters -> scanner -> tokens -> parser -> events -> composer -> nodes
             -> constructor -> Common Lisp values
Common Lisp values -> representer -> serializer -> emitter -> characters
```

The reader stages are owned by `src/reader-macros.lisp`, the character and
scanner files, and the parser files. `src/events.lisp` defines the event
contract. `src/composer.lisp` builds representation graphs in the node types
from `src/nodes.lisp`; schema and constructor files turn those graphs into
Common Lisp values. The reverse path is owned by the representer, serializer,
emitter, and dumper files.

Events model stream, document, sequence, mapping, scalar, and alias actions.
Every event carries start and end marks. Nodes model scalar, sequence, and
mapping values and share tag, anchor, style, and source-mark metadata.

Data tables and types live in `data.lisp`, `events.lisp`, and `nodes.lisp`;
pipeline behavior lives in the stage-specific files. This separation keeps
contracts inspectable without loading implementation logic.

Event delivery uses continuation-passing style (CPS): each stage accepts the
next stage as a continuation and sends each produced item to it. This permits
streaming consumers and keeps parser, composer, and emitter boundaries
independently testable.
