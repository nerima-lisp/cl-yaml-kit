# Architecture

cl-yaml-kit follows the YAML 1.2.2 pipeline in both directions.

```text
characters -> scanner -> tokens -> parser -> events -> composer -> nodes
nodes -> constructor -> Common Lisp values
Common Lisp values -> representer -> serializer -> emitter -> characters
```

The reader stages are owned by the scanner and parser files. `src/events.lisp`
defines the event contract. `src/composer.lisp` builds representation graphs
from the node types in `src/nodes.lisp`; schema and constructor files turn
those graphs into Common Lisp values. The reverse path is owned by the
representer, serializer, emitter, and dumper files.

Events model stream, document, sequence, mapping, scalar, and alias actions.
They expose start and end spans. Nodes carry tag, anchor, style, and start and
end spans where source locations are available. YAML conditions store a
`cl-parser-kit:diagnostic`, which can carry a span and structured data.

Data values and types live in `data.lisp`, `events.lisp`, `nodes.lisp`, and
`tokens.lisp`. Schema tables are in `schema.lisp`, and character-class tables
are in `char-classes.lisp`; pipeline behavior lives in the stage-specific
files.

Event delivery uses continuation-passing style (CPS): each stage accepts the
next stage as a continuation and sends each produced item to it. This permits
streaming consumers and keeps parser, composer, and emitter boundaries
independently testable.
