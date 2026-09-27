# Pipeline

The reader direction is scanner, parser, events, composer, nodes, and
constructor. The writer direction is representer, serializer, and emitter.
See the [architecture reference](../reference/architecture.md) for ownership.

## Input encoding

Reader entry points accept strings, streams, and octet vectors. Octet vectors
are decoded with cl-codec-kit's `:auto` mode: BOMs select UTF-8, UTF-16, or
UTF-32 byte order, and BOM-less UTF-16/UTF-32 input is selected from its null
byte pattern. Invalid or truncated octet sequences become `yaml-parse-error`.
