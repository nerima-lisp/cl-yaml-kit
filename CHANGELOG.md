# Changelog

All notable changes to cl-yaml-kit are documented here.

## [0.1.0] - 2026-09-28

### Added

- YAML 1.2.2 reader, loader, representer, serializer, and dumper.
- Scanner, parser-event, representation-node, and value APIs.
- Core, JSON, and Failsafe schema selection.
- UTF-8, UTF-16, and UTF-32 octet input with BOM and BOM-less automatic
  detection.
- Resource limits for input length, parser depth, scalar length, node count,
  and alias expansion count.
- Structured parse, compose, emit, and resource-limit conditions with source
  coordinates where available.
- Ordered `yaml-mapping` values and configurable mapping, sequence, and
  duplicate-key representations.

### Compatibility

- SBCL is the supported Common Lisp implementation.
- Runtime dependencies are cl-regex-kit v2.2.0 and cl-codec-kit v0.6.0.

### Conformance

The yaml-test-suite `data-2022-01-17` run passed 333/333 reader cases,
231/231 loader-isolated cases, 231/231 loader end-to-end cases, 31/31
emitter-isolated cases, and 204/213 dumper end-to-end cases. The dumper stage
reported no failures, exclusions, or drift.

[0.1.0]: https://github.com/nerima-lisp/cl-yaml-kit/releases/tag/v0.1.0
