# cl-yaml-kit v0.1.0

cl-yaml-kit is a Common Lisp implementation of YAML 1.2.2 with reader,
loader, representer, serializer, and dumper APIs.

## Included

- Core, JSON, and Failsafe schema selection.
- Scanner, parser-event, representation-node, and value-level APIs.
- YAML null and false sentinels, ordered mappings, aliases, and configurable
  mapping and sequence representations.
- UTF-8, UTF-16, and UTF-32 octet input, with automatic BOM and BOM-less
  encoding detection.
- Resource limits for input length, parser depth, scalar length, node count,
  and alias expansion count.
- Structured parse, compose, emit, and resource-limit conditions with source
  coordinates where available.

## Compatibility and dependencies

- Supported implementation: SBCL.
- cl-regex-kit v2.2.0.
- cl-codec-kit v0.6.0.

## yaml-test-suite result

Using `data-2022-01-17`:

| Stage | Passed | Applicable |
| --- | ---: | ---: |
| Reader | 333 | 333 |
| Loader isolated | 231 | 231 |
| Loader end-to-end | 231 | 231 |
| Emitter isolated | 31 | 31 |
| Dumper end-to-end | 204 | 213 |

The dumper end-to-end stage reported 0 failures, 0 exclusions, and 0 drift.
