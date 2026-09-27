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

## Performance and coverage policy

`benchmark/run.lisp` generates six deterministic corpus cases: large block
mapping, large block sequence, deep nesting, long scalar, anchor-heavy input,
and multi-document input. It measures every case through the public
`yaml-kit:parse`, `yaml-kit:emit`, and `yaml-kit:map-events` paths. Each TSV row
reports the case, operation, input size, throughput in MB/s, bytes consed, and
the time used by the full GC immediately before the sample. Timing uses
`cl-weave:measure`; warmups and sample counts are controlled by
`BENCH_WARMUP`, `BENCH_SAMPLES`, and `BENCH_ITERATIONS`. Case sizes are
controlled by the `BENCH_*` variables in the script. The TSV is observational,
not a CI timing gate: compare runs only with the same implementation, runtime,
corpus settings, and machine conditions. Unsupported case/operation pairs are
reported as `error` rows and do not suppress the remaining measurements.

Coverage is collected by `scripts/run-coverage.lisp`, which forces compilation
with SB-COVER instrumentation before invoking the suite entry point that the
plain test run also reaches. Validate the result with
`scripts/check-coverage.pl --input TSV --min-line PERCENT --min-branch
PERCENT`, which reports per-file line and branch coverage plus the uncovered
lines. Coverage thresholds are quality gates; benchmark timings are not. A
partial implementation remains measurable when its public entry points and
corpus preflight are available, so failures in one operation should not be
hidden by changing the corpus or the measurement unit.
