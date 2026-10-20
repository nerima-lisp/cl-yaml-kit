# Benchmarking

Run the benchmark from the repository root:

```sh
nix run .#benchmark
```

`benchmark/run.lisp` generates deterministic YAML inputs and measures the
reader (`parse-events`), loader (`parse`), and dumper (`emit`) for each corpus
case. It prints one TSV row per case and stage. The columns report case, stage,
status, input size in bytes, throughput in MiB/s, average bytes consed per
operation, and `gc_seconds`, elapsed time for the forced full GC and the
measurement call together. This is not the GC duration alone.

`BENCH_WARMUP`, `BENCH_SAMPLES`, and `BENCH_ITERATIONS` control measurement
settings. These variables control corpus sizes:

| Variable | Corpus size |
| --- | --- |
| `BENCH_BLOCK_MAPPING_ITEMS` | Entries in the large block mapping |
| `BENCH_BLOCK_SEQUENCE_ITEMS` | Items in the large block sequence |
| `BENCH_DEEP_DEPTH` | Nesting depth |
| `BENCH_LONG_SCALAR_LENGTH` | Length of each long scalar case |
| `BENCH_FLOW_ITEMS` | Items in the flow-collection case |
| `BENCH_ANCHOR_ITEMS` | Items in the anchor and alias case |
| `BENCH_CONFIG_BYTES` | Target size of the realistic configuration case |

After the corpus is built and parsed, a failed measurement stage produces an
`error` row; the runner continues with the remaining stages and exits nonzero
if any stage failed. A failure while building or initially parsing the corpus
aborts before measurement rows are produced.

The flake exposes the benchmark as a separate app. `nix flake check` does not
apply a timing threshold to its output.
