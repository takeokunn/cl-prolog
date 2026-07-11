# Performance

`cl-prolog` ships a small benchmark harness for same-machine regression checks.

It is not a cross-machine scoreboard. Use it to compare before/after changes in
the same environment.

## Commands

```sh
sbcl --script scripts/benchmark.lisp --help
sbcl --script scripts/benchmark.lisp --version
sbcl --script scripts/benchmark.lisp
sbcl --script scripts/benchmark.lisp --json --scenario ancestor-first --iterations 500
```

The benchmark runner loads its support code directly from
`scripts/bootstrap.lisp`, so it works from a plain checkout without an ASDF
system definition.

## Scenarios

- `ancestor-first`: recursive proof search over an immutable family rulebase
- `append-first`: first-solution list relation through the built-in proof path
- `dcg-phrase`: minimal DCG parse over a single token stream

These scenarios cover three distinct hot paths:

- recursive rule expansion
- variable binding and list processing
- grammar expansion plus parsing

## Output

Each scenario reports:

- `scenario`
- `ok`
- `iterations`
- `total_ns`
- `avg_ns`
- `last_result`

JSON mode additionally emits:

- `report_type`
- `project_version`
- `requested_scenarios`
- `scenario_count`
- top-level `ok`

`last_result` is intentionally preserved as a printed Lisp representation so
benchmark smoke can also detect semantic drift.

## Release Usage

The release audit can include a benchmark smoke pass:

```sh
sbcl --script scripts/release-audit.lisp --with-benchmarks
```

No hard performance threshold is enforced. The benchmark layer exists to make
behavioral and performance regressions visible, not to gate on absolute numbers.
