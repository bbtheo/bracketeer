# bracketeer 0.1.0

First release.

## Tournament construction

- `tournament(participants, auto_advance = TRUE)` creates a live tournament
  runtime ready for result entry.
- `spec()` creates a reusable blueprint without participants; materialize it
  with `build(spec, participants)`.
- `validate(spec, n)` runs a preflight feasibility check before building.

## Stage verbs

Pipe stage types onto `tournament()` or `spec()` to define the competition
structure:

- `round_robin(id, ...)` — round-robin stage; supports `groups =` for
  parallel group play.
- `single_elim(id, ...)` — single-elimination bracket.
- `double_elim(id, ...)` — double-elimination bracket.
- `swiss(id, ...)` — Swiss-system stage.
- `two_leg(id, ...)` — two-leg knockout stage.
- `group_stage_knockout(id, ...)` — combined group stage and knockout.

Each verb accepts `from = previous_stage()` (implicit in linear chains;
explicit `from =` required only when branching) and `take =` for routing.

## Routing selectors

Selectors sit in `take =` and resolve against the source stage's standings
at transition time:

- `top_n(n)`, `bottom_n(n)`, `slice_range(from, to)` — flat ranking cuts.
- `top_per_group(n)`, `bottom_per_group(n)`, `slice_per_group(from, to)` —
  per-group cuts; require a grouped source stage.
- `remaining()` — participants not yet consumed by a prior transition.
- `losers()` — eliminated participants.
- `filter_by(fn)` — custom predicate on the standings data frame.

## Result entry

- `result(trn, stage, match, score = c(x, y))` enters a single match result.
  For best-of series, pass per-game scores as a longer vector.
- `results(trn, stage, df)` batch-enters results from a data frame with
  columns `match`, `score1`, `score2`.
- Auto-advance is on by default: when the last match in a stage is entered,
  all downstream stages materialize automatically. Disable with
  `auto_advance = FALSE` and trigger manually with `advance(trn, stage)`.
- `teardown(trn, stage)` un-materializes a stage and all downstream
  dependents, preserving source-stage results.

## Inspection

- `matches(trn, stage, status)` — match table, filterable by stage and
  status (`"pending"`, `"complete"`, `"all"`).
- `standings(trn, stage)` — standings table for one or all stages.
- `stage_status(trn)` — per-stage overview with columns `stage`, `status`,
  `complete`, `total`, `materialized`.
- `winner(trn)` — tournament winner, or `NA` if incomplete.
- `rankings(trn)` — final placement table.
- `routing_log(trn)` — transition audit trail.
