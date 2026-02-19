# bracketeer NEWS

## Tournament API Rewrite (Planned Surface)

### New primary workflow

- Tournament construction now centers on `spec()`, `build()`, and `tournament()`.
- Stage wiring is done directly by stage verbs in pipe flow:
  `round_robin()`, `single_elim()`, `double_elim()`, `swiss()`, `two_leg()`,
  `group_stage_knockout()`.
- Stage routing now uses `take =` selectors (`top_n()`, `remaining()`, `losers()`,
  grouped variants) instead of standalone transition helper workflows.
- Tournament result entry uses `result(trn, stage, match, score = c(x, y))` and
  `results(trn, stage, df)` (`match`, `score1`, `score2` columns).
- Inspection now uses noun helpers: `matches()`, `standings()`, `stage_status()`,
  `winner()`, `rankings()`, `routing_log()`.

### Removed/renamed legacy tournament APIs

- `tournament_spec()` -> `spec()`
- `build_tournament()` -> `build()`
- `validate_tournament()` -> `validate()`
- `from_previous()` -> `previous_stage()`
- `qualify_remaining()` -> `remaining()`
- `qualify_losers()` -> `losers()`
- `get_routing_log()` -> `routing_log()`
- Tournament-level direct exports:
  `export_matches()` -> `matches()`,
  `export_standings()` -> `standings()`,
  `export_tournament_log()` -> `routing_log()`
- Legacy graph-construction style (`add_stage()`, `add_transition()`, `split_stage()`)
  is replaced in tests and docs by explicit stage-verb pipeline wiring.

## Breaking Changes

- Removed legacy spec APIs: `bracket_spec()`, `single_elim_spec()`, `double_elim_spec()`,
  `round_robin_spec()`, `swiss_spec()`, `group_stage_knockout_spec()`,
  `two_leg_knockout_spec()`, `build_bracket()`, `fit_bracket()`, `update_spec()`, and
  `print.bracket_spec()`.
- Tournament graph stages now use stage constructors only:
  `single_elim_stage()`, `double_elim_stage()`, `round_robin_stage()`,
  `swiss_stage()`, `group_stage_knockout_stage()`, and `two_leg_stage()`.
- `tournament_spec()` no longer inherits `bracket_spec`.
- `bracket()` no longer accepts spec/stage objects as the first argument.

## Replacement Table

- `single_elim_spec()` -> `single_elim_stage()`
- `double_elim_spec()` -> `double_elim_stage()`
- `round_robin_spec()` -> `round_robin_stage()`
- `swiss_spec()` -> `swiss_stage()`
- `group_stage_knockout_spec()` -> `group_stage_knockout_stage()`
- `two_leg_knockout_spec()` -> `two_leg_stage()`
