# Migration Map (Phase 0)

Date: 2026-02-18

| Old API | Planned API | Migration State |
|---|---|---|
| `tournament_spec()` | `spec()` | implemented |
| `build_tournament(spec, teams)` | `build(spec, teams)` | implemented |
| `tournament(participants, ...)` | `tournament(participants, ...)` | implemented |
| `add_stage(spec, "id", round_robin_stage(...))` | `round_robin(spec, "id", ...)` | implemented (core), selector wiring pending |
| `add_stage(spec, "id", single_elim_stage(...))` | `single_elim(spec, "id", ...)` | implemented (core), selector wiring pending |
| `add_stage(spec, "id", double_elim_stage(...))` | `double_elim(spec, "id", ...)` | implemented (core), selector wiring pending |
| `add_stage(spec, "id", swiss_stage(...))` | `swiss(spec, "id", ...)` | implemented (core), selector wiring pending |
| `add_stage(spec, "id", two_leg_stage(...))` | `two_leg(spec, "id", ...)` | implemented (alias path), selector wiring pending |
| `add_stage(spec, "id", group_stage_knockout_stage(...))` | `group_stage_knockout(spec, "id", ...)` | implemented (core), selector wiring pending |
| `from_previous()` | `previous_stage()` | implemented (alias) |
| default implicit prior-stage wiring via legacy edge calls | omit `from` in stage verbs | implemented |
| `add_transition(from, to, rule=...)` | stage verb `from=` + `take=` | partial (`from` works; selector objects pending) |
| `split_stage(...)` | explicit multiple stage verbs with `from=` | pending |
| `qualify_remaining()` | `remaining()` | pending |
| `qualify_losers(...)` | `losers(...)` | pending |
| `result(trn, stage, match_id, score1, score2)` | `result(trn, stage, match, score=c(x,y))` | pending |
| `set_result(trn, ..., stage_id=...)` | `result(trn, ...)` | pending |
| `get_matches(trn$stage_state$s$bracket)` | `matches(trn, "s")` | pending |
| `get_standings(trn$stage_state$s$bracket)` | `standings(trn, "s")` | pending |
| `get_winner(trn)` | `winner(trn)` | pending |
| `get_routing_log(trn)` | `routing_log(trn)` | pending |
| `validate_tournament(spec, n)` + `validate_tournament_spec(spec)` | `validate(spec, n)` | pending |

## Phase 0 Decision Lock

- Standalone bracket constructors are dropped from the public API target surface.
- During migration window they still exist for compatibility with existing tests; Phase 1/7a removes public exposure in favor of `new_*_bracket()` internals.
