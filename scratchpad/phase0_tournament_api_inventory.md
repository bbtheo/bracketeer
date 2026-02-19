# Phase 0 Inventory: Tournament API Exports and Methods

Date: 2026-02-18

## Exported Tournament-Related Functions

| Symbol | Type | Status | Planned Name / Note |
|---|---|---|---|
| `spec` | constructor | keep | `spec` |
| `build` | constructor | keep | `build` |
| `tournament` | constructor | keep | `tournament` |
| `round_robin` | stage verb + bracket constructor | rename | stage verb stays `round_robin`; bracket constructor moves to `new_round_robin_bracket` (pending) |
| `single_elim` | stage verb + bracket constructor | rename | stage verb stays `single_elim`; bracket constructor moves to `new_single_elim_bracket` (pending) |
| `double_elim` | stage verb + bracket constructor | rename | stage verb stays `double_elim`; bracket constructor moves to `new_double_elim_bracket` (pending) |
| `swiss` | stage verb + bracket constructor | rename | stage verb stays `swiss`; bracket constructor moves to `new_swiss_bracket` (pending) |
| `two_leg` | stage verb alias | keep | `two_leg` |
| `group_stage_knockout` | stage verb + bracket constructor | rename | stage verb stays `group_stage_knockout`; bracket constructor moves to `new_group_stage_knockout_bracket` (pending) |
| `result` | runtime verb | rename | signature changes to `result(trn, stage, match, score)` (pending) |
| `advance` | runtime verb | keep | `advance` |
| `set_result` | mixed legacy/runtime | remove | bracket-level behavior retained; tournament-level entry replaced by `result` |
| `set_winner` | bracket-level | keep | bracket-level unchanged |
| `get_matches` | mixed legacy | rename | tournament-level replaced by `matches` (pending) |
| `get_standings` | mixed legacy | rename | tournament-level replaced by `standings` (pending) |
| `get_winner` | mixed legacy | rename | tournament-level replaced by `winner` (pending) |
| `is_complete` | bracket-level | keep | bracket-level unchanged |
| `is_stage_complete` | legacy runtime helper | remove | replaced by `stage_status` row checks (pending) |
| `get_ready_stages` | legacy runtime helper | remove | replaced by `stage_status` filtering (pending) |
| `get_routing_log` | legacy runtime helper | rename | replaced by `routing_log` (pending) |
| `export_matches` | legacy export helper | rename | folded into `matches` (pending) |
| `export_standings` | legacy export helper | rename | folded into `standings` (pending) |
| `export_tournament_log` | legacy export helper | rename | folded into `routing_log` (pending) |
| `qualify_remaining` | transition rule helper | rename | `remaining` selector (pending) |
| `qualify_losers` | transition rule helper | rename | `losers` selector (pending) |
| `validate_tournament` | validation | rename | unified `validate` (pending) |
| `validate_tournament_spec` | validation | rename | unified `validate` (pending) |
| `tournament_spec` | legacy spec constructor | rename | `spec` |
| `add_stage` | legacy graph API | remove | replaced by stage verbs |
| `add_transition` | legacy graph API | remove | replaced by `from`/`take` on stage verbs |
| `split_stage` | legacy graph API | remove | replaced by explicit branching with stage verbs |
| `from_previous` | sentinel | rename | `previous_stage` |
| `previous_stage` | sentinel | keep | `previous_stage` |
| `set_outcome` | legacy spec modifier | remove | folded into `spec(..., track_placements=)` or dropped |

## Exported S3 Methods (Tournament-Related)

| Method | Status | Note |
|---|---|---|
| `*.bracketeer_spec` stage-verb methods (`round_robin`, `single_elim`, `double_elim`, `swiss`, `group_stage_knockout`, `two_leg_knockout`) | keep | new construction flow |
| `*.tournament` stage-verb methods (same set) | keep | pipe sugar over rebuild |
| `add_stage.tournament_spec` | remove | legacy graph API |
| `add_transition.tournament_spec` | remove | legacy graph API |
| `split_stage.tournament_spec` | remove | legacy graph API |
| `build_tournament.tournament_spec` | rename | `build` |
| `set_result.tournament` | rename | `result` |
| `advance.tournament` | keep | `advance` |
| `get_ready_stages.tournament` | remove | replaced by `stage_status` |
| `get_routing_log.tournament` | rename | `routing_log` |
| `is_stage_complete.tournament` | remove | replaced by `stage_status` |
| `get_winner.tournament` | rename | `winner` |
| `print.tournament_spec` | rename | `print.bracketeer_spec` target surface (pending) |

## Notes

- This inventory is a Phase 0 decision artifact only; removal/rename execution is scheduled in Phase 1+.
- Bracket-level generics (`set_result.bracket`, `get_matches.bracket`, `get_standings.bracket`, `get_winner.bracket`, `is_complete`) remain until the planned constructor rename is completed.
