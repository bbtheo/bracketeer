# Tournament API Rewrite TODO (Plan-Only)

## Rewrite Goal
- [x] Replace the current tournament API with the plan in `scratchpad/tournament_api.qmd`.
- [x] Remove legacy tournament DSL/public helpers that are not part of the planned surface.
- [x] Keep base R + S3 style, functional returns, and deterministic behavior.
- [x] Treat this as a full rewrite (no backward-compat shims unless explicitly approved).

## Guardrails (Always)
- [x] Follow test-first workflow for every functional change (add/update failing tests, then implement).
- [x] Never edit `NAMESPACE` or `man/*.Rd` by hand; update roxygen in `R/*.R` and run `devtools::document()`.
- [x] Keep function and argument names in snake_case.
- [x] Keep error messages actionable with IDs (`stage_id`, `transition_id`, `match_id`) where relevant.
- [x] `spec()` returns class `bracketeer_spec`; `tournament()` and `build()` return class `c("tournament", "bracketeer_spec")`.
- [x] Stage verbs dispatch on `.bracketeer_spec` only and are purely structural (record stage/edge definitions, no materialization).
- [x] Runtime and inspection verbs dispatch on `.tournament` only.
- [x] Materialization occurs at `build()` time for spec path, and at runtime/inspection execution for tournament path.
- [x] Every public function validates inputs at entry and errors with expected/actual details (types, IDs, score shape, participant uniqueness, positive integer counts).

## API Translation Reference

Every test rewrite and implementation follows this mapping:

| Old | New | Notes |
|-----|-----|-------|
| `single_elim(teams)` | `new_single_elim_bracket(teams)` | Renamed, unexported. Bracket-level tests only. |
| `double_elim(teams)` | `new_double_elim_bracket(teams)` | Same. |
| `round_robin(teams)` | `new_round_robin_bracket(teams)` | Same. |
| `swiss(teams)` | `new_swiss_bracket(teams)` | Same. |
| `two_leg_knockout(teams)` | `new_two_leg_bracket(teams)` | Same. |
| `group_stage_knockout(teams)` | `new_group_stage_knockout_bracket(teams)` | Same. |
| `bracket(teams, type)` | `new_bracket(teams, type)` | Internal dispatcher, renamed. |
| `tournament_spec()` | `spec()` | |
| `add_stage(spec, "id", round_robin_stage())` | `round_robin(spec, "id", ...)` | Stage verb absorbs stage-type params. |
| `add_stage(spec, "id", single_elim_stage())` | `single_elim(spec, "id", ...)` | Same pattern for all formats. |
| `add_transition(from, to, rule = fn)` | Implicit via `from =` / `take =` on stage verbs | No standalone edge function. |
| `split_stage(from, into = list(...))` | Multiple stage verbs with explicit `from =` | |
| `from_previous()` | `previous_stage()` | Sentinel rename. |
| `set_outcome(spec, track_placements = n)` | `spec(..., track_placements = n)` or dropped | Decide during implementation. |
| `validate_tournament_spec(spec)` | `validate(spec, n)` | Merged with dry-run validation. |
| `validate_tournament(spec, n)` | `validate(spec, n)` | |
| `build_tournament(spec, teams)` | `build(spec, teams)` | |
| `set_result(b, match, score1, score2)` | `set_result(b, match, score1, score2)` | **Unchanged** at bracket level. |
| `set_result(trn, match, score1, score2, stage_id =)` | `result(trn, stage, match, score = c(x, y))` | Tournament-level only. |
| `result(trn, stage, match, score1, score2)` | `result(trn, stage, match, score = c(x, y))` | Signature change. |
| `set_winner(b, match, winner)` | `set_winner(b, match, winner)` | **Unchanged** at bracket level. |
| `advance(trn, stage)` | `advance(trn, stage)` | Same; rarely needed with auto-advance default. |
| `get_matches(bracket)` | `get_matches(bracket)` | **Unchanged** at bracket level. |
| `get_matches(trn$stage_state$s$bracket)` | `matches(trn, "s")` | Tournament-level. |
| `get_standings(bracket)` | `get_standings(bracket)` | **Unchanged** at bracket level. |
| `get_standings(trn$stage_state$s$bracket)` | `standings(trn, "s")` | Tournament-level. |
| `get_winner(bracket)` | `get_winner(bracket)` | **Unchanged** at bracket level. |
| `get_winner(trn)` | `winner(trn)` | Tournament-level. |
| `is_complete(bracket)` | `is_complete(bracket)` | **Unchanged** at bracket level. |
| `is_stage_complete(trn, stage)` | `stage_status(trn)` row check | Folded into stage_status. |
| `get_ready_stages(trn)` | `stage_status(trn)` filtered | Folded into stage_status. |
| `get_routing_log(trn)` | `routing_log(trn)` | |
| `qualify_remaining()` | `remaining()` | Returns `bracketeer_selector`. |
| `qualify_losers(...)` | `losers(...)` | Returns `bracketeer_selector`. |
| `export_matches(trn)` | `matches(trn)` (`stage = NULL`) | |
| `export_standings(trn)` | `standings(trn)` (`stage = NULL`) | |
| `export_tournament_log(trn)` | `routing_log(trn)` | |

## Phase 0: Freeze, Inventory, and Decisions
- [x] Inventory all exported tournament-related functions and S3 methods.
- [x] Mark each function as: keep (planned), rename (planned name), or remove (unplanned).
- [x] Add snapshot tests of current behavior only where needed to protect invariants that remain in the new API.
- [x] Create a migration map file in `scratchpad/` from old names to planned names.
- [x] Decision locked: standalone bracket constructors (`single_elim(teams)` without a tournament) are dropped from the public API.

## Phase 1: Remove Legacy API Surface
- [x] Remove legacy tournament entry points not in the plan.
- [x] Remove `add_transition()` and `split_stage()` from public API.
- [x] Remove legacy routing/inspection helpers that conflict with planned names/semantics.
- [x] Remove ambiguous single-entry score signatures that rely on separate score fields.
- [x] Remove legacy stage-wiring patterns that conflict with `from = previous_stage()` default semantics.
- [x] Update tests to stop asserting removed behavior.

## Phase 1.5: Internal Architecture Alignment

### Spec graph (`tournament_spec.R`)
- [x] Refactor edge creation so stage verbs call internal `register_edge()` from `from =` and `take =`.
- [x] Keep `spec$edges` and `transition_spec`; change only edge creation entrypoint (stage verbs, not public transition API).
- [x] Keep transition ID generation and duplicate detection logic from existing internal code.
- [x] Store `take =` on `transition_spec` as selector object (`bracketeer_selector`); default to all participants when `take` is missing.
- [x] Align previous-stage sentinel naming/behavior with `previous_stage()`.

### Materialization and advance (`tournament_runtime.R`)
- [x] Refactor transition invocation to support both legacy rule functions and new selector objects during migration.
- [x] Flip `auto_advance` default in result flow from `FALSE` to `TRUE`.
- [x] After result state refresh, auto-call `advance()` when a stage transitions to complete and auto-advance is enabled.
- [x] Keep `materialize_stage()` and `stage$builder(participants)` pattern unchanged.
- [x] Keep reusable internals unchanged where possible: topological ordering, transition participant resolution, compound match ID parsing, pending pools.

### Selectors (`tournament_rules.R`)
- [x] Wrap existing `qualify_remaining()` and `qualify_losers()` logic as selector-object factories for `remaining()` and `losers()`.
- [x] Implement selector constructors for `top_n`, `bottom_n`, `slice_range`, `filter_by`, `top_per_group`, `bottom_per_group`, `slice_per_group`.
- [x] Keep fallback support for user-supplied rule functions via `filter_by(fn)` wrapping.

### Result entry and helpers (`bracket.R`, `tournament_helpers.R`)
- [x] Change `result()` signature to `(trn, stage, match, score)` and adapt internally to existing per-bracket `set_result` machinery.
- [x] Implement `teardown.tournament()` to un-materialize a stage and downstream dependents.

### Validation (`tournament_validation.R`)
- [x] Extend preflight transition selection to evaluate selector objects against mock standings/context, not just legacy rules.

### Keep unchanged
- [x] Bracket-level generation/scheduling/seeding/tiebreaker internals.
- [x] Utility helpers: participant normalization, seeding math, tiebreaker computation.
- [x] Bracket print methods; tournament print method added separately in Phase 5.
- [x] All bracket-level tests must keep passing throughout the rewrite.

## Phase 2: New Core Construction API
- [x] Implement `spec()` returning a `bracketeer_spec` object with empty stage/edge graph.
- [x] Implement `build(spec, participants)` to materialize a spec into a live `tournament` object.
- [x] Implement `tournament(participants, auto_advance = TRUE)` as construction sugar for the planned pipeline flow.
- [x] Implement stage verbs that work in the same pipe for spec/runtime:
  - [x] `round_robin(x, id, ...)`
  - [x] `single_elim(x, id, ...)`
  - [x] `double_elim(x, id, ...)`
  - [x] `swiss(x, id, ...)`
  - [x] `two_leg(x, id, ...)`
  - [x] `group_stage_knockout(x, id, ...)`
- [x] Implement `round_robin(..., groups = )` for grouped parallel play.
- [x] Implement `previous_stage()` sentinel and default `from = previous_stage()` resolution.
- [x] Enforce explicit `from =` for branching from non-immediate prior stage.
- [x] Implement `take =` on all stage verbs by storing an unevaluated selector object on edges (evaluation deferred).
- [x] Add input validation for all new public constructors/verbs.

## Phase 3: Routing Helpers (Planned Set Only)
- [x] Implement flat selectors:
  - [x] `top_n(n)`, `bottom_n(n)`, `slice_range(from, to)`, `remaining()`, `losers()`, `filter_by(fn)`.
- [x] Implement grouped selectors:
  - [x] `top_per_group(n)`, `bottom_per_group(n)`, `slice_per_group(from, to)`.
- [x] Wire selector evaluation into `build()` materialization and `advance()` so stored `take =` resolves against source standings at transition time.
- [x] Validate grouped selectors error clearly when source stage has no groups.
- [x] Ensure deterministic participant ordering for all selector results.
- [x] Add input validation for selector constructors.

## Phase 4: Runtime Result/Advance API
- [x] Implement `result(trn, stage, match, score)`.
- [x] Support score vectors for both single-match (`c(x, y)`) and best-of per-game scoring shapes.
- [x] Implement `results(trn, stage, df)` batch entry — Option B: df columns are `match`, `score1`, `score2`. Document why wide columns are used here vs. single-entry vector.
- [x] Implement `advance(trn, stage)` for manual mode.
- [x] Keep auto-advance default behavior; `auto_advance = FALSE` enables explicit manual advance.
- [x] Overwrite policy:
  - [x] Allow overwrite before downstream materialization; recalculate standings.
  - [x] Block overwrite after downstream materialization with actionable error including blocking downstream stage ID.
- [x] Implement `teardown(trn, stage)` runtime verb:
  - [x] Cascade un-materialization from target stage through all downstream dependents following the DAG.
  - [x] Reset downstream stage states to blocked/unmaterialized and clear dependent routing artifacts.
  - [x] Preserve source-stage results/standings while clearing advanced/materialized flags as needed.
  - [x] Add `teardown()` to the Runtime verbs section of the API surface alongside `result`, `results`, `advance`.
- [x] Add input validation for runtime verbs.

## Phase 5: Inspection API (Planned Nouns)
- [x] Implement `matches(trn, stage = NULL, status = "pending"|"complete"|"all")`.
- [x] Implement `standings(trn, stage = NULL)`.
- [x] Implement `stage_status(trn)`.
- [x] Ensure `stage_status()` correctly reflects teardown/reset states (torn-down stages revert to `"blocked"`).
- [x] Implement `print.tournament()` summary output aligned with planned status presentation.
- [x] Implement `winner(trn)`.
- [x] Implement `rankings(trn)`.
- [x] Implement `routing_log(trn)`.
- [x] Add input validation for inspection helpers.

## Phase 6: Spec Validation and DAG Checks
- [x] Implement `validate(spec, n)` preflight feasibility checks.
- [x] Validate DAG consistency: cycle detection, unreachable stages, fan-in conflicts.
- [x] Validate transition feasibility from selector intent (e.g., impossible `take = top_n(8)` from 4 entrants).
- [x] Ensure `build()` implicitly calls `validate()` before materialization.
- [x] Keep validation errors identifier-rich and repair-oriented.

## Phase 7: Tests

### 7a: Bracket-level tests — constructor rename only (15 files)

These test format-specific behavior in isolation. The bracket internals don't change;
only the direct constructor name changes because old public names are repurposed as stage
verbs. For each file, find-and-replace the constructor call; all other assertions stay
identical.

- [x] `test-single-elim.R`: `single_elim(teams)` → `new_single_elim_bracket(teams)`
- [x] `test-round-robin.R`: `round_robin(teams, ...)` → `new_round_robin_bracket(teams, ...)`
- [x] `test-swiss.R`: `swiss(teams, ...)` → `new_swiss_bracket(teams, ...)`
- [x] `test-double-elim.R`: `double_elim(teams)` → `new_double_elim_bracket(teams)`
- [x] `test-double-elim-reseed.R`: `double_elim(teams, reseed = TRUE)` → `new_double_elim_bracket(teams, reseed = TRUE)`
- [x] `test-group-stage.R`: `group_stage_knockout(teams, ...)` → `new_group_stage_knockout_bracket(teams, ...)`
- [x] `test-best-of.R`: `single_elim(teams, best_of = 3)` → `new_single_elim_bracket(teams, best_of = 3)`
- [x] `test-tiebreakers.R`: `round_robin(teams, ...)` → `new_round_robin_bracket(teams, ...)`
- [x] `test-two-leg-knockout.R`: `two_leg_knockout(teams)` → `new_two_leg_bracket(teams)`
- [x] `test-two-leg-away-goals.R`: `two_leg_knockout(teams, away_goals = TRUE)` → `new_two_leg_bracket(teams, away_goals = TRUE)`
- [x] `test-edge-cases.R`: all six format constructors + `bracket()` → `new_*_bracket()` equivalents
- [x] `test-edge-cases-2.R`: same pattern
- [x] `test-edge-cases-3.R`: same pattern
- [x] `test-stage-constructors.R`: if `*_stage()` helpers stay internal, un-export only; if inlined into stage verbs, delete and cover parameter validation in stage verb tests
- [x] `test-s3-dispatch-public-api.R`: split into bracket dispatch (constructor rename) and tournament dispatch (rewrite to new API per 7b below)

### 7b: Tournament API tests — full rewrites (16 files)

All files use the old spec-graph API (`tournament_spec()`, `add_stage()`, `add_transition()`,
etc.). Rewrite construction and operation to the pipe API; keep assertions on behavior.

**Common pattern:**
```r
# OLD
spec <- tournament_spec() |>
  add_stage("groups", round_robin_stage(home_away = TRUE)) |>
  add_stage("finals", single_elim_stage()) |>
  add_transition("groups", "finals", rule = function(pool, standings, ...) {
    standings$participant[1:4]
  })
trn <- build_tournament(spec, teams)
trn <- result(trn, "groups", match_id = 1, score1 = 2, score2 = 1)
advance(trn, "groups")

# NEW
trn <- tournament(teams) |>
  round_robin("groups", home_away = TRUE) |>
  single_elim("finals", take = top_n(4))
trn <- result(trn, "groups", match = 1, score = c(2, 1))
# no explicit advance() — auto-advance is default
```

- [x] `test-spec-api.R`: `tournament_spec() |> add_stage() |> add_transition() |> build_tournament()` → `spec() |> round_robin() |> single_elim(take = top_n()) |> build(teams)`
- [x] `test-tournament-spec-graph.R`: stage ID uniqueness test unchanged; cycle tests removed (impossible with pipe API) or moved to internal DAG validator; `from_previous()` → `previous_stage()`; spec printing → `print.bracketeer_spec()` snapshot
- [x] `test-tournament-qualify-losers.R`: `qualify_losers(...)` → `losers(...)`; test selector evaluation via `evaluate_selector(sel, source_pool, standings, bracket)` directly; bracket setup uses `new_single_elim_bracket()`
- [x] `test-tournament-validation-dry-run.R`: `validate_tournament(spec, n)` → `validate(spec, n)`; spec construction → pipe API; feasibility assertions unchanged
- [x] `test-world-cup-workflow.R`:
  - FIFA flow: `group_stage_knockout()` monolith → `tournament(teams) |> round_robin("groups", groups = 4) |> single_elim("knockout", take = top_per_group(2))`
  - LoL flow: `tournament_spec() |> add_stage() |> add_transition() |> build_tournament()` → `tournament(teams) |> swiss("swiss", rounds = 5) |> single_elim("top_cut", take = top_n(8))`
  - `result(trn, stage, match_id, score1, score2)` → `result(trn, stage, match = id, score = c(x, y))`
  - Remove all explicit `advance()` calls (auto-advance)
  - `get_routing_log(trn)` → `routing_log(trn)`; `get_winner(trn)` → `winner(trn)`
- [x] `test-tournament-export-helpers.R`: `export_tournament_log()` → `routing_log(trn)`; `export_matches()` → `matches(trn)`; `export_standings()` → `standings(trn)`; construction → pipe API; output shape assertions stay (verify column names match)
- [x] `test-tournament-seeding-policies.R`: seeding moves to stage verb parameter — `add_transition(..., seeding = "cross_group")` → `single_elim(spec, "finals", from = "groups", seeding = "cross_group", take = top_n(8))`; participant order assertions unchanged
- [x] `test-tournament-transition-conflicts.R`: `add_transition(..., consume = TRUE)` → implicit consume via `top_n()` / `remaining()` selectors; `allow_overlap = TRUE` → explicit option on stage verb if kept; conflict assertions stay
- [x] `test-tournament-split-stage.R`: `split_stage(...)` → multiple stage verbs with explicit `from =`; remove transition ID / named-list validation tests; fan-out materialization assertions stay
- [x] `test-tournament-result-alias.R`: `result(trn, stage, match_id, score1, score2)` → `result(trn, stage, match, score = c(x, y))`; rewrite auto-advance test so default is `TRUE` (not opt-in); add test that `auto_advance = FALSE` suppresses
- [x] `test-tournament-branching.R`: full pipe API rewrite; `qualify_remaining()` → `remaining()`; priority assertions → declaration-order determinism assertions; `get_routing_log(trn)` → `routing_log(trn)`
- [x] `test-tournament-routing-log.R`: construction → pipe API; `get_routing_log(trn)` → `routing_log(trn)`; priority-ordering assertions → declaration-order assertions; entry field assertions stay
- [x] `test-tournament-active-stages.R`: `get_ready_stages(trn)` → `stage_status(trn)` filtered; construction → pipe API; auto-advance means "ready stage" tests must use `auto_advance = FALSE` or assert on post-advance `stage_status()`
- [x] `test-tournament-validation-errors.R`: `validate_tournament(spec, n)` → `validate(spec, n)`; construction → pipe API; error message assertions updated if wording changes; error semantics unchanged
- [x] `test-tournament-match-addressing.R`: `set_result(trn, match, score1, score2, stage_id =)` → `result(trn, stage, match, score = c(x, y))`; compound ID tests kept if feature preserved, removed if not
- [x] `test-tournament-stage-transition.R`: construction → pipe API; `is_stage_complete(trn, "s")` → `stage_status(trn)` row; overwrite tests rewritten around new policy (allow before, block after, `teardown()` to unlock); auto-advance tests rewritten under `auto_advance = FALSE` for manual flow, new tests for default auto-advance

### 7c: Cross-cutting / documentation tests (3 files)

- [x] `test-docs-dsl-positioning.R`: update banned-pattern list from old format names (`group_stage_knockout`, `single_elim` as direct constructors) to internal names (`new_*_bracket`); purpose unchanged
- [x] `test-vignettes-catalog.R`: update expected function names — `validate_tournament` → `validate`; `split_stage` → stage verbs; `get_routing_log` → `routing_log`; `get_winner` → `winner`
- [x] `test-docs-tournament-guides.R`: update all 30+ expected function-call patterns to new API names; consider deriving pattern list from API surface table to stay in sync automatically

### 7d: New tests — features with no current coverage

**API construction**
- [x] `spec()` returns `bracketeer_spec` with empty stages and edges.
- [x] `tournament(teams)` returns class `c("tournament", "bracketeer_spec")`.
- [x] Stage verbs work on both `spec()` and `tournament()` pipes (inheritance dispatch).
- [x] `build(spec, teams)` materializes a spec into a live tournament.
- [x] `tournament(teams) |> round_robin("x")` produces a playable tournament with matches.

**`previous_stage()` and `from =`**
- [x] Linear chain: `from =` never needed; resolves via declaration order.
- [x] Branching: explicit `from =` required when two stages read from the same source.
- [x] `from = "nonexistent_stage"` errors with actionable message.
- [x] `previous_stage()` on first stage (no prior) errors clearly.

**`take =` with selectors**
- [x] `take = top_n(n)` selects top n by overall standings.
- [x] `take = bottom_n(n)` selects bottom n by overall standings.
- [x] `take = slice_range(from, to)` selects standings positions from:to.
- [x] `take = remaining()` selects participants not consumed by prior transitions.
- [x] `take = losers(...)` selects eliminated participants with round/ordering options.
- [x] `take = filter_by(fn)` applies custom predicate on standings data frame.
- [x] `take = top_per_group(n)` on grouped source selects top n from each group.
- [x] `take = bottom_per_group(n)` on grouped source selects bottom n from each group.
- [x] `take = slice_per_group(from, to)` selects positions per group.
- [x] `top_per_group()` on non-grouped source errors with clear message.
- [x] All selectors produce deterministic ordering.

**`round_robin(groups = )`**
- [x] `round_robin("groups", groups = 4)` with 16 teams creates 4 groups of 4.
- [x] Standings include group membership column.
- [x] Per-group selectors work correctly on grouped standings.

**Auto-advance**
- [x] Entering last result in a stage auto-materializes all downstream stages.
- [x] Multi-stage linear chain auto-advances through to completion.
- [x] `auto_advance = FALSE` disables auto-advance; explicit `advance()` required.

**`result()` new signature**
- [x] `result(trn, stage, match, score = c(2, 1))` records a result.
- [x] `score` must be a numeric vector of length >= 2.
- [x] Best-of scoring: `score = c(3, 1, 2, 0, 3)` (per-game scores) is accepted.

**`results()` batch entry**
- [x] `results(trn, stage, df)` with columns `match`, `score1`, `score2` enters multiple results.
- [x] Batch entry triggers auto-advance when the final results complete the stage.

**Inspection nouns**
- [x] `matches(trn)` returns all matches across all stages.
- [x] `matches(trn, "stage")` returns matches for one stage.
- [x] `matches(trn, "stage", status = "pending")` filters by status.
- [x] `standings(trn, "stage")` returns standings for one stage.
- [x] `standings(trn)` returns standings across all stages.
- [x] `stage_status(trn)` returns overview table with columns: stage, status, complete, total, materialized.
- [x] `winner(trn)` returns winner name or `NA` if tournament incomplete.
- [x] `rankings(trn)` returns final placement table.
- [x] `routing_log(trn)` returns transition audit trail.

**`print.tournament()`**
- [x] Snapshot test: print output shows stage count, per-stage status, and match progress.

**`teardown()`**
- [x] `teardown(trn, "stage")` un-materializes the stage and all downstream dependents.
- [x] Source-stage results and standings are preserved after teardown.
- [x] Torn-down stages revert to `"blocked"` in `stage_status()`.
- [x] Diamond DAG: tearing down a shared source tears down both branches.
- [x] Tearing down an already-blocked stage is a no-op.
- [x] After teardown, re-entering results and re-advancing works correctly.

**Overwrite policy**
- [x] Overwriting a result before advance is allowed and recalculates standings.
- [x] Overwriting a result after downstream materialization is blocked with error containing downstream stage ID.
- [x] After `teardown()`, overwriting the upstream result is allowed again.

**`validate()`**
- [x] `validate(spec, n = 16)` passes for feasible specs.
- [x] `validate(spec, n = 3)` fails for specs requiring more participants with error.
- [x] Error messages include stage IDs and participant counts.

## Phase 8: Documentation and Cleanup
- [x] Update roxygen examples to show only the planned API.
- [x] Regenerate docs with `devtools::document()`.
- [x] Update `README.qmd` to showcase only the planned API; then render to `README.md`.
- [x] Add migration notes in `NEWS.md` describing removed API and new equivalents.
- [x] Verify package checks/tests pass after rewrite.

## Done Definition
- [x] No public tournament API remains outside the planned surface in `scratchpad/tournament_api.qmd`.
- [x] Test suite passes using new API semantics only.
- [x] Documentation, examples, and exported references match the rewritten API exactly.
- [x] Verify: auto-advance is default; manual advance is opt-in via `auto_advance = FALSE`.
- [x] Verify: formula addressing (`"A" ~ "B"`) is not implemented in MVP.
- [x] Verify: one-match scoring uses `score = c(x, y)` and not separate single-entry score args.
- [x] Verify: batch `results()` uses Option B (`score1`, `score2` columns) and is documented.
- [x] Verify: no name-collision regressions for `top_n`, `bottom_n`, `slice_range`, `stage_status`.
- [x] Verify: `from` defaults to `previous_stage()` in linear chains; explicit `from =` required for branching.
- [x] Verify: `top_per_group(n)` is standalone; no `top_n(n, per_group = TRUE)` API exists.
- [x] Verify: standalone bracket constructors are unexported; all public tournament flows go through stage verbs.

## Roadmap (Explicitly Deferred)
- [x] Evaluate `best_across_groups(rank, n)` helper as a non-MVP convenience on top of grouped selectors. (Decision: defer; current `filter_by()` + grouped selectors are sufficient for MVP.)
- [x] Evaluate formula match-addressing syntax (`"A" ~ "B"`) after MVP stability. (Decision: defer; keep explicit `match` IDs in MVP for deterministic addressing.)
