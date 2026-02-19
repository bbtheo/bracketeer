# Test Suite Migration Guide

How the 34 test files (2,544 lines) should change so all features stay tested
but are exposed exclusively through the new API.

---

## API Function Mapping (old → new)

Every test rewrite follows this translation table:

| Old API | New API | Notes |
|---------|---------|-------|
| `single_elim(teams)` | internal `new_single_elim_bracket(teams)` | Renamed, unexported. Only used in bracket-level tests. |
| `double_elim(teams)` | internal `new_double_elim_bracket(teams)` | Same. |
| `round_robin(teams)` | internal `new_round_robin_bracket(teams)` | Same. |
| `swiss(teams)` | internal `new_swiss_bracket(teams)` | Same. |
| `two_leg_knockout(teams)` | internal `new_two_leg_bracket(teams)` | Same. |
| `group_stage_knockout(teams)` | internal `new_group_stage_knockout_bracket(teams)` | Same. |
| `bracket(teams, type)` | internal `new_bracket(teams, type)` | Same dispatcher, renamed. |
| `tournament_spec()` | `spec()` | |
| `add_stage(spec, "id", single_elim_stage())` | `single_elim(spec, "id", ...)` | Stage verb on spec. Stage-specific params go directly as args. |
| `add_stage(spec, "id", round_robin_stage())` | `round_robin(spec, "id", ...)` | Same pattern for all formats. |
| `add_transition(from, to, rule = fn)` | Implicit via `from =` and `take =` on stage verbs | No standalone edge function. |
| `split_stage(from, into = list(...))` | Multiple stage verbs with explicit `from =` | e.g., `single_elim(spec, "a", from = "src", take = top_n(4)) |> single_elim(spec, "b", from = "src", take = remaining())` |
| `from_previous()` | `previous_stage()` | Sentinel rename. |
| `set_outcome(spec, track_placements = n)` | Dropped or folded into `spec(..., track_placements = n)` | Decide during implementation. |
| `validate_tournament_spec(spec)` | `validate(spec, n)` | Merged with dry-run validation. |
| `validate_tournament(spec, n)` | `validate(spec, n)` | |
| `build_tournament(spec, teams)` | `build(spec, teams)` | |
| `set_result(b, match, score1, score2)` | `set_result(b, match, score1, score2)` | **Unchanged** for bracket-level. |
| `set_result(trn, match, score1, score2, stage_id =)` | `result(trn, stage, match, score = c(x, y))` | Tournament-level entry. |
| `result(trn, stage, match, score1, score2)` | `result(trn, stage, match, score = c(x, y))` | Signature change. |
| `set_winner(b, match, winner)` | `set_winner(b, match, winner)` | **Unchanged** for bracket-level. |
| `advance(trn, stage)` | `advance(trn, stage)` | Same but rarely needed (auto-advance is default). |
| `get_matches(bracket)` | `get_matches(bracket)` | **Unchanged** for bracket-level. |
| `get_matches(trn$stage_state$s$bracket)` | `matches(trn, "s")` | Tournament-level. |
| `get_standings(bracket)` | `get_standings(bracket)` | **Unchanged** for bracket-level. |
| `get_standings(trn$stage_state$s$bracket)` | `standings(trn, "s")` | Tournament-level. |
| `get_winner(bracket)` | `get_winner(bracket)` | **Unchanged** for bracket-level. |
| `get_winner(trn)` | `winner(trn)` | Tournament-level. |
| `is_complete(bracket)` | `is_complete(bracket)` | **Unchanged** for bracket-level. |
| `is_stage_complete(trn, stage)` | Check via `stage_status(trn)` | No standalone predicate; read from status table. |
| `get_ready_stages(trn)` | Filter `stage_status(trn)` where status is `"ready"` or similar | Folded into `stage_status()`. |
| `get_routing_log(trn)` | `routing_log(trn)` | |
| `qualify_remaining()` | `remaining()` | Returns `bracketeer_selector`. |
| `qualify_losers(...)` | `losers(...)` | Returns `bracketeer_selector`. |
| `export_matches(trn)` | `matches(trn)` (all stages, `stage = NULL`) | |
| `export_standings(trn)` | `standings(trn)` (all stages, `stage = NULL`) | |
| `export_tournament_log(trn)` | `routing_log(trn)` | |

---

## Bracket-level tests (15 files) — minimal changes

These test format-specific behavior in isolation: match generation, seeding, scoring,
tiebreakers. The bracket internals don't change — only the constructor entry point is
renamed because the old public names (`single_elim()`, `round_robin()`, etc.) are
repurposed as stage verbs.

**What to do:** Find-and-replace the constructor call. Everything else stays identical.

### test-single-elim.R (26 lines)
```r
# OLD
b <- single_elim(teams)
# NEW
b <- new_single_elim_bracket(teams)
```
Rest of file unchanged — `get_matches()`, `set_result()` on brackets stay as-is.

### test-round-robin.R (114 lines)
```r
# OLD
b <- round_robin(teams, home_away = TRUE, n_rounds = 2)
# NEW
b <- new_round_robin_bracket(teams, home_away = TRUE, n_rounds = 2)
```
All `get_matches()` assertions unchanged.

### test-swiss.R (8 lines)
```r
# OLD
b <- swiss(teams, rounds = 3)
# NEW
b <- new_swiss_bracket(teams, rounds = 3)
```

### test-double-elim.R (26 lines)
```r
# OLD
b <- double_elim(teams)
# NEW
b <- new_double_elim_bracket(teams)
```
`set_winner()` calls unchanged.

### test-double-elim-reseed.R (37 lines)
```r
# OLD
b <- double_elim(teams, reseed = TRUE)
# NEW
b <- new_double_elim_bracket(teams, reseed = TRUE)
```

### test-group-stage.R (13 lines)
```r
# OLD
b <- group_stage_knockout(teams, groups = 2, advance_per_group = 2)
# NEW
b <- new_group_stage_knockout_bracket(teams, groups = 2, advance_per_group = 2)
```

### test-best-of.R (11 lines)
```r
# OLD
b <- single_elim(teams, best_of = 3)
# NEW
b <- new_single_elim_bracket(teams, best_of = 3)
```

### test-tiebreakers.R (40 lines)
```r
# OLD
b <- round_robin(teams, tiebreakers = c("points", "head_to_head"))
# NEW
b <- new_round_robin_bracket(teams, tiebreakers = c("points", "head_to_head"))
```
`get_standings()`, `tiebreaker_chain()` calls unchanged.

### test-two-leg-knockout.R (31 lines)
```r
# OLD
b <- two_leg_knockout(teams)
# NEW
b <- new_two_leg_bracket(teams)
```

### test-two-leg-away-goals.R (12 lines)
```r
# OLD
b <- two_leg_knockout(teams, away_goals = TRUE)
# NEW
b <- new_two_leg_bracket(teams, away_goals = TRUE)
```

### test-edge-cases.R (126 lines)
Multiple constructors — replace each:
- `single_elim(...)` → `new_single_elim_bracket(...)`
- `round_robin(...)` → `new_round_robin_bracket(...)`
- `double_elim(...)` → `new_double_elim_bracket(...)`
- `swiss(...)` → `new_swiss_bracket(...)`
- `group_stage_knockout(...)` → `new_group_stage_knockout_bracket(...)`
- `two_leg_knockout(...)` → `new_two_leg_bracket(...)`
- `bracket(...)` → `new_bracket(...)`

All validation assertions (`expect_error`) unchanged — the internal constructors
enforce the same constraints.

### test-edge-cases-2.R (47 lines)
Same pattern: replace constructor calls. `set_result()`, `advance()`, `get_standings()`
on brackets unchanged.

### test-edge-cases-3.R (71 lines)
Same pattern.

### test-stage-constructors.R (29 lines)
`single_elim_stage()`, `double_elim_stage()`, etc. — these are internal stage spec
builders. If they stay as internal helpers, tests use them directly. If they're removed
entirely (stage verbs absorb their parameters), delete this file and cover the same
parameter validation in the stage verb tests.

**Decision needed:** Keep `*_stage()` as internal helpers or inline their logic into
stage verbs? If kept, this file needs only un-export changes. If inlined, delete the file.

### test-s3-dispatch-public-api.R (33 lines)
Mixed bracket + tournament dispatch. Split into two:
1. Bracket dispatch assertions → rename constructors, keep `get_matches()`,
   `get_standings()`, `is_complete()`, `set_result()` on brackets.
2. Tournament dispatch assertions → rewrite using new API (see tournament section below).

---

## Tournament API tests (16 files) — major rewrites

These all use the old spec-graph API (`tournament_spec()`, `add_stage()`, `add_transition()`,
etc.). Every file needs a full rewrite to use the pipe API. The *assertions* largely stay
the same — what changes is how the tournament is constructed and operated.

### Common rewrite pattern

Old:
```r
spec <- tournament_spec() |>
  add_stage("groups", round_robin_stage(home_away = TRUE)) |>
  add_stage("finals", single_elim_stage()) |>
  add_transition("groups", "finals", rule = function(pool, standings, ...) {
    standings$participant[1:4]
  })
trn <- build_tournament(spec, teams)
trn <- result(trn, "groups", match_id = 1, score1 = 2, score2 = 1)
advance(trn, "groups")
```

New:
```r
trn <- tournament(teams) |>
  round_robin("groups", home_away = TRUE) |>
  single_elim("finals", take = top_n(4))
trn <- result(trn, "groups", match = 1, score = c(2, 1))
# auto-advance — no explicit advance() needed
```

### test-spec-api.R (17 lines)
**Old pattern:** `tournament_spec() |> add_stage() |> add_transition() |> build_tournament()`
**Rewrite:** `spec() |> round_robin("groups") |> single_elim("finals", take = top_n(4)) |> build(teams)`
Assertions on structure stay (check that stages exist, bracket is materialized).

### test-tournament-spec-graph.R (150 lines)
**What it tests:** Stage ID uniqueness, edge registration, cycle detection, `from_previous()`
resolution, outcome tracking, spec printing.

**Rewrite approach:**
- Stage ID uniqueness: `spec() |> round_robin("x") |> round_robin("x")` → expect error
- Cycle detection: can't happen with pipe API (stages are declared linearly, `from =` only
  references prior stages). Either remove cycle tests or test the internal DAG validator
  directly.
- `from_previous()` → `previous_stage()`: test that linear chains resolve correctly, test
  that `from = "nonexistent"` errors.
- Outcome tracking: if `set_outcome()` is kept, test it. If dropped, remove.
- Spec printing: replace with `print.bracketeer_spec()` snapshot tests.

### test-tournament-qualify-losers.R (135 lines)
**What it tests:** `qualify_losers()` rule with round filters, ordering modes, validation.

**Rewrite approach:** This tests selector behavior, not tournament construction. Rewrite
to use `losers()` selector:
```r
# OLD
rule <- qualify_losers(round = "all", ordering = "source_seed")
selected <- rule(source_pool, standings, bracket)

# NEW — test selector evaluation directly
sel <- losers(round = "all", ordering = "source_seed")
selected <- evaluate_selector(sel, source_pool, standings, bracket)
```
The bracket setup (creating a single_elim, playing matches) uses internal constructors.
The assertions on which participants are selected stay identical.

### test-tournament-validation-dry-run.R (63 lines)
**Old pattern:** `tournament_spec() |> add_stage() |> add_transition() |> validate_tournament(n)`
**Rewrite:**
```r
s <- spec() |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(4))
validate(s, n = 16)
```
Feasibility/infeasibility assertions stay. Error message assertions may need updating
if wording changes.

### test-world-cup-workflow.R (169 lines)
**What it tests:** Two end-to-end workflows — FIFA groups-to-knockout, LoL Worlds swiss-to-cut.

**Rewrite approach — FIFA workflow:**
```r
# OLD
b <- group_stage_knockout(teams, groups = 4, advance_per_group = 2)
# ... set_result loop ... advance() ...

# NEW
trn <- tournament(teams) |>
  round_robin("groups", groups = 4) |>
  single_elim("knockout", take = top_per_group(2))
# ... result() loop (auto-advances) ...
winner(trn)
```

**Rewrite approach — LoL Worlds workflow:**
```r
# OLD
spec <- tournament_spec() |>
  add_stage("swiss", swiss_stage(rounds = 5)) |>
  add_stage("top_cut", single_elim_stage()) |>
  add_transition("swiss", "top_cut", rule = function(pool, standings, ...) {
    standings$participant[1:8]
  }) |>
  set_outcome(track_placements = 4)
trn <- build_tournament(spec, teams)
# ... result(trn, "swiss", match_id, score1, score2) loop ...

# NEW
trn <- tournament(teams) |>
  swiss("swiss", rounds = 5) |>
  single_elim("top_cut", take = top_n(8))
# ... result(trn, "swiss", match = id, score = c(x, y)) loop ...
```
- `get_routing_log(trn)` → `routing_log(trn)`
- `get_winner(trn)` → `winner(trn)`
- `is_complete(trn)` → check `stage_status(trn)` or a new `is_complete(trn)` if kept
- Auto-advance means `advance()` calls are removed.

### test-tournament-export-helpers.R (115 lines)
**What it tests:** `export_tournament_log()`, `export_matches()`, `export_standings()`.

**Rewrite:** These are replaced by `routing_log(trn)`, `matches(trn)`, `standings(trn)`.
The tournament construction changes to pipe API. The assertions on output shape (column
names, row counts, compound IDs) stay — verify the new functions produce the same
structure. If column names change (e.g., `stage_id` → `stage`), update assertions.

### test-tournament-seeding-policies.R (136 lines)
**What it tests:** Transition seeding modes (by_source_rank, as_is, cross_group, snake).

**Rewrite approach:** Seeding is now a parameter on the stage verb:
```r
# OLD
add_transition("groups", "finals", rule = fn, seeding = "cross_group")

# NEW
single_elim(spec, "finals", from = "groups", take = top_n(8), seeding = "cross_group")
```
Tournament construction changes to pipe API. Seeding assertions on participant order
in materialized brackets stay identical.

### test-tournament-transition-conflicts.R (87 lines)
**What it tests:** Overlap detection when multiple transitions consume from the same source.

**Rewrite approach:**
```r
# OLD
add_transition("src", "a", rule = fn1, consume = TRUE) |>
add_transition("src", "b", rule = fn2, consume = TRUE)

# NEW
single_elim(spec, "a", from = "src", take = top_n(4)) |>
single_elim(spec, "b", from = "src", take = remaining())
```
Overlap/conflict assertions stay. The `consume` semantics may become implicit (each
`take =` selector consumes by default — `remaining()` takes what's left). If `consume`
becomes implicit, remove `consume`-specific assertions and test via `remaining()`.

### test-tournament-split-stage.R (80 lines)
**What it tests:** `split_stage()` fan-out — multiple destinations from one source.

**Rewrite approach:** `split_stage()` is replaced by multiple stage verbs with explicit
`from =`:
```r
# OLD
split_stage(spec, "groups", into = list(
  winners = list(rule = function(...) ..., stage = single_elim_stage()),
  losers  = list(rule = qualify_remaining(), stage = single_elim_stage())
))

# NEW
spec() |>
  round_robin("groups") |>
  single_elim("winners", from = "groups", take = top_n(4)) |>
  single_elim("losers",  from = "groups", take = remaining())
```
Assertions on fan-out materialization and participant routing stay. Remove assertions
about `split_stage()` internal mechanics (transition ID naming, named-list validation).

### test-tournament-result-alias.R (56 lines)
**What it tests:** `result()` fluent helper with `score1`/`score2` positional args.

**Rewrite approach:** Signature changes from `result(trn, stage, match, score1, score2)`
to `result(trn, stage, match, score = c(x, y))`. Rewrite every call:
```r
# OLD
trn <- result(trn, "groups", match_id = 1, score1 = 2, score2 = 1)

# NEW
trn <- result(trn, "groups", match = 1, score = c(2, 1))
```
Auto-advance forwarding test: change to verify auto-advance is *default* (not opt-in).
Add a test that `auto_advance = FALSE` disables it.

### test-tournament-branching.R (241 lines)
**What it tests:** Fan-out materialization, transition priority/ordering, consume flag,
`qualify_remaining()`, multi-inbound dependency, routing log determinism.

**Rewrite approach:** Largest rewrite. Replace all `add_stage()`/`add_transition()` with
pipe API. Replace `qualify_remaining()` with `remaining()`.
```r
# OLD
spec <- tournament_spec() |>
  add_stage("groups", round_robin_stage()) |>
  add_stage("winners", single_elim_stage()) |>
  add_stage("losers", single_elim_stage()) |>
  add_transition("groups", "winners", rule = fn_top4) |>
  add_transition("groups", "losers", rule = qualify_remaining())

# NEW
trn <- tournament(teams) |>
  round_robin("groups") |>
  single_elim("winners", from = "groups", take = top_n(4)) |>
  single_elim("losers",  from = "groups", take = remaining())
```
- Transition priority assertions: if the new API doesn't expose priority (it's implicit
  from declaration order), rewrite assertions to check declaration-order determinism.
- Consume flag assertions: replace with `remaining()` behavior tests.
- Multi-inbound tests: if a stage has multiple sources (future feature), keep as-is
  or defer.
- Routing log: `get_routing_log(trn)` → `routing_log(trn)`.

### test-tournament-routing-log.R (116 lines)
**What it tests:** Routing log entry fields, transition ordering by priority/ID, determinism.

**Rewrite approach:** Tournament construction → pipe API. Log query → `routing_log(trn)`.
Priority/ID ordering assertions may change if priority is replaced by declaration order.
Entry field assertions (from_stage, to_stage, participants, transition_id) stay — verify
`routing_log()` returns the same columns.

### test-tournament-active-stages.R (73 lines)
**What it tests:** `get_ready_stages()` topological ordering after split-source completion.

**Rewrite approach:** `get_ready_stages(trn)` → filter `stage_status(trn)` for actionable
stages. Tournament construction → pipe API. Auto-advance changes the test logic: with
auto-advance, stages may already be materialized after entering the last result, so
"ready stages" may advance before you check. Tests should either:
- Use `auto_advance = FALSE` to test manual stage readiness
- Or assert on `stage_status()` after entering results (stages auto-advance through)

### test-tournament-validation-errors.R (47 lines)
**What it tests:** Detailed error messages from `validate_tournament()` with IDs and counts.

**Rewrite:** `validate_tournament(spec, n)` → `validate(spec, n)`. Tournament construction
→ pipe API. Error message text assertions may need updating if wording changes. Error
*semantics* (infeasible transition, too few participants) stay.

### test-tournament-match-addressing.R (36 lines)
**What it tests:** Stage-scoped `set_result(stage_id, match_id)`, compound match ID parsing.

**Rewrite approach:** The new API always requires stage: `result(trn, "stage", match, score)`.
Compound match ID (`"stage::match"`) may still be supported internally but isn't the primary
API. Rewrite to use `result()` with explicit stage arg. Keep compound-ID tests if the
feature is preserved, remove if not.

### test-tournament-stage-transition.R (159 lines)
**What it tests:** Source/downstream materialization, stage completion requirements,
auto-advance, `track_placements`, `is_stage_complete()`, overwrite semantics.

**Rewrite approach:**
- Construction → pipe API
- `is_stage_complete(trn, "s")` → check `stage_status(trn)` row
- Auto-advance is now default, so tests for "stage completes but doesn't advance until
  `advance()` is called" should be rewritten under `auto_advance = FALSE` mode
- Add new tests for auto-advance-as-default behavior
- Overwrite semantics: rewrite to use `result()` with `score =` and test the overwrite
  policy from Issue 7 (allow before advance, block after downstream materialization,
  `teardown()` to unlock)

---

## Cross-cutting / documentation tests (3 files)

### test-docs-dsl-positioning.R (45 lines)
**What it tests:** Ensures FIFA vignette avoids "classic-format" function names.

**Rewrite:** Update banned-pattern list to match new API names. The purpose stays (vignette
should use tournament pipe API, not internal constructors). Banned patterns change from
`group_stage_knockout`, `single_elim`, etc. to whatever internal names are chosen
(`new_*_bracket`).

### test-vignettes-catalog.R (47 lines)
**What it tests:** Vignette existence and correct API usage patterns.

**Rewrite:** Update expected function names:
- `validate_tournament` → `validate`
- `split_stage` → multiple stage verbs with `from =`
- `result` → `result` (same name, new signature)
- `get_routing_log` → `routing_log`
- `get_winner` → `winner`
- `is_complete` → `is_complete` or `stage_status` check

### test-docs-tournament-guides.R (148 lines)
**What it tests:** Multi-vignette documentation compliance — lifecycle, function deep-dive,
error catalog, NHL vignette.

**Rewrite:** Largest doc-test rewrite. Update all expected function-name patterns to match
new API surface. The 30+ function-call patterns validated here need to map to the new names.
Consider generating the expected-pattern list from the planned API surface table to keep
it in sync.

---

## New tests to add

These cover features that exist in the plan but have no corresponding test in the
current suite.

### New API construction
- [ ] `spec()` returns a `bracketeer_spec` object with empty stages/edges.
- [ ] `tournament(teams)` returns `c("tournament", "bracketeer_spec")` class.
- [ ] Stage verbs work on both `spec()` and `tournament()` objects (inheritance dispatch).
- [ ] `build(spec, teams)` materializes a spec into a tournament.
- [ ] `tournament(teams) |> round_robin("x")` produces a playable tournament.

### `previous_stage()` and `from =`
- [ ] Linear chain: `from` is never needed, resolves via declaration order.
- [ ] Branching: explicit `from =` required when two stages read from the same source.
- [ ] `from = "nonexistent_stage"` errors with actionable message.
- [ ] `previous_stage()` on first stage (no prior) errors clearly.

### `take =` with selectors
- [ ] `take = top_n(n)` selects top n by standings.
- [ ] `take = bottom_n(n)` selects bottom n.
- [ ] `take = slice_range(from, to)` selects standings positions from:to.
- [ ] `take = remaining()` selects participants not consumed by prior transitions.
- [ ] `take = losers(...)` selects eliminated participants with round/ordering options.
- [ ] `take = filter_by(fn)` applies custom predicate on standings.
- [ ] `take = top_per_group(n)` on grouped source selects top n per group.
- [ ] `take = bottom_per_group(n)` on grouped source selects bottom n per group.
- [ ] `take = slice_per_group(from, to)` on grouped source selects positions per group.
- [ ] `top_per_group()` on non-grouped source errors with clear message.
- [ ] All selectors produce deterministic ordering.

### `round_robin(groups = )`
- [ ] `round_robin("groups", groups = 4)` with 16 teams creates 4 groups of 4.
- [ ] Standings have group membership column.
- [ ] Per-group selectors work on grouped standings.

### Auto-advance (default)
- [ ] Entering last result in a stage auto-materializes downstream stages.
- [ ] Multi-stage linear chain auto-advances through completion.
- [ ] `auto_advance = FALSE` disables auto-advance; explicit `advance()` required.

### `result()` new signature
- [ ] `result(trn, stage, match, score = c(2, 1))` works.
- [ ] `score` must be numeric vector of length >= 2.
- [ ] Best-of scoring: `score = c(3, 1, 2, 0, 3)` (per-game scores) works.

### `results()` batch entry
- [ ] `results(trn, stage, df)` with columns `match`, `score1`, `score2` enters multiple results.
- [ ] Batch entry triggers auto-advance if final results complete the stage.

### Inspection nouns
- [ ] `matches(trn)` returns all matches across all stages.
- [ ] `matches(trn, "stage")` returns matches for one stage.
- [ ] `matches(trn, "stage", status = "pending")` filters by status.
- [ ] `standings(trn, "stage")` returns standings for one stage.
- [ ] `standings(trn)` returns standings across all stages.
- [ ] `stage_status(trn)` returns overview table (stage, status, complete, total, materialized).
- [ ] `winner(trn)` returns winner name or NA if incomplete.
- [ ] `rankings(trn)` returns final placement table.
- [ ] `routing_log(trn)` returns transition audit trail.

### `print.tournament()`
- [ ] Snapshot test: print output shows stage count, per-stage status, match progress.

### `teardown()`
- [ ] `teardown(trn, "stage")` un-materializes the stage and all downstream dependents.
- [ ] Source stage results and standings are preserved.
- [ ] Torn-down stages revert to `"blocked"` in `stage_status()`.
- [ ] Diamond DAG: tearing down a shared source tears down both branches.
- [ ] Tearing down an already-blocked stage is a no-op.
- [ ] After teardown, re-entering results and re-advancing works.

### Overwrite policy
- [ ] Overwriting a result before advance is allowed.
- [ ] Overwriting a result after downstream materialization is blocked with error.
- [ ] After `teardown()`, overwriting the result is allowed again.

### `validate()`
- [ ] `validate(spec, n = 16)` passes for feasible specs.
- [ ] `validate(spec, n = 3)` fails for specs requiring more participants.
- [ ] Error messages include stage IDs and participant counts.
