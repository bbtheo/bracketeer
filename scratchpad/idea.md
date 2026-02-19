# Complex Tournament DSL: Implementation Blueprint

## 1) Goal

`bracketeer` should support highly composable tournaments while preserving its core principles:

- Base R only.
- S3-first design.
- Functional style (return updated objects; never mutate in place).
- Existing constructors (`single_elim()`, `double_elim()`, `round_robin()`, `swiss()`, `group_stage_knockout()`, `two_leg_knockout()`) remain fully supported.

The key outcome is a graph-based tournament system that can describe real-world flows (group phase, top cut, consolation, drop-down brackets) with clear, testable wiring.

## 2) API Direction and Naming

### 2.1 Core naming decisions

- `tournament_spec()` defines a multi-stage tournament.
- `add_stage()` defines stage nodes.
- `add_transition()` defines edges between stages.
- `set_outcome(track_placements = n)` defines output ranking depth.
- `build_tournament()` builds runtime state from a spec and participants.

### 2.2 Separation of concerns

Stage definition and stage wiring must be separate.

- `add_stage()` only defines format and parameters for a stage.
- `add_transition()` only defines where entrants come from and how they are selected.

This matches the internal graph model (`stages` + `edges`) and avoids burying transition logic inside stage constructors.

## 3) Proposed User-Facing API

### 3.1 Primary pattern (explicit stage wiring)

```r
spec <- tournament_spec() |>
  add_stage(
    stage_id = "group_phase",
    stage = round_robin_stage(
      groups = 6,
      rounds = 2,
      tiebreakers = tiebreaker_chain("points", "score_diff", "score_for", "head_to_head")
    )
  ) |>
  add_stage(
    stage_id = "playoffs",
    stage = single_elim_stage(seed = TRUE, reseed = FALSE)
  ) |>
  add_transition(
    from = "group_phase",
    to = "playoffs",
    rule = qualify_top(4, per_group = TRUE),
    seeding = "by_source_rank"
  ) |>
  set_outcome(track_placements = 4)

trn <- build_tournament(spec, participants = teams)
```

### 3.2 Branching pattern (winners + consolation)

```r
spec <- tournament_spec() |>
  add_stage("group_phase", round_robin_stage(groups = 4, rounds = 1)) |>
  add_stage("championship", single_elim_stage()) |>
  add_stage("consolation", single_elim_stage()) |>
  add_transition(
    from = "group_phase",
    to = "championship",
    rule = qualify_top(2, per_group = TRUE),
    consume = TRUE,
    priority = 1L
  ) |>
  add_transition(
    from = "group_phase",
    to = "consolation",
    rule = qualify_remaining(),
    consume = TRUE,
    priority = 2L
  )
```

### 3.3 Optional sugar for common splits

```r
spec <- tournament_spec() |>
  add_stage("group_phase", round_robin_stage(groups = 4)) |>
  add_stage("championship", single_elim_stage()) |>
  add_stage("consolation", single_elim_stage()) |>
  split_stage(
    from = "group_phase",
    into = list(
      championship = qualify_top(2, per_group = TRUE),
      consolation = qualify_remaining()
    )
  )
```

`split_stage()` is syntactic sugar that compiles into multiple `add_transition()` calls.

### 3.4 Optional sugar for simple linear flows

For linear specs, optional shorthand can reduce repetition:

```r
spec <- tournament_spec() |>
  add_stage("groups", round_robin_stage(groups = 4)) |>
  add_stage("playoffs", single_elim_stage()) |>
  add_transition(from = from_previous(), to = "playoffs", rule = qualify_top(2, per_group = TRUE))
```

`from_previous()` resolves to the most recently added stage ID.

## 4) Scope Model

### 4.1 MVP scope

Support linear multi-stage flows first:

- Stage A -> Stage B -> Stage C.
- Group phase -> single elimination.
- Swiss -> top cut single elimination.

### 4.2 Phase-2 scope

Support branched flows:

- Winners path + consolation path.
- Multiple downstream stages consuming different slices of one upstream stage.
- Parallel-ready stages (more than one stage can be active at the same time).

### 4.3 Non-goals for first implementation

- Cyclic tournament graphs.
- Arbitrary user-defined pairing engines.

## 5) Core Data Model

### 5.1 `tournament_spec` (new S3 class)

```r
list(
  stages = list(),
  edges = list(),
  outcome = list(track_placements = 1L),
  metadata = list(version = 1L)
)
```

Class: `c("tournament_spec", "bracket_spec")`

### 5.2 Stage specs

Each stage is a normalized `stage_spec` object:

- `round_robin_stage(...)`
- `swiss_stage(...)`
- `single_elim_stage(...)`
- `double_elim_stage(...)`
- `two_leg_stage(...)`

Double-elimination scope decision:

- MVP/Phase-2: `double_elim_stage()` remains a self-contained stage type (existing behavior model).
- Phase-3+: optional composition of double elimination via explicit winners/losers graph routing can be explored.

### 5.3 Transition specs

```r
list(
  transition_id = "group_phase_to_playoffs",
  from_stage_id = "group_phase",
  to_stage_id = "playoffs",
  rule = qualify_top(4, per_group = TRUE),
  seeding = "by_source_rank",
  priority = 1L,
  consume = TRUE,
  allow_overlap = FALSE
)
```

Class: `"transition_spec"`.

Transition IDs must be predictable by default:

- Default auto ID format: `"{from_stage_id}_to_{to_stage_id}"`.
- If duplicates are created for the same pair, suffix deterministically (`..._2`, `..._3`).

## 6) Runtime Object Model

Add runtime class: `tournament`.

```r
list(
  spec = <tournament_spec>,
  participants = <normalized participant table>,
  stage_state = list(
    group_phase = list(bracket = <bracket>, status = "in_progress"),
    championship = list(bracket = NULL, status = "blocked"),
    consolation = list(bracket = NULL, status = "blocked")
  ),
  active_stage_ids = c("group_phase"),
  completed = FALSE,
  rankings = NULL,
  routing_log = list()
)
```

Class: `"tournament"`.

`active_stage_ids` is plural to support parallel-ready stages.

## 7) Execution Semantics

### 7.1 Build

`build_tournament(spec, participants)`:

1. Validate graph and stage configs.
2. Normalize participant input.
3. Materialize all source stages with no incoming edges.
4. Mark dependent stages as blocked.
5. Set `active_stage_ids` to all materialized, unfinished stages.

### 7.2 Result entry and match addressing

Primary method:

- `set_result.tournament(tournament, match_id, score1, score2, stage_id = NULL)`

Addressing rules:

- If `stage_id` is provided, `match_id` is interpreted within that stage.
- If `stage_id` is omitted, `match_id` must be globally unambiguous.
- Support explicit compound IDs: `"stage_id::match_id"`.
- Reserve `::` as a separator and disallow `::` inside `stage_id` and `match_id` values.

Recommended helper alias for fluent workflows:

- `result(tournament, stage_id, match_id, score1, score2)`

Example:

```r
trn <- trn |>
  result("group_phase", "G1-M1", score1 = 3, score2 = 1) |>
  result("group_phase", "G1-M2", score1 = 2, score2 = 2)
```

Optional terse form can accept positional scores:

```r
result(trn, "group_phase", "G1-M1", 3, 1)
```

### 7.3 Advance behavior

`advance.tournament(tournament, stage_id = NULL)`:

1. Validate target source stage is complete.
2. Compute source standings/results once.
3. Resolve all outgoing transitions in deterministic order (`priority`, then `transition_id`).
4. Materialize all destination stages whose dependencies are satisfied in the same call.
5. Refresh `active_stage_ids` to all unfinished and runnable stages.
6. If no unfinished stages remain, mark tournament complete and compute rankings.

## 8) Transition Rules (Selection DSL)

### 8.1 Rule constructors

- `qualify_top(n, per_group = TRUE)`
- `qualify_positions(positions, per_group = TRUE)`
- `qualify_top(n, per_group = FALSE)`
- `qualify_remaining()`
- `qualify_losers(round = "all", stage = NULL, ordering = "elimination_round")`

`qualify_losers()` semantics:

- `round = "all"` selects all eliminated participants from the source stage.
- `round = 1` selects losers from round 1 only.
- `round = c(1, 2)` selects losers from rounds 1 and 2.
- `round = "latest"` selects only the most recently eliminated set.

Ordering options:

- `ordering = "elimination_round"` (default) sorts by round eliminated, then source seed.
- `ordering = "source_seed"` sorts by initial source seeding.
- `ordering = "as_recorded"` preserves elimination event order.

`qualify_losers(round = "latest")` is the expected mode for per-round drop-down flows.

### 8.2 Consumption semantics

Use intent-driven parameters:

- `consume = TRUE` means selected participants are removed from the transition pool for subsequent transitions from the same source completion event.
- `consume = FALSE` means selected participants remain available.

`allow_overlap` is retained for explicit exceptional cases; defaults remain safe.

## 9) Tiebreakers and Seeding Semantics

### 9.1 Tiebreakers

`tiebreakers` are ordered by priority, highest first.

Preferred API:

- `tiebreaker_chain("points", "score_diff", "head_to_head")`

Equivalent vector form can still be accepted for backward compatibility.

Supported values remain:

- `points`, `wins`, `draws`, `losses`, `score_for`, `score_against`, `score_diff`, `sos`, `buchholz`, `head_to_head`, `alphabetical`.

### 9.2 Cross-stage seeding

Supported policies:

- `"as_is"`
- `"by_source_rank"`: bucket by placement (all 1st, then all 2nd, etc.) in deterministic group/stage order
- `"cross_group"`: rank all qualifiers globally by stage tiebreakers, then seed by global rank
- `"snake"`
- `"random"` (deterministic when seed is controlled)

Default: `"by_source_rank"`.

## 10) Validation Contract

### 10.1 Spec-level validation

- Unique stage IDs.
- Unique transition IDs.
- Acyclic graph.
- All edges reference existing stages.
- Non-source stages have incoming transitions.
- `stage_id` and `match_id` cannot contain reserved separator `::`.

Cycle detection algorithm decision:

- Use Kahn topological sort for deterministic cycle detection and source-stage discovery.

### 10.2 Parameter validation

- `best_of` must be odd integer.
- Byes auto-resolve only in round 1.
- Two-leg away-goals only valid for two-leg matches.
- Group allocation validation must check compatibility with participant count.

Group allocation behavior:

- `groups = 6` means balanced split with max size difference of 1.
- If strict equal sizes are requested, validation errors with actionable hints.
- Allow explicit manual group sizes, e.g. `groups = c(5, 4, 4, 4)`.

### 10.3 Runtime validation

- Cannot `advance()` incomplete source stage.
- Transition must produce entrant count compatible with destination stage.
- Transition conflicts must error clearly unless explicitly allowed.
- Destination with multiple inbound dependencies does not materialize until all are resolved.
- `advance(stage_id)` with multiple outbound transitions resolves and materializes all eligible downstream stages in one call.

### 10.4 Dry-run validation

Add:

- `validate_tournament(spec, n_participants)`

Purpose:

- Validate full flow feasibility before runtime.
- Catch qualifier-count and bracket-size mismatches early.
- Produce actionable diagnostics without needing participant names.

### 10.5 Result correction and irreversibility policy

MVP policy:

- `set_result()` on completed matches errors by default.
- `set_result(..., overwrite = TRUE)` is allowed for correction before downstream stages are materialized.
- `advance()` is irreversible in MVP once it materializes dependent stages.

Future enhancement:

- Snapshot/restore or `rollback_advance()` can be introduced in a later phase.

Knockout tie policy:

- If ties are not allowed for the stage format, match resolution must be explicit (e.g., tie-break score entry or manual winner resolution via existing APIs).

## 11) Backward Compatibility Strategy

- Existing constructors stay unchanged.
- Existing single-format specs stay unchanged.
- New graph API is additive.
- Existing `build_bracket()` remains for single-format specs.
- New `build_tournament()` handles multi-stage graphs.

Compatibility sugar can be added later:

- `as_tournament_spec(single_elim_spec(...))`

## 12) Suggested Generics and Helpers

- `build_tournament()`
- `validate_tournament_spec()`
- `validate_tournament()`
- `add_transition()`
- `from_previous()`
- `split_stage()`
- `resolve_transition()`
- `materialize_stage()`
- `is_stage_complete()`
- `get_ready_stages()`
- `get_routing_log()`
- `compute_tournament_rankings()`

Existing generics reused:

- `set_result()`
- `advance()`
- `get_matches()`
- `get_standings()`

## 13) Test-First Implementation Plan

### Phase 1: Graph foundations

Tests:

- `tournament_spec()` creates valid empty graph.
- `add_stage()` registers unique stage IDs.
- `add_transition()` registers valid edges.
- Duplicate stage or transition IDs error.
- Cycle detection errors.

Files:

- `tests/testthat/test-tournament-spec-graph.R`

### Phase 2: Linear flow runtime

Tests:

- Group stage builds first.
- Completing source stage + `advance()` builds downstream stage.
- `track_placements` works for ranking output depth.
- Match addressing works with `stage_id + match_id` and compound IDs.
- Compound ID parser with `::` rejects ambiguous/reserved IDs.

Files:

- `tests/testthat/test-tournament-stage-transition.R`
- `tests/testthat/test-tournament-match-addressing.R`

### Phase 3: Parallel active stages

Tests:

- After split, two downstream stages can be active simultaneously.
- `active_stage_ids` lists all runnable unfinished stages.
- `get_ready_stages()` is deterministic.

Files:

- `tests/testthat/test-tournament-active-stages.R`

### Phase 4: Branching and transition fan-out

Tests:

- Winners/consolation split from one source stage.
- Transition order is `priority` then `transition_id`.
- `consume = TRUE` partitioning behaves correctly.
- `qualify_remaining()` selects remainder correctly.
- One `advance()` call materializes all ready downstream stages from the same source.

Files:

- `tests/testthat/test-tournament-branching.R`

### Phase 5: Conflict handling and loser routing

Tests:

- Overlap conflict errors by default.
- Explicit overlap allowed only with opt-in.
- `qualify_losers(round = "all" | "latest" | integer vector)` behaves per spec.
- Loser ordering modes are deterministic and tested.

Files:

- `tests/testthat/test-tournament-transition-conflicts.R`
- `tests/testthat/test-tournament-qualify-losers.R`

### Phase 6: Validation and diagnostics

Tests:

- `validate_tournament(spec, n_participants)` detects infeasible flows.
- Group-size compatibility diagnostics include fixes.
- Error messages include transition ID, stage IDs, and counts.

Files:

- `tests/testthat/test-tournament-validation-dry-run.R`
- `tests/testthat/test-tournament-validation-errors.R`

### Phase 7: Routing auditability

Tests:

- Each transition resolution appends a routing log entry.
- Log includes selection reason and pool size before/after.
- Log order matches resolver order.

Files:

- `tests/testthat/test-tournament-routing-log.R`

## 14) Phase-2 Detailed Plan (Branched Flows)

### 14.1 Branching mechanics

- One source stage completion triggers resolution of all outgoing transitions.
- Transition resolver uses one canonical source table.
- `consume = TRUE` transitions remove selected participants from subsequent pools.
- `qualify_remaining()` enables simple winners/rest split.
- One `advance()` call resolves full fan-out from the source stage.

### 14.2 Transition metadata

Required fields:

- `transition_id`
- `from_stage_id`
- `to_stage_id`
- `rule`
- `priority`
- `consume`

Optional fields:

- `allow_overlap`
- `seeding`

### 14.3 Predictable transition IDs

Default generation:

- `"{from}_to_{to}"`
- deterministic suffix for collisions (`_2`, `_3`, ...)

No UUID defaults.

### 14.4 Routing log schema

```r
list(
  source_stage_id = "group_phase",
  transition_id = "group_phase_to_championship",
  rule_applied = "qualify_top(2, per_group = TRUE)",
  selected = c("Team A", "Team B"),
  pool_before = 16L,
  pool_after = 8L,
  timestamp = Sys.time()
)
```

### 14.5 Split-stage sugar behavior

`split_stage(from, into)` compiles to multiple transitions with:

- deterministic transition order matching `into` list order,
- default `consume = TRUE` for mutually exclusive splits,
- deterministic auto-generated transition IDs.

### 14.6 Loser-routing details

- `qualify_losers(round = "latest")` is the default for drop-down brackets between rounds.
- `qualify_losers(round = "all")` is for post-stage consolation seeding.
- If `round` references unavailable elimination rounds, validation fails with explicit available rounds in the error.

## 15) Outcome Semantics

Replace ambiguous `champion_count` with explicit placement tracking:

- `set_outcome(track_placements = 1)` means champion only.
- `set_outcome(track_placements = 4)` means 1st through 4th.

This separates:

- tournament winner semantics (always one champion unless format explicitly allows ties), and
- reporting depth semantics.

## 16) Group Allocation Semantics

### 16.1 Common case first

- `groups = 6` is the default ergonomic form.

### 16.2 Explicit strategy when needed

- `groups = group_allocation("seeded", 6)`
- `groups = group_allocation("manual", list(A = c(...), B = c(...)))`

### 16.3 Validation behavior

If requested allocation is infeasible, emit clear diagnostics and repair options.

Example:

```text
Error: 17 participants cannot be evenly divided into 4 groups.
Hint: Use balanced groups, provide explicit group sizes (e.g. c(5,4,4,4)), or add byes.
```

## 17) Visual and Documentation UX

### 17.1 Spec printer

`print.tournament_spec()` should show a readable stage graph.

Example style:

```text
[ group_phase ] ---> [ championship ]
       |
       +-----------> [ consolation ]
```

### 17.2 Error catalog

Add docs section listing common errors, causes, and fixes.

Include at minimum:

- transition conflict,
- infeasible qualifier counts,
- ambiguous match IDs,
- unresolved stage dependencies,
- invalid group allocation.

## 18) Incremental Milestones

Milestone A:

- Graph API: `add_stage()`, `add_transition()`, validation basics.

Milestone B:

- Linear runtime execution and placement tracking.

Milestone C:

- Branching runtime with `consume` semantics and parallel `active_stage_ids`.

Milestone D:

- `split_stage()` sugar and `qualify_losers()` rule.

Milestone E:

- Dry-run validation, routing log, visual printer, and error catalog docs.

## 19) Phase-2 Engineering Checklist

### 19.1 API and naming

- [ ] Replace transition-in-stage examples with `add_transition()` examples.
- [ ] Replace `champion_count` with `track_placements` everywhere.
- [ ] Add `split_stage()` and document how it compiles to transitions.

### 19.2 Transition engine

- [ ] Add deterministic transition ID generation (`{from}_to_{to}`, suffixed on collisions).
- [ ] Add `consume` semantics and tests.
- [ ] Keep `allow_overlap` opt-in and heavily validated.

### 19.3 Branching and losers routing

- [ ] Add `qualify_remaining()` rule.
- [ ] Add `qualify_losers(round, ordering)` with explicit round semantics.
- [ ] Add winners/consolation integration fixture and assertions.

### 19.4 Runtime behavior

- [ ] Move from `active_stage_id` to `active_stage_ids`.
- [ ] Add `get_ready_stages()` helper.
- [ ] Add compound match IDs (`"stage::match"`) and reserved-token checks.
- [ ] Add optional fluent alias `result()`.
- [ ] Ensure one `advance()` call materializes all ready downstream stages for the chosen source stage.

### 19.5 Validation and diagnostics

- [ ] Implement `validate_tournament(spec, n_participants)` dry-run checks.
- [ ] Add group-allocation compatibility validation with actionable hints.
- [ ] Standardize transition error payloads (IDs + counts + suggested fixes).
- [ ] Implement Kahn topological validation for deterministic cycle detection.
- [ ] Document/result correction policy (`overwrite = TRUE`) and non-reversible `advance()` behavior.

### 19.6 Auditability

- [ ] Add routing log entries with `rule_applied`, `pool_before`, `pool_after`.
- [ ] Add `get_routing_log()` accessor.

### 19.7 Docs and discoverability

- [ ] Add `print.tournament_spec()` graph output.
- [ ] Add README branch-routing example.
- [ ] Add error catalog documentation page.
- [ ] Add docs note: `double_elim_stage()` remains monolithic in MVP/Phase-2.

### 19.8 Verification gate

- [ ] Run targeted new tests by filter.
- [ ] Run full suite: `devtools::test()`.
- [ ] Verify no regression in existing bracket/spec APIs.

## 20) Bottom Line

The architecture remains spec -> build -> runtime orchestration, but with a clearer API surface:

- explicit `add_transition()` wiring,
- explicit `track_placements` outcome semantics,
- explicit branched-flow tools (`consume`, `split_stage`, `qualify_losers`),
- explicit parallel stage readiness (`active_stage_ids`), and
- explicit preflight checks (`validate_tournament`).

This keeps the model powerful for advanced tournaments while reducing user cognitive load for common cases.

## 21) Recommended Sprint Order

Sprint 1: Foundation (Milestone A)

1. `tournament_spec()` constructor.
2. `add_stage()` with validation.
3. `add_transition()` with validation.
4. Kahn-based cycle detection.
5. `print.tournament_spec()` simple graph view.

Sprint 2: Linear Runtime (Milestone B)

1. `build_tournament()` for source stages.
2. `set_result.tournament()` with stage routing.
3. `advance.tournament()` for single downstream.
4. `is_stage_complete()` generic.
5. `track_placements` ranking output.

Sprint 3: Branching Core (Milestone C)

1. `active_stage_ids` and `get_ready_stages()`.
2. Multi-transition fan-out in one `advance()` call.
3. `consume` semantics.
4. `qualify_remaining()`.
5. Transition conflict detection.

Sprint 4: Convenience and Polish (Milestone D-E)

1. `split_stage()` sugar.
2. `qualify_losers()` (start with `"all"` and `"latest"`).
3. `validate_tournament()` dry-run preflight.
4. `result()` helper.
5. Routing log + `get_routing_log()`.
6. Error catalog and expanded docs.

## 22) Adoption Risks and Mitigations

### 22.1 Discoverability risk

Risk:

- Potential users may not find the package easily because tournament orchestration is a niche search domain in R.

Mitigations:

- Prioritize a strong README with visual examples and copy-paste workflows.
- Publish domain-specific vignettes (World Cup style, Swiss + top cut, esports group-to-playoff).
- Publish external educational content (blog posts, community posts) tied to concrete examples.

### 22.2 Large API surface risk

Risk:

- Multi-stage workflows introduce many functions and concepts.

Mitigations:

- Define a strict onboarding path in docs:
  1. `tournament_spec()`
  2. `add_stage()`
  3. `add_transition()`
  4. `build_tournament()`
  5. `set_result()/result()`
  6. `advance()`
- Group function reference docs by role:
  - Stage constructors
  - Transition rules
  - Runtime operations
  - Diagnostics and validation
- Add `tournament_template()` presets for common formats (see section 23.3).

### 22.3 Error quality risk

Risk:

- Complex graph and tournament-state rules can produce high-friction errors if messages are not specific.

Mitigations:

- Every error must include:
  - what failed,
  - where it failed (`stage_id`, `transition_id`, `match_id` when relevant),
  - fix guidance.
- Error catalog becomes a required deliverable (not optional docs polish).

## 23) Documentation and Onboarding Plan

### 23.1 Priority flagship vignette

Add a full FIFA World Cup-style vignette:

- group stage,
- round of 16,
- quarterfinals,
- semifinals,
- final,
- third-place match.

This should be a first-class end-to-end showcase.

### 23.2 High-impact domain vignette

Add Swiss + top cut vignette early:

- Swiss rounds with tiebreakers,
- qualification to elimination bracket,
- final placement extraction.

### 23.3 Preset templates

Introduce optional `tournament_template()` helpers for quick starts:

- `tournament_template("world_cup_32")`
- `tournament_template("swiss_top8")`
- `tournament_template("group_then_single_elim")`

Templates should return editable `tournament_spec` objects.

### 23.4 Function discovery structure

Document functions by lifecycle:

- Define: `tournament_spec()`, stage constructors, transition rules.
- Validate: `validate_tournament_spec()`, `validate_tournament()`.
- Run: `build_tournament()`, `result()`, `advance()`.
- Inspect: `get_matches()`, `get_standings()`, `get_ready_stages()`, `get_routing_log()`.
- Export: logging/serialization functions.

## 24) Visualization and Data Export Strategy

### 24.1 Visualization scope

Core package remains base-R and execution-focused. Visualization is intentionally out-of-core.

Recommended path:

- Provide stable extraction helpers from core objects.
- Support a companion visualization package (for example `bracketeer.viz`) for bracket charts and live status displays.

### 24.2 Export interfaces

Add explicit export utilities:

- `export_tournament_log(tournament)` returning a tidy event/match/routing data frame.
- `export_matches(tournament)` and `export_standings(tournament)` helpers for downstream reporting.

## 25) Persistence and Interoperability

### 25.1 Persistence guarantees

- `tournament_spec` and `tournament` objects must be fully serializable with `saveRDS()` / `readRDS()`.
- Serialization round-trip should be covered by tests.

### 25.2 Interchange format

Consider a JSON layer for specs:

- `as_json(tournament_spec)` / `from_json_tournament_spec()` (phase-later feature).
- Focus on stable schema versioning if introduced.

## 26) Simulation and What-If Roadmap

### 26.1 Branching exploration

Add helper:

- `clone_tournament(tournament)` for counterfactual branches without mutating original state.

### 26.2 Monte Carlo support (phase-later)

Potential analytical helpers:

- `simulate_tournament(spec, participants, n_sims = 1000, model = NULL)`
- `champion_distribution(simulation_result)`

These can start as optional utilities after core orchestration is stable.

## 27) Live Operations Guidance

### 27.1 Real-time workflow expectations

Document operator loop:

1. Query runnable matches for active stages.
2. Enter results incrementally.
3. Advance only when stage-complete rules are satisfied.

### 27.2 Readiness helpers

`get_ready_stages()` should be paired with a match-readiness helper (or documented pattern) so users can identify what can be played now during live operations.

### 27.3 Partial progression clarity

Clarify and document:

- In-stage progress is incremental.
- Cross-stage progression is explicit via `advance()`.
- This separation is intentional for operational safety.
