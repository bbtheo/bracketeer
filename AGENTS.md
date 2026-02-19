# AGENTS

- Before implementing any feature or fixing any bug in package
  functionality, add or update a test that reproduces the behavior, then
  implement the change.
- Documentation-only changes (README, vignettes, comments, prose) do not
  require adding or updating tests unless they also change functional
  package behavior.
- Never edit `README.md` directly. Edit `README.qmd` only, then
  render/knit it to regenerate `README.md`.
- Never edit `NAMESPACE` or `man/*.Rd` files by hand. Update roxygen
  comments in `R/*.R` and regenerate package documentation with
  `devtools::document()`.
- Whenever the user asks for a plan, write the plan to a file in the
  `scratchpad/` directory.
- This repo is an R package (`bracketeer`) focused on base R with no
  required dependencies.
- The codebase uses S3 classes for brackets and matches.
- Use `snake_case` for function names and arguments.
- Functions should return modified bracket objects (functional style),
  not mutate in place.
- `best_of` values must be odd integers;
  [`set_result()`](https://bbtheo.github.io/bracketeer/reference/set_result.md)
  supports either series wins or per-game score vectors.
- Byes are only auto-resolved in round 1.
- Reseeding (single elim and winners bracket in double elim) requires
  calling
  [`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
  after a completed round.
- Group stage + knockout requires calling
  [`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
  after all group matches complete to build the knockout bracket.
- Two-leg knockout uses aggregate scoring with optional away-goals
  tiebreaker.
- Round robin and Swiss use configurable tiebreakers; supported values
  include: `points`, `wins`, `draws`, `losses`, `score_for`,
  `score_against`, `score_diff`, `sos`, `buchholz`, `head_to_head`,
  `alphabetical`.
- Tests use `testthat`; run them with `devtools::test()`.

## Multi-stage Tournament DSL Conventions

- Keep stage definition and wiring separate:
  - [`add_stage()`](https://bbtheo.github.io/bracketeer/reference/add_stage.md)
    defines stage structure only.
  - [`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md)
    defines cross-stage participant routing.
- Prefer explicit
  [`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md)
  over embedding transition logic inside stage constructors.
- Use `set_outcome(track_placements = n)` semantics for output ranking
  depth.
- Runtime should support parallel-ready stages via `active_stage_ids`
  (plural), not a singular active stage.
- `advance(stage_id = ...)` should resolve all eligible outgoing
  transitions from that source and materialize all ready downstream
  stages in the same call.
- Transition IDs must be deterministic and human-readable:
  - default format `"{from}_to_{to}"`, deterministic numeric suffix on
    collisions.
- Transition consumption semantics should use `consume = TRUE/FALSE`
  (intent-based naming).
- For branching sugar,
  [`split_stage()`](https://bbtheo.github.io/bracketeer/reference/split_stage.md)
  should compile into deterministic
  [`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md)
  calls.
- [`double_elim_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  stays monolithic for MVP/Phase-2; composable double-elimination is
  future scope.

## Match Addressing and Result Entry

- Use unambiguous compound IDs with `::` separator:
  `"stage_id::match_id"`.
- Reserve `::` and do not allow it inside stage IDs or match IDs.
- [`result()`](https://bbtheo.github.io/bracketeer/reference/result.md)
  helper should use neutral score naming (`score1`, `score2`) rather
  than domain-specific naming.

## Transition Rules and Seeding

- `qualify_top(n, per_group = TRUE/FALSE)` is the primary top-cut
  selector.
- [`qualify_remaining()`](https://bbtheo.github.io/bracketeer/reference/qualify_remaining.md)
  is the preferred complement selector for winners/consolation splits.
- [`qualify_losers()`](https://bbtheo.github.io/bracketeer/reference/qualify_losers.md)
  must support explicit round semantics (`"all"`, `"latest"`, integer
  vector rounds) and deterministic ordering.
- Document seeding policy behavior explicitly:
  - `"by_source_rank"` = rank buckets by placement (all 1st, then all
    2nd, …).
  - `"cross_group"` = global ranking across qualifiers by configured
    tiebreakers.

## Validation and Operational Policy

- Use deterministic acyclic graph validation (Kahn topological sort) for
  stage/transition DAG checks.
- Provide preflight validation for feasibility before runtime
  (`validate_tournament(spec, n_participants)`).
- Error messages must include identifiers (`stage_id`, `transition_id`,
  `match_id` where relevant) and suggested fixes.
- Result correction policy:
  - default: cannot overwrite completed matches,
  - allow explicit override only when safe (`overwrite = TRUE`) and
    before dependent stages are materialized.
- [`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
  is irreversible in MVP (no rollback API unless explicitly implemented
  later).

## S3 Class Hierarchy

- `tournament_spec`: Configuration object with `stages`, `edges`,
  `outcome`, `metadata`. Class: `c("tournament_spec", "bracket_spec")`.
- `stage_spec`: Individual stage definitions from
  [`round_robin_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md),
  [`swiss_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md),
  [`single_elim_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md),
  [`double_elim_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md),
  [`two_leg_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md).
- `transition_spec`: Edge definitions with `transition_id`,
  `from_stage_id`, `to_stage_id`, `rule`, `seeding`, `priority`,
  `consume`, `allow_overlap`.
- `tournament`: Runtime object with `spec`, `participants`,
  `stage_state`, `active_stage_ids`, `completed`, `rankings`,
  `routing_log`.

## Routing Log Schema

Each transition resolution appends a log entry:

``` r
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

Access via `get_routing_log(tournament)`.

## Group Allocation

- `groups = 6` is ergonomic shorthand for balanced random groups.
- `groups = group_allocation("seeded", 6)` for seeded pot-based
  allocation.
- `groups = group_allocation("manual", list(A = c(...), B = c(...)))`
  for explicit assignment.
- `groups = c(5, 4, 4, 4)` for explicit unequal sizes.
- Validation must produce actionable error messages with repair hints.

## Implementation Sprint Order

**Sprint 1 (Foundation):** -
[`tournament_spec()`](https://bbtheo.github.io/bracketeer/reference/tournament_spec.md)
constructor -
[`add_stage()`](https://bbtheo.github.io/bracketeer/reference/add_stage.md)
with validation -
[`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md)
with validation - Kahn-based cycle detection - `print.tournament_spec()`
ASCII graph

**Sprint 2 (Linear Runtime):** -
[`build_tournament()`](https://bbtheo.github.io/bracketeer/reference/build_tournament.md)
for source stages -
[`set_result.tournament()`](https://bbtheo.github.io/bracketeer/reference/set_result.md)
with stage routing -
[`advance.tournament()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
for single downstream -
[`is_stage_complete()`](https://bbtheo.github.io/bracketeer/reference/is_stage_complete.md)
generic - `track_placements` ranking output

**Sprint 3 (Branching Core):** - `active_stage_ids` and
[`get_ready_stages()`](https://bbtheo.github.io/bracketeer/reference/get_ready_stages.md) -
Multi-transition fan-out in one
[`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
call - `consume` semantics -
[`qualify_remaining()`](https://bbtheo.github.io/bracketeer/reference/qualify_remaining.md) -
Transition conflict detection

**Sprint 4 (Convenience & Polish):** -
[`split_stage()`](https://bbtheo.github.io/bracketeer/reference/split_stage.md)
sugar - `qualify_losers(round, ordering)` -
[`validate_tournament()`](https://bbtheo.github.io/bracketeer/reference/validate_tournament.md)
dry-run preflight -
[`result()`](https://bbtheo.github.io/bracketeer/reference/result.md)
fluent helper - Routing log +
[`get_routing_log()`](https://bbtheo.github.io/bracketeer/reference/get_routing_log.md) -
Error catalog documentation

## Test File Structure (New)

- `test-tournament-spec-graph.R`: Spec construction, validation, cycle
  detection.
- `test-tournament-stage-transition.R`: Linear flow execution.
- `test-tournament-match-addressing.R`: Compound ID parsing, `::`
  separator.
- `test-tournament-active-stages.R`: Parallel stage readiness.
- `test-tournament-branching.R`: Fan-out, consume semantics.
- `test-tournament-transition-conflicts.R`: Overlap detection.
- `test-tournament-qualify-losers.R`: Loser routing rules.
- `test-tournament-validation-dry-run.R`: Preflight validation.
- `test-tournament-routing-log.R`: Audit log correctness.

## Key Generics and Helpers

New generics: -
[`build_tournament()`](https://bbtheo.github.io/bracketeer/reference/build_tournament.md),
[`validate_tournament_spec()`](https://bbtheo.github.io/bracketeer/reference/validate_tournament_spec.md),
[`validate_tournament()`](https://bbtheo.github.io/bracketeer/reference/validate_tournament.md) -
[`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md),
[`split_stage()`](https://bbtheo.github.io/bracketeer/reference/split_stage.md),
[`from_previous()`](https://bbtheo.github.io/bracketeer/reference/from_previous.md) -
`resolve_transition()`, `materialize_stage()`,
[`is_stage_complete()`](https://bbtheo.github.io/bracketeer/reference/is_stage_complete.md) -
[`get_ready_stages()`](https://bbtheo.github.io/bracketeer/reference/get_ready_stages.md),
[`get_routing_log()`](https://bbtheo.github.io/bracketeer/reference/get_routing_log.md),
[`compute_tournament_rankings()`](https://bbtheo.github.io/bracketeer/reference/compute_tournament_rankings.md)

Existing generics (reused for tournament class): -
[`set_result()`](https://bbtheo.github.io/bracketeer/reference/set_result.md),
[`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md),
[`get_matches()`](https://bbtheo.github.io/bracketeer/reference/get_matches.md),
[`get_standings()`](https://bbtheo.github.io/bracketeer/reference/get_standings.md)
