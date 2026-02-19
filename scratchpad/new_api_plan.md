# Tidymodels-Style API Plan for Tournament Dispatch

## Goals
- Provide a consistent, composable API that mirrors tidymodels patterns (specification + fit/build + extract).
- Preserve existing constructors while offering a unified entry point for new workflows.
- Keep base R, S3-first design with explicit, predictable behavior.

## High-Level Concept
- **Specification objects**: describe a tournament configuration without building it.
- **Build/fit**: realize a bracket from a specification and participants.
- **Dispatch**: method-based dispatch on spec class to create the correct bracket type.

## Proposed User Experience

### 1) Create a spec
```r
bracket_spec() %>%
  single_elim_spec(seed = "standard", third_place = TRUE)
```

### 2) Build with participants
```r
spec <- single_elim_spec(seed = "standard", third_place = TRUE)
bracket <- build_bracket(spec, participants)
```

### 3) Use standard verbs
```r
bracket <- set_result(bracket, 1, 3, 1)
get_matches(bracket)
```

## API Surface

### Core spec constructors
- `bracket_spec()`
- `single_elim_spec(seed = TRUE, third_place = FALSE, best_of = NULL, reseed = FALSE)`
- `double_elim_spec(seed = TRUE, grand_final_reset = TRUE, best_of = NULL, reseed = FALSE)`
- `round_robin_spec(home_away = FALSE, best_of = NULL, tiebreakers = NULL)`
- `swiss_spec(rounds = NULL, seed = TRUE, allow_ties = TRUE, bye_points = 1, best_of = NULL, tiebreakers = NULL)`
- `group_stage_knockout_spec(groups = 2, advance_per_group = 2, seed = TRUE, group_home_away = FALSE, group_best_of = NULL, group_tiebreakers = NULL, knockout_type = "single_elim", knockout_seed = TRUE, third_place = FALSE, grand_final_reset = TRUE, knockout_best_of = NULL)`
- `two_leg_knockout_spec(seed = TRUE, third_place = FALSE, away_goals = TRUE, reseed = FALSE)`

### Build and finalize
- `build_bracket(spec, participants, ...)` (S3 generic)
- `fit_bracket(spec, participants, ...)` (alias to `build_bracket()` to match tidymodels vocabulary)
- `update_spec(spec, ...)` (modify parameters)

## Dispatch Strategy

### S3 Classes
- `bracket_spec` base class
- Specific subclasses:
  - `single_elim_spec`, `double_elim_spec`, `round_robin_spec`, `swiss_spec`,
    `group_stage_knockout_spec`, `two_leg_knockout_spec`

### Generics
- `build_bracket()` (generic)
- `fit_bracket()` (generic)
- `update_spec()` (generic)

### Methods
- `build_bracket.single_elim_spec()` -> calls `single_elim()`
- `build_bracket.double_elim_spec()` -> calls `double_elim()`
- `build_bracket.round_robin_spec()` -> calls `round_robin()`
- `build_bracket.swiss_spec()` -> calls `swiss()`
- `build_bracket.group_stage_knockout_spec()` -> calls `group_stage_knockout()`
- `build_bracket.two_leg_knockout_spec()` -> calls `two_leg_knockout()`

### Spec Storage
- Each spec stores:
  - `type` (string), `params` (list), `options` (list), `engine` (optional future use)
- `print.bracket_spec()` summarizes type and params.

## Compatibility and Migration
- Keep all current constructors unchanged.
- Add spec constructors and `build_bracket()`; existing workflows remain valid.
- Add `bracket()` to accept specs directly:
  - `bracket(spec, participants)` detects spec class and forwards to `build_bracket()`.

## Testing Strategy
- Add tests for each spec:
  - `build_bracket()` returns correct class and structure.
  - `update_spec()` modifies params.
  - `bracket(spec, participants)` works for each format.

## Documentation
- Add a new vignette: `tidymodels-style-api.Rmd`.
- Update README to show spec usage alongside constructors.

## Future Extensions
- Optional `engine` concept for plotting or external schedulers.
- Add `tune_spec()` hooks for parameter tuning (rounds, tiebreakers, etc.).
- Add `validate_spec()` for early error checking before build.
