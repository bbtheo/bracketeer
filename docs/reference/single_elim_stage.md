# Create a stage specification

Stage specifications describe how to materialize a stage bracket from a
participant set inside a `tournament_spec` graph.

## Usage

``` r
single_elim_stage(
  seed = TRUE,
  third_place = FALSE,
  best_of = NULL,
  reseed = FALSE
)

double_elim_stage(
  seed = TRUE,
  grand_final_reset = TRUE,
  best_of = NULL,
  reseed = FALSE
)

round_robin_stage(
  home_away = FALSE,
  n_rounds = NULL,
  best_of = NULL,
  tiebreakers = NULL,
  groups = NULL
)

swiss_stage(
  rounds = NULL,
  seed = TRUE,
  allow_ties = TRUE,
  bye_points = 1,
  best_of = NULL,
  tiebreakers = NULL
)

group_stage_knockout_stage(
  groups = 2,
  advance_per_group = 2,
  seed = TRUE,
  group_home_away = FALSE,
  group_best_of = NULL,
  group_tiebreakers = NULL,
  knockout_type = "single_elim",
  knockout_seed = TRUE,
  third_place = FALSE,
  grand_final_reset = TRUE,
  knockout_best_of = NULL
)

two_leg_stage(
  seed = TRUE,
  third_place = FALSE,
  away_goals = TRUE,
  reseed = FALSE
)
```

## Arguments

- seed:

  Logical or character seed method.

- third_place:

  Logical; include third-place match.

- best_of:

  Optional best-of value (must be odd).

- reseed:

  Logical; reseed between rounds for supported formats.

- grand_final_reset:

  Logical; allow grand-final reset.

- home_away:

  Logical; whether repeated pairings alternate home/away.

- n_rounds:

  Number of round-robin cycles.

- tiebreakers:

  Ordered tiebreakers.

- groups:

  Number of groups.

- rounds:

  Number of Swiss rounds.

- allow_ties:

  Logical; whether ties are allowed.

- bye_points:

  Points awarded for a bye.

- advance_per_group:

  Number of qualifiers per group.

- group_home_away:

  Logical; home/away behavior in groups.

- group_best_of:

  Optional best-of in groups.

- group_tiebreakers:

  Ordered group-stage tiebreakers.

- knockout_type:

  Knockout format: `"single_elim"` or `"double_elim"`.

- knockout_seed:

  Logical or character seed method for knockout.

- knockout_best_of:

  Optional knockout best-of value.

- away_goals:

  Logical; enable away-goals tiebreaker.

## Value

A `stage_spec` object.
