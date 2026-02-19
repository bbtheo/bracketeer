# Internal group-stage-knockout bracket constructor

Internal group-stage-knockout bracket constructor

## Usage

``` r
new_group_stage_knockout_bracket(
  participants,
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
```

## Value

A group_stage_knockout object.
