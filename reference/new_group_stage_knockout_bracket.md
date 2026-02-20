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

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a `name`
  column and optional `seed` column.

- groups:

  Number of groups to create.

- advance_per_group:

  Number of participants advancing from each group.

- seed:

  Seeding policy for initial participant allocation.

- group_home_away:

  Whether group matches are home/away double round robin.

- group_best_of:

  Optional odd-integer series length for group matches.

- group_tiebreakers:

  Optional ordered tiebreaker vector for groups.

- knockout_type:

  Knockout format: `"single_elim"` or `"double_elim"`.

- knockout_seed:

  Seeding policy for knockout-stage placement.

- third_place:

  Whether to include a third-place match in single elimination.

- grand_final_reset:

  Whether double-elim knockout can trigger a reset final.

- knockout_best_of:

  Optional odd-integer series length for knockout matches.

## Value

A group_stage_knockout object.
