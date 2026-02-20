# Internal round-robin bracket constructor

Internal round-robin bracket constructor

## Usage

``` r
new_round_robin_bracket(
  participants,
  home_away = FALSE,
  n_rounds = NULL,
  best_of = NULL,
  tiebreakers = NULL,
  groups = NULL
)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a `name`
  column and optional `seed` column.

- home_away:

  Whether to schedule home/away mirrored pairings.

- n_rounds:

  Optional positive integer number of round-robin cycles.

- best_of:

  Optional odd-integer series length specification.

- tiebreakers:

  Optional ordered tiebreaker vector.

- groups:

  Optional positive integer number of groups.

## Value

A round_robin_bracket object.
