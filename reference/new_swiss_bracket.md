# Internal swiss bracket constructor

Internal swiss bracket constructor

## Usage

``` r
new_swiss_bracket(
  participants,
  rounds = NULL,
  seed = TRUE,
  allow_ties = TRUE,
  bye_points = 1,
  best_of = NULL,
  tiebreakers = NULL
)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a `name`
  column and optional `seed` column.

- rounds:

  Optional positive integer number of Swiss rounds.

- seed:

  Seeding policy for initial ordering.

- allow_ties:

  Whether drawn results are allowed.

- bye_points:

  Points awarded for a bye.

- best_of:

  Optional odd-integer series length specification.

- tiebreakers:

  Optional ordered tiebreaker vector.

## Value

A swiss_bracket object.
