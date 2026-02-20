# Internal two-leg bracket constructor

Internal two-leg bracket constructor

## Usage

``` r
new_two_leg_bracket(
  participants,
  seed = TRUE,
  third_place = FALSE,
  away_goals = TRUE,
  reseed = FALSE
)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a `name`
  column and optional `seed` column.

- seed:

  Seeding policy forwarded to internal seeding helpers.

- third_place:

  Whether to include a third-place match.

- away_goals:

  Whether away goals break aggregate-score ties.

- reseed:

  Whether to reseed participants between rounds.

## Value

A two_leg_knockout object.
