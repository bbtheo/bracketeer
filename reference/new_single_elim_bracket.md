# Internal single-elimination bracket constructor

Internal single-elimination bracket constructor

## Usage

``` r
new_single_elim_bracket(
  participants,
  seed = TRUE,
  third_place = FALSE,
  best_of = NULL,
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

- best_of:

  Optional odd-integer series length specification.

- reseed:

  Whether to reseed participants between rounds.

## Value

A single_elim_bracket object.
