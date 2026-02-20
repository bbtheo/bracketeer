# Internal double-elimination bracket constructor

Internal double-elimination bracket constructor

## Usage

``` r
new_double_elim_bracket(
  participants,
  seed = TRUE,
  grand_final_reset = TRUE,
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

- grand_final_reset:

  Whether to schedule a potential reset grand final.

- best_of:

  Optional odd-integer series length specification.

- reseed:

  Whether to reseed winners-bracket participants between rounds.

## Value

A double_elim_bracket object.
