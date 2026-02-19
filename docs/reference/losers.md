# Select losers from a source stage by elimination round

Select losers from a source stage by elimination round

## Usage

``` r
losers(round = "all", stage = NULL, ordering = "elimination_round")
```

## Arguments

- round:

  One of `"all"`, `"latest"`, or an integer vector of rounds.

- stage:

  Optional stage selector (reserved for future use).

- ordering:

  Ordering mode: `"elimination_round"`, `"source_seed"`, or
  `"as_recorded"`.

## Value

A `bracketeer_selector` object.
