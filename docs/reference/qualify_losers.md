# Select losers from a source stage by elimination round

Returns a transition rule function intended for use with
[`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md).

## Usage

``` r
qualify_losers(round = "all", stage = NULL, ordering = "elimination_round")
```

## Arguments

- round:

  One of `"all"`, `"latest"`, or an integer vector of elimination rounds
  to include.

- stage:

  Optional stage selector (reserved for future use).

- ordering:

  Ordering mode: `"elimination_round"`, `"source_seed"`, or
  `"as_recorded"`.

## Value

A transition rule function.
