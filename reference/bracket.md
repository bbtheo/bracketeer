# Create a tournament bracket

Generic constructor for tournament brackets. Dispatches to specific
tournament type constructors.

## Usage

``` r
bracket(participants, type = "single_elim", ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' or 'rating' columns.

- type:

  Tournament type: "single_elim", "double_elim", "round_robin", "swiss",
  "group_stage_knockout", or "two_leg_knockout"

- ...:

  Additional arguments passed to type-specific constructor

## Value

A bracket object
