# Dry-run preflight validation for tournament flow feasibility

Validates a `tournament_spec` against a participant count without
running a live tournament. This preflight catches infeasible routing
paths and stage size mismatches early.

## Usage

``` r
validate_tournament(spec, n_participants)
```

## Arguments

- spec:

  A `tournament_spec` object.

- n_participants:

  Positive integer participant count.

## Value

A `tournament_validation` summary list.
