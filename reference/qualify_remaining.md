# Select all entrants remaining in the transition source pool

Returns a transition rule function intended for use with
[`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md).
During
[`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md),
this selects all participants still available from the source stage
after higher-priority consuming transitions have resolved.

## Usage

``` r
qualify_remaining()
```

## Value

A transition rule function.
