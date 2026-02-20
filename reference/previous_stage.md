# Resolve source stage from the immediately preceding stage

Alias for
[`from_previous()`](https://bbtheo.github.io/bracketeer/reference/from_previous.md)
used by the rewritten stage-verb API.

## Usage

``` r
previous_stage()
```

## Value

Sentinel object to be resolved by transition wiring.

## Examples

``` r
teams <- paste("Team", LETTERS[1:8])

# Implicit: defaults to previous_stage()
trn <- tournament(teams) |>
  swiss("open", rounds = 3) |>
  single_elim("playoffs", take = top_n(4))

# Explicit: useful for branching
trn <- tournament(teams) |>
  round_robin("groups") |>
  single_elim("finals", from = previous_stage(), take = top_n(2))
```
