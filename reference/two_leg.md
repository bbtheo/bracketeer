# Create a two-leg stage or bracket

Alias for
[`two_leg_knockout()`](https://bbtheo.github.io/bracketeer/reference/two_leg_knockout.md)
used by the tournament stage-verb API.

## Usage

``` r
two_leg(participants, ...)
```

## Arguments

- participants:

  Participants, a spec object, or a tournament object.

- ...:

  Additional arguments forwarded to
  [`two_leg_knockout()`](https://bbtheo.github.io/bracketeer/reference/two_leg_knockout.md).

## Value

A bracket, spec, or tournament depending on `participants`.

## Examples

``` r
# Two-leg knockout (Champions League style)
teams <- paste("Club", sprintf("%02d", 1:16))
trn <- tournament(teams) |>
  round_robin("groups", groups = 4) |>
  two_leg("knockouts", take = top_per_group(2))
```
