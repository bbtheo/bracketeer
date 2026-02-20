# Fluent tournament result entry helper

Convenience wrapper around
[`set_result()`](https://bbtheo.github.io/bracketeer/reference/set_result.md)
for tournament workflows.

## Usage

``` r
result(tournament, stage, match, score, overwrite = FALSE, auto_advance = NULL)
```

## Arguments

- tournament:

  A `tournament` object.

- stage:

  Stage identifier containing the match.

- match:

  Match identifier inside `stage`.

- score:

  Numeric vector score payload. For a single match, pass
  `c(score1, score2)`.

- overwrite:

  Logical; forwards to `set_result(..., overwrite = ...)`.

- auto_advance:

  Optional logical override. If `NULL`, defaults to the tournament's
  `auto_advance` setting when present.

## Value

Updated `tournament` object.

## Examples

``` r
teams <- c("A", "B", "C", "D")
trn <- tournament(teams) |>
  round_robin("groups")

# Enter a single result
trn <- result(trn, "groups", match = 1, score = c(2, 1))
```
