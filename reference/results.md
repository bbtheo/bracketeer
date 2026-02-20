# Fluent tournament batch result entry helper

Convenience wrapper for entering multiple match results for one stage.

## Usage

``` r
results(tournament, stage, df, overwrite = FALSE, auto_advance = NULL)
```

## Arguments

- tournament:

  A `tournament` object.

- stage:

  Stage identifier containing the matches.

- df:

  Data frame with required columns: `match`, `score1`, `score2`.

- overwrite:

  Logical; forwards to `result(..., overwrite = ...)`.

- auto_advance:

  Optional logical override for the final row. If `NULL`, defaults to
  the tournament's `auto_advance` setting when present.

## Value

Updated `tournament` object.

## Examples

``` r
teams <- c("A", "B", "C", "D")
trn <- tournament(teams) |>
  round_robin("groups")

m <- matches(trn, "groups")
trn <- results(trn, "groups", data.frame(
  match  = m$match_id,
  score1 = c(2, 1, 3),
  score2 = c(1, 2, 0)
))
```
