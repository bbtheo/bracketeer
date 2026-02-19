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
