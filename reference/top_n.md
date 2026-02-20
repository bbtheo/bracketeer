# Select top ranked participants from source standings

Select top ranked participants from source standings

## Usage

``` r
top_n(n)
```

## Arguments

- n:

  Positive integer count.

## Value

A `bracketeer_selector` object.

## Examples

``` r
# Route top 4 to playoffs
trn <- tournament(paste("Team", LETTERS[1:8])) |>
  swiss("open", rounds = 3) |>
  single_elim("playoffs", take = top_n(4))
```
