# Get tournament winner

Get tournament winner

## Usage

``` r
winner(tournament)
```

## Arguments

- tournament:

  A `tournament` object.

## Value

Winner name or `NA_character_`.

## Examples

``` r
teams <- c("A", "B", "C", "D")
trn <- tournament(teams) |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(2))

# ... enter all results ...

# Get the champion
winner(trn)
#> [1] NA
```
