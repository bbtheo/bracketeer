# Select top ranked participants per group

Select top ranked participants per group

## Usage

``` r
top_per_group(n)
```

## Arguments

- n:

  Positive integer count per group.

## Value

A `bracketeer_selector` object.

## Examples

``` r
# World Cup style: 8 groups, top 2 per group advance
teams <- paste("Team", sprintf("%02d", 1:32))
trn <- tournament(teams) |>
  round_robin("groups", groups = 8) |>
  single_elim("knockout", take = top_per_group(2))
```
