# Validate a tournament spec preflight

Validate a tournament spec preflight

## Usage

``` r
validate(x, n)
```

## Arguments

- x:

  A `bracketeer_spec` or `tournament_spec` object.

- n:

  Participant count for feasibility checks.

## Value

A preflight validation summary.

## Examples

``` r
my_spec <- spec() |>
  round_robin("groups", groups = 4) |>
  single_elim("knockout", take = top_per_group(2))

# Check if 16 participants can work
validate(my_spec, n = 16)
#> $ok
#> [1] TRUE
#> 
#> $n_participants
#> [1] 16
#> 
#> $stage_counts
#>   groups knockout 
#>       16        8 
#> 
#> attr(,"class")
#> [1] "tournament_validation"
```
