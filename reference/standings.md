# Inspect tournament standings

Inspect tournament standings

## Usage

``` r
standings(x, stage = NULL)

# S3 method for class 'tournament'
standings(x, stage = NULL)
```

## Arguments

- x:

  A `tournament` object.

- stage:

  Optional stage identifier.

## Value

Data frame of standings.

## Examples

``` r
trn <- tournament(c("A", "B", "C", "D")) |>
  round_robin("groups")

# Enter some results
m <- matches(trn, "groups")
trn <- result(trn, "groups", m$match_id[1], score = c(2, 1))

# View current standings
standings(trn, "groups")
#>   stage_id rank participant wins draws losses points score_diff sos
#> 1   groups    1           A    1     0      0      1          1   0
#> 2   groups    2           B    0     0      0      0          0   0
#> 3   groups    3           C    0     0      0      0          0   0
#> 4   groups    4           D    0     0      1      0         -1   1
#>   head_to_head
#> 1            0
#> 2            0
#> 3            0
#> 4            0
```
