# Inspect tournament matches

Inspect tournament matches

## Usage

``` r
matches(x, stage = NULL, status = "pending")

# S3 method for class 'tournament'
matches(x, stage = NULL, status = "pending")
```

## Arguments

- x:

  A `tournament` object.

- stage:

  Optional stage identifier.

- status:

  One of `"pending"`, `"complete"`, or `"all"`.

## Value

Data frame of matches.

## Examples

``` r
trn <- tournament(c("A", "B", "C", "D")) |>
  round_robin("groups")

# Get pending matches
matches(trn, "groups")
#>   stage_id match_id compound_match_id id round position participant1
#> 1   groups        1         groups::1  1     1        1            A
#> 2   groups        2         groups::2  2     1        2            B
#> 3   groups        3         groups::3  3     2        1            A
#> 4   groups        4         groups::4  4     2        2            B
#> 5   groups        5         groups::5  5     3        1            A
#> 6   groups        6         groups::6  6     3        2            C
#>   participant2 score1 score2 winner bracket_type  status
#> 1            D     NA     NA   <NA>  round_robin pending
#> 2            C     NA     NA   <NA>  round_robin pending
#> 3            C     NA     NA   <NA>  round_robin pending
#> 4            D     NA     NA   <NA>  round_robin pending
#> 5            B     NA     NA   <NA>  round_robin pending
#> 6            D     NA     NA   <NA>  round_robin pending

# Get all matches across stages
matches(trn, status = "all")
#>   stage_id match_id compound_match_id id round position participant1
#> 1   groups        1         groups::1  1     1        1            A
#> 2   groups        2         groups::2  2     1        2            B
#> 3   groups        3         groups::3  3     2        1            A
#> 4   groups        4         groups::4  4     2        2            B
#> 5   groups        5         groups::5  5     3        1            A
#> 6   groups        6         groups::6  6     3        2            C
#>   participant2 score1 score2 winner bracket_type  status
#> 1            D     NA     NA   <NA>  round_robin pending
#> 2            C     NA     NA   <NA>  round_robin pending
#> 3            C     NA     NA   <NA>  round_robin pending
#> 4            D     NA     NA   <NA>  round_robin pending
#> 5            B     NA     NA   <NA>  round_robin pending
#> 6            D     NA     NA   <NA>  round_robin pending
```
