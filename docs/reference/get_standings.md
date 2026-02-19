# Get tournament standings

Get tournament standings

## Usage

``` r
get_standings(bracket)

# S3 method for class 'double_elim_bracket'
get_standings(bracket)

# S3 method for class 'group_stage_knockout'
get_standings(bracket)

# S3 method for class 'round_robin_bracket'
get_standings(bracket)

# S3 method for class 'single_elim_bracket'
get_standings(bracket)

# S3 method for class 'swiss_bracket'
get_standings(bracket)
```

## Arguments

- bracket:

  A bracket object

## Value

Data frame with standings

## Details

Standings are ordered using bracket-specific tiebreakers where
applicable (e.g., round robin and Swiss).
