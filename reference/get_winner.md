# Get tournament winner

Get tournament winner

## Usage

``` r
get_winner(bracket)

# S3 method for class 'bracket'
get_winner(bracket)

# S3 method for class 'double_elim_bracket'
get_winner(bracket)

# S3 method for class 'group_stage_knockout'
get_winner(bracket)

# S3 method for class 'round_robin_bracket'
get_winner(bracket)

# S3 method for class 'single_elim_bracket'
get_winner(bracket)

# S3 method for class 'swiss_bracket'
get_winner(bracket)

# S3 method for class 'tournament'
get_winner(bracket)
```

## Arguments

- bracket:

  A bracket object

## Value

Name of winner, or NA if tournament not complete
