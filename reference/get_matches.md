# Get matches from a bracket

Get matches from a bracket

## Usage

``` r
get_matches(bracket, round = NULL, status = "all")

# S3 method for class 'bracket'
get_matches(bracket, round = NULL, status = "all")

# S3 method for class 'group_stage_knockout'
get_matches(bracket, round = NULL, status = "all")
```

## Arguments

- bracket:

  A bracket object

- round:

  Optional round number to filter by

- status:

  Filter by status: "pending", "complete", or "all"

## Value

Data frame of matches
