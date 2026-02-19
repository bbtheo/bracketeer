# Set match winner directly

Record the winner of a match without specifying scores.

## Usage

``` r
set_winner(bracket, match_id, winner)

# S3 method for class 'bracket'
set_winner(bracket, match_id, winner)

# S3 method for class 'group_stage_knockout'
set_winner(bracket, match_id, winner)
```

## Arguments

- bracket:

  A bracket object

- match_id:

  The ID of the match to update

- winner:

  Name of the winning participant

## Value

Updated bracket object
