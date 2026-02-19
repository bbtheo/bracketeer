# Advance tournament to next round

Check if current round is complete and update bracket state.

## Usage

``` r
advance(x, stage = NULL, ...)

# S3 method for class 'bracket'
advance(x, stage = NULL, ...)

# S3 method for class 'double_elim_bracket'
advance(bracket, stage_id = NULL)

# S3 method for class 'group_stage_knockout'
advance(bracket, stage_id = NULL)

# S3 method for class 'single_elim_bracket'
advance(bracket, stage_id = NULL)

# S3 method for class 'swiss_bracket'
advance(bracket, stage_id = NULL)

# S3 method for class 'tournament'
advance(x, stage = NULL, ...)
```

## Arguments

- x:

  A bracket object.

- stage:

  Optional stage identifier for tournament methods.

- ...:

  Additional method-specific arguments.

## Value

Updated bracket object
