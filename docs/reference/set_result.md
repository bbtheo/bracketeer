# Set match result by scores

Record the score for a match. The winner is determined by the higher
score.

## Usage

``` r
set_result(
  bracket,
  match_id,
  score1,
  score2,
  stage_id = NULL,
  overwrite = FALSE,
  auto_advance = FALSE
)

# S3 method for class 'bracket'
set_result(
  bracket,
  match_id,
  score1,
  score2,
  stage_id = NULL,
  overwrite = FALSE,
  auto_advance = FALSE
)

# S3 method for class 'group_stage_knockout'
set_result(
  bracket,
  match_id,
  score1,
  score2,
  stage_id = NULL,
  overwrite = FALSE,
  auto_advance = FALSE
)

# S3 method for class 'round_robin_bracket'
set_result(
  bracket,
  match_id,
  score1,
  score2,
  stage_id = NULL,
  overwrite = FALSE,
  auto_advance = FALSE
)

# S3 method for class 'swiss_bracket'
set_result(
  bracket,
  match_id,
  score1,
  score2,
  stage_id = NULL,
  overwrite = FALSE,
  auto_advance = FALSE
)

# S3 method for class 'tournament'
set_result(
  bracket,
  match_id,
  score1,
  score2,
  stage_id = NULL,
  overwrite = FALSE,
  auto_advance = TRUE
)
```

## Arguments

- bracket:

  A bracket object

- match_id:

  The ID of the match to update

- score1:

  Score for participant 1, or a numeric vector of game scores.

- score2:

  Score for participant 2, or a numeric vector of game scores.

- stage_id:

  Optional stage identifier used by multi-stage tournament runtimes.

- overwrite:

  Logical; when `TRUE`, requests explicit result overwrite handling
  where supported.

- auto_advance:

  Logical; when `TRUE` and supported by the bracket type, automatically
  advances completed stages.

## Value

Updated bracket object
