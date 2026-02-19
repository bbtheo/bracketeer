# Normalize match scores (supports series)

Normalize match scores (supports series)

## Usage

``` r
normalize_match_scores(
  score1,
  score2,
  best_of = NULL,
  allow_ties = FALSE,
  participant1 = NA_character_,
  participant2 = NA_character_
)
```

## Arguments

- score1:

  Numeric score or vector of game scores for participant1

- score2:

  Numeric score or vector of game scores for participant2

- best_of:

  Optional best-of value

- allow_ties:

  Logical, whether ties are allowed

## Value

List with score1, score2, winner, loser, games
