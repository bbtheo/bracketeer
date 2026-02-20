# Create a double elimination bracket

Double elimination tournament with winners and losers brackets.

## Usage

``` r
double_elim(participants, ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' column.

- ...:

  Additional arguments passed to bracket constructors or tournament
  stage-verb dispatch methods.

## Value

A double_elim_bracket object

## Examples

``` r
# Double elimination bracket (two losses to be eliminated)
trn <- tournament(paste("Team", LETTERS[1:8])) |>
  double_elim("bracket")

# After Swiss rounds
trn <- tournament(paste("Team", LETTERS[1:16])) |>
  swiss("open", rounds = 4) |>
  double_elim("playoffs", take = top_n(8))
```
