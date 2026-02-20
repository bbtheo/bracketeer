# Build a live tournament runtime from a specification

Build a live tournament runtime from a specification

## Usage

``` r
build(x, participants)
```

## Arguments

- x:

  A `bracketeer_spec` or `tournament_spec` object.

- participants:

  Character vector of participant names, or a data.frame with a `name`
  column.

## Value

A tournament runtime object.

## Examples

``` r
my_spec <- spec() |>
  swiss("open", rounds = 3) |>
  single_elim("playoffs", take = top_n(4))

# Materialize with participants
trn <- build(my_spec, paste("Team", LETTERS[1:8]))
```
