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
