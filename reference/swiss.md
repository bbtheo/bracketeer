# Create a Swiss-system tournament

Swiss system pairs participants by similar records each round.

## Usage

``` r
swiss(participants, ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' column.

- ...:

  Additional arguments passed to bracket constructors or tournament
  stage-verb dispatch methods.

## Value

A swiss_bracket object
