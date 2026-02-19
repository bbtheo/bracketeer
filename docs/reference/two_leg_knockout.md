# Create a two-leg knockout bracket

Two-leg knockout tournament with home/away legs and aggregate scoring.
Pass per-leg scores as length-2 vectors to
[`set_result()`](https://bbtheo.github.io/bracketeer/reference/set_result.md).

## Usage

``` r
two_leg_knockout(participants, ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' column.

- ...:

  Additional arguments passed to bracket constructors or tournament
  stage-verb dispatch methods.

## Value

A two_leg_knockout object
