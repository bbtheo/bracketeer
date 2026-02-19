# Create a group stage followed by knockout bracket

Create a group stage followed by knockout bracket

## Usage

``` r
group_stage_knockout(participants, ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' column.

- ...:

  Additional arguments passed to bracket constructors or tournament
  stage-verb dispatch methods.

## Value

A group_stage_knockout object
