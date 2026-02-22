# Create an empty live tournament pipeline

Create an empty live tournament pipeline

## Usage

``` r
tournament(participants, auto_advance = TRUE)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a `name`
  column.

- auto_advance:

  Logical scalar. Stored as the runtime default for future result-entry
  helpers.

## Value

A tournament runtime object with no stages materialized yet.
