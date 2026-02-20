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

## Examples

``` r
# Simple tournament with auto-advance
teams <- c("Lions", "Bears", "Eagles", "Wolves")
trn <- tournament(teams) |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(2))

# Manual advance mode
trn_manual <- tournament(teams, auto_advance = FALSE) |>
  swiss("open", rounds = 3)
```
