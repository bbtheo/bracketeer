# Create a round robin tournament

Round robin tournament where each participant plays every other
participant.

## Usage

``` r
round_robin(participants, ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' column.

- ...:

  Additional arguments passed to bracket constructors or tournament
  stage-verb dispatch methods.

## Value

A round_robin_bracket object

## Examples

``` r
# Simple round robin
trn <- tournament(c("A", "B", "C", "D")) |>
  round_robin("groups")

# Multiple groups (World Cup style)
teams <- paste("Team", sprintf("%02d", 1:32))
trn <- tournament(teams) |>
  round_robin("groups", groups = 8)
```
