# Create a single elimination bracket

Single elimination (knockout) tournament where losing a match eliminates
the participant from the tournament.

## Usage

``` r
single_elim(participants, ...)
```

## Arguments

- participants:

  Character vector of participant names, or a data.frame with a 'name'
  column and optional 'seed' column.

- ...:

  Additional arguments passed to bracket constructors or tournament
  stage-verb dispatch methods.

## Value

A single_elim_bracket object

## Examples

``` r
# Simple knockout bracket
trn <- tournament(paste("Team", LETTERS[1:8])) |>
  single_elim("bracket")

# Chain after group stage
trn <- tournament(c("A", "B", "C", "D")) |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(2))
```
