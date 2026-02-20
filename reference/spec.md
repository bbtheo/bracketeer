# Create a bracketeer tournament specification

Create a bracketeer tournament specification

## Usage

``` r
spec()
```

## Value

A `bracketeer_spec` object.

## Examples

``` r
# Create a reusable tournament blueprint
my_spec <- spec() |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(2))

# Build with different participant lists
trn1 <- build(my_spec, c("A", "B", "C", "D"))
trn2 <- build(my_spec, c("W", "X", "Y", "Z"))
```
