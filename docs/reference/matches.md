# Inspect tournament matches

Inspect tournament matches

## Usage

``` r
matches(x, stage = NULL, status = "pending")

# S3 method for class 'tournament'
matches(x, stage = NULL, status = "pending")
```

## Arguments

- x:

  A `tournament` object.

- stage:

  Optional stage identifier.

- status:

  One of `"pending"`, `"complete"`, or `"all"`.

## Value

Data frame of matches.
