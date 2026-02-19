# Teardown tournament state

For tournament runtimes, this un-materializes a stage and its downstream
dependents so upstream results can be corrected and replayed.

## Usage

``` r
teardown(x, stage = NULL, ...)

# S3 method for class 'bracket'
teardown(x, stage = NULL, ...)

# S3 method for class 'tournament'
teardown(x, stage = NULL, ...)
```

## Arguments

- x:

  A bracket or tournament object.

- stage:

  Stage identifier to teardown (tournament method).

- ...:

  Additional method-specific arguments.

## Value

Updated object.
