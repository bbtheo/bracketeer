# Generate standard seeding order for bracket

Creates seeding positions so that seed 1 vs seed N is in the final if
both win all matches. Uses standard bracket seeding algorithm.

## Usage

``` r
generate_seed_order(n)
```

## Arguments

- n:

  Number of slots (must be power of 2)

## Value

Vector of seed positions
