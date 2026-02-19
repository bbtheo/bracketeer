# Add multiple transitions from one source stage

Convenience sugar for branching stage fan-out. Compiles into
deterministic
[`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md)
calls.

## Usage

``` r
split_stage(
  spec,
  from,
  into,
  priority_start = 1L,
  consume = TRUE,
  allow_overlap = FALSE,
  seeding = "by_source_rank"
)
```

## Arguments

- spec:

  A `tournament_spec` object.

- from:

  Source stage ID, or
  [`from_previous()`](https://bbtheo.github.io/bracketeer/reference/from_previous.md).

- into:

  Named list mapping destination stage IDs to transition rules, or
  branch configs with a required `rule` field.

- priority_start:

  Starting priority for branch transitions when a branch does not
  explicitly provide `priority`.

- consume:

  Default `consume` value for branches.

- allow_overlap:

  Default `allow_overlap` value for branches.

- seeding:

  Default seeding policy for branches.

## Value

Updated `tournament_spec`.
