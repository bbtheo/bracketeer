# Add a transition between stages

Add a transition between stages

## Usage

``` r
add_transition(
  spec,
  from,
  to,
  rule = NULL,
  seeding = "by_source_rank",
  take = NULL,
  priority = 1L,
  consume = TRUE,
  allow_overlap = FALSE,
  transition_id = NULL
)
```

## Arguments

- spec:

  A `tournament_spec` object.

- from:

  Source stage ID, or
  [`from_previous()`](https://bbtheo.github.io/bracketeer/reference/from_previous.md).

- to:

  Destination stage ID.

- rule:

  Transition rule object (optional for MVP graph wiring).

- seeding:

  Seeding policy label.

- take:

  Selector object for transition participant selection.

- priority:

  Transition resolution priority.

- consume:

  Whether selected participants are consumed.

- allow_overlap:

  Whether overlap is allowed across transitions.

- transition_id:

  Transition ID. If `NULL`, deterministic auto-ID is used.

## Value

Updated `tournament_spec`.
