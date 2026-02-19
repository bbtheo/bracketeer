

<!-- badges: start -->

<!-- badges: end -->

# bracketeer <img src="man/figures/logo.png" align="right" height="150" />

**Model and run tournament competitions in R.**

bracketeer has a pipe-first API: stage types are verbs, you chain them
to describe the structure, then drive it live with result entry.
Downstream stages materialize automatically when their source completes.

``` r
library(bracketeer)

teams <- paste("Team", LETTERS[1:16])

tournament(teams) |>
  swiss("open", rounds = 5) |>
  single_elim("playoffs", take = top_n(8))
```

    Tournament [2 stages]
      open         in_progress  0/8 matches
      playoffs     blocked      0/0 matches

The definition reads like a rulebook. The runtime feels like a
scoreboard.

## Installation

``` r
# install.packages("pak")
pak::pak("bbtheo/bracketeer")
```

Or try it right now without installing anything — open the World Cup
2026 simulation notebook in Google Colab:

[![](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/bbtheo/bracketeer/blob/main/notebooks/world_cup_2026.ipynb)

## A complete tournament

Four teams. A group stage, then a final for the top two. Definition to
champion in one session.

``` r
teams <- c("Lions", "Bears", "Eagles", "Wolves")

trn <- tournament(teams) |>
  round_robin("groups") |>
  single_elim("grand_final", take = top_n(2))

trn
```

    Tournament [2 stages]
      groups       in_progress  0/6 matches
      grand_final  blocked      0/0 matches

`groups` opens immediately. `grand_final` is blocked until two teams
qualify. Use `matches()` to get the schedule:

``` r
m <- matches(trn, "groups")

trn <- results(trn, "groups", data.frame(
  match  = m$match_id,
  score1 = c(2, 1, 3, 1, 2, 1),
  score2 = c(1, 2, 1, 0, 0, 0)
))

trn
```

    Tournament [2 stages]
      groups       complete     6/6 matches
      grand_final  in_progress  0/1 matches

When the last group result lands, `grand_final` materializes
automatically. No explicit advance call needed.

``` r
standings(trn, "groups")
```

      stage_id rank participant wins draws losses points score_diff sos
    1   groups    1       Bears    3     0      0      3          4   3
    2   groups    2       Lions    2     0      1      2          1   4
    3   groups    3      Eagles    1     0      2      1         -2   5
    4   groups    4      Wolves    0     0      3      0         -3   6
      head_to_head
    1            0
    2            0
    3            0
    4            0

``` r
final_m <- matches(trn, "grand_final")

trn <- result(trn, "grand_final",
  match = final_m$match_id[[1]],
  score = c(3, 1)
)

winner(trn)
```

    [1] "Lions"

## Define any structure

Stage types are the verbs. The `from =` argument defaults to the
previous stage, so linear chains need no wiring at all.

``` r
# Swiss → top 8 playoffs (common esports format)
tournament(teams) |>
  swiss("open", rounds = 3) |>
  single_elim("playoffs", take = top_n(2))
```

    Tournament [2 stages]
      open         in_progress  0/2 matches
      playoffs     blocked      0/0 matches

When two stages read from the same source, name it explicitly with
`from =`:

``` r
# Championship track + consolation bracket from the same group stage
tournament(teams) |>
  round_robin("groups") |>
  single_elim("championship", from = "groups", take = top_n(2)) |>
  single_elim("consolation",  from = "groups", take = remaining())
```

    Tournament [3 stages]
      groups       in_progress  0/6 matches
      championship blocked      0/0 matches
      consolation  blocked      0/0 matches

``` r
# World Cup: 8 groups of 4, top 2 from each group into the knockouts
teams_32 <- paste("Nation", sprintf("%02d", 1:32))

tournament(teams_32) |>
  round_robin("groups", groups = 8) |>
  single_elim("round_of_16", take = top_per_group(2))
```

    Tournament [2 stages]
      groups       in_progress  0/48 matches
      round_of_16  blocked      0/0 matches

All routing selectors — `top_n`, `top_per_group`, `remaining`, `losers`,
`slice_range`, `filter_by`, and their `_per_group` variants — sit in
`take =` and evaluate against the source stage’s standings at transition
time.

## The spec path

Define a blueprint without participants, validate it, then reuse it
across different fields:

``` r
my_spec <- spec() |>
  round_robin("groups") |>
  single_elim("finals",      from = "groups", take = top_n(2)) |>
  single_elim("consolation", from = "groups", take = remaining())

validate(my_spec, n = 16)   # errors loudly if routing is infeasible
```

``` r
trn2 <- build(my_spec, teams)
trn2
```

    Tournament [3 stages]
      groups       in_progress  0/6 matches
      finals       blocked      0/0 matches
      consolation  blocked      0/0 matches

## Entering results

``` r
# One at a time
trn2 <- result(trn2, "groups", match = 1, score = c(2, 1))

# Batch: a data frame with columns match, score1, score2
more <- matches(trn2, "groups")
trn2 <- results(trn2, "groups", data.frame(
  match  = more$match_id,
  score1 = rep(2L, nrow(more)),
  score2 = rep(0L, nrow(more))
))
```

`score = c(home, away)` — always a numeric vector. For best-of series,
pass per-game scores and bracketeer sums them:
`score = c(1, 0, 1, 0, 1)`.

## Inspecting state

``` r
stage_status(trn)
```

            stage   status complete total materialized
    1      groups complete        6     6         TRUE
    2 grand_final complete        1     1         TRUE

``` r
routing_log(trn)
```

      source_stage_id         transition_id rule_applied     selected
    1          groups groups_to_grand_final   top_n(n=2) Bears, Lions
      selected_count pool_before pool_after           timestamp
    1              2           4          2 2026-02-19 13:15:52

## Manual advance (opt-in)

Auto-advance is the default. Pass `auto_advance = FALSE` to control each
stage transition yourself:

``` r
trn_m <- tournament(teams, auto_advance = FALSE) |>
  round_robin("groups") |>
  single_elim("grand_final", take = top_n(2))

m <- matches(trn_m, "groups")
trn_m <- results(trn_m, "groups", data.frame(
  match  = m$match_id,
  score1 = c(2, 1, 3, 1, 2, 1),
  score2 = c(1, 2, 1, 0, 0, 0)
))

# Groups are complete but grand_final hasn't opened yet
stage_status(trn_m)
```

            stage   status complete total materialized
    1      groups complete        6     6         TRUE
    2 grand_final  blocked        0     0        FALSE

``` r
trn_m <- advance(trn_m, "groups")
stage_status(trn_m)
```

            stage      status complete total materialized
    1      groups    complete        6     6         TRUE
    2 grand_final in_progress        0     1         TRUE

## API reference

### Definition verbs

| Function | Purpose |
|----|----|
| `tournament(participants, auto_advance = TRUE)` | Create a live tournament |
| `spec()` | Create a reusable blueprint |
| `round_robin(id, ...)` | Add round-robin stage |
| `single_elim(id, ...)` | Add single-elimination stage |
| `double_elim(id, ...)` | Add double-elimination stage |
| `swiss(id, ...)` | Add Swiss-system stage |
| `two_leg(id, ...)` | Add two-leg knockout stage |
| `group_stage_knockout(id, ...)` | Add combined group+knockout stage |

Each verb accepts `from = previous_stage()` (default in linear chains)
and `take =` (routing selector; default: all participants from source).

### Routing selectors

| Selector                    | Picks                                        |
|-----------------------------|----------------------------------------------|
| `top_n(n)`                  | Top n by overall standings                   |
| `bottom_n(n)`               | Bottom n by overall standings                |
| `slice_range(from, to)`     | Positions from–to                            |
| `top_per_group(n)`          | Top n from every group                       |
| `bottom_per_group(n)`       | Bottom n from every group                    |
| `slice_per_group(from, to)` | Positions from–to within every group         |
| `remaining()`               | Not yet consumed by a prior transition       |
| `losers()`                  | Eliminated participants                      |
| `filter_by(fn)`             | Custom predicate on the standings data frame |

### Runtime verbs

| Function | Purpose |
|----|----|
| `result(trn, stage, match, score)` | Enter one match result |
| `results(trn, stage, df)` | Batch result entry |
| `advance(trn, stage)` | Manually advance a completed stage |
| `teardown(trn, stage)` | Un-materialize a stage and all dependents |

### Inspection nouns

| Function                        | Purpose                                |
|---------------------------------|----------------------------------------|
| `matches(trn, stage?, status?)` | Match table (pending / complete / all) |
| `standings(trn, stage?)`        | Standings table                        |
| `stage_status(trn)`             | Per-stage overview                     |
| `winner(trn)`                   | Tournament winner                      |
| `rankings(trn)`                 | Final placement table                  |
| `routing_log(trn)`              | Transition audit trail                 |

### Spec-only

| Function                    | Purpose                                 |
|-----------------------------|-----------------------------------------|
| `validate(spec, n)`         | Preflight feasibility check             |
| `build(spec, participants)` | Materialize spec into a live tournament |

## Documentation

- `vignette("tournament-lifecycle")` — Full API lifecycle walkthrough
- `vignette("fifa-world-cup")` — Group stage routing with
  `top_per_group()`
- `vignette("swiss-top-cut")` — Swiss to single-elimination linear chain
- `vignette("nhl-stanley-cup")` — Best-of-7 single-elimination playoffs
- `vignette("error-catalog")` — Common errors and how to fix them

## License

MIT
