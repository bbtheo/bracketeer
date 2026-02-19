# Tournament Lifecycle and Live Operations

``` r
library(bracketeer)
```

This guide is a function-discovery map for tournament operations. Every
public function is shown in context so you know what it does, when to
call it, and what you get back.

------------------------------------------------------------------------

## Mental model

A tournament definition reads like a rulebook. Stage-type functions
(`round_robin`, `single_elim`, `swiss`, …) are the **verbs** — pipe them
onto
[`tournament()`](https://bbtheo.github.io/bracketeer/reference/tournament.md)
or [`spec()`](https://bbtheo.github.io/bracketeer/reference/spec.md) to
build the graph.

A tournament runtime feels like a scoreboard. Noun functions (`matches`,
`standings`, `stage_status`, `winner`, …) expose live state without
mutating it.

------------------------------------------------------------------------

## 1) Define

### `tournament()` and `spec()`

`tournament(participants)` is the entry point for a live runtime. It
returns a `tournament` object that you pipe stage verbs onto.

[`spec()`](https://bbtheo.github.io/bracketeer/reference/spec.md) is the
entry point for a reusable blueprint without participants. It returns a
`bracketeer_spec` object; use `build(spec, participants)` to materialize
it later.

``` r
teams <- paste("Team", LETTERS[1:16])

# ── Live tournament ───────────────────────────────────────────────────────────
trn <- tournament(teams) |>
  round_robin("groups")

# ── Blueprint / reusable spec ─────────────────────────────────────────────────
my_spec <- spec() |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(8))
```

[`tournament()`](https://bbtheo.github.io/bracketeer/reference/tournament.md)
and [`spec()`](https://bbtheo.github.io/bracketeer/reference/spec.md)
accept the same stage verbs. The difference is that
[`tournament()`](https://bbtheo.github.io/bracketeer/reference/tournament.md)
materializes the first stage immediately while
[`spec()`](https://bbtheo.github.io/bracketeer/reference/spec.md) defers
everything to
[`build()`](https://bbtheo.github.io/bracketeer/reference/build.md).

### Stage verbs

Each stage verb adds one node to the tournament graph. The verb name is
the format; the first argument is a unique stage ID.

| Verb                            | Format                     |
|---------------------------------|----------------------------|
| `round_robin(id, ...)`          | All-play-all               |
| `single_elim(id, ...)`          | Single-elimination bracket |
| `double_elim(id, ...)`          | Double-elimination bracket |
| `swiss(id, ...)`                | Swiss-system               |
| `two_leg(id, ...)`              | Two-leg knockout           |
| `group_stage_knockout(id, ...)` | Combined group + knockout  |

Every verb accepts two wiring parameters:

- `from` — source stage ID (default
  [`previous_stage()`](https://bbtheo.github.io/bracketeer/reference/previous_stage.md)).
- `take` — routing selector (default: all participants from source).

### `from =` and `previous_stage()`

In a linear chain, `from` defaults to the immediately preceding stage.
You never write it explicitly.

``` r
# These two are identical:
tournament(teams) |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(8))

tournament(teams) |>
  round_robin("groups") |>
  single_elim("finals", from = previous_stage(), take = top_n(8))
```

For branching — two stages reading from the same source — you must name
`from` explicitly:

``` r
trn <- tournament(teams) |>
  round_robin("groups") |>
  single_elim("championship", from = "groups", take = top_n(8)) |>
  single_elim("consolation",  from = "groups", take = remaining())
```

### Routing helpers (`take =`)

Pass a selector to `take =` to control who advances from the source
stage.

``` r
top_n(8)               # top 8 by overall standings
bottom_n(4)            # bottom 4
slice_range(5, 8)      # positions 5 through 8
remaining()            # everyone not already consumed by a prior transition
losers()               # eliminated participants
filter_by(function(df, ...) df[df$points >= 6, ])  # custom predicate

# ── Per-group variants (require grouped source stage) ─────────────────────────
top_per_group(2)       # top 2 from each group
bottom_per_group(1)    # bottom 1 from each group
slice_per_group(3, 3)  # 3rd place from each group
```

------------------------------------------------------------------------

## 2) Validate (spec path)

`validate(spec, n)` runs preflight feasibility checks before
materializing anything.

``` r
my_spec <- spec() |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(8))

validate(my_spec, n = 16)   # passes
validate(my_spec, n = 6)    # fails — can't select top 8 from 6 teams
```

Errors are identifier-rich: they include stage IDs and participant
counts.
[`build()`](https://bbtheo.github.io/bracketeer/reference/build.md)
implicitly calls
[`validate()`](https://bbtheo.github.io/bracketeer/reference/validate.md)
before materializing.

------------------------------------------------------------------------

## 3) Build (spec path)

``` r
my_spec <- spec() |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(8))

trn <- my_spec |> build(teams)
```

[`build()`](https://bbtheo.github.io/bracketeer/reference/build.md)
materializes the first stage immediately and leaves downstream stages in
`"blocked"` state until transitions resolve.

------------------------------------------------------------------------

## 4) Enter results

### `result()` — single match

``` r
trn <- trn |> result("groups", match = 1, score = c(2, 1))
```

- `stage` — stage ID.
- `match` — match ID from
  [`matches()`](https://bbtheo.github.io/bracketeer/reference/matches.md).
- `score` — length-2+ numeric vector. For best-of series, a per-game
  score vector (e.g. `c(3, 1, 2, 0, 3)` for a 5-game series).

### `results()` — batch from data frame

``` r
results_df <- data.frame(
  match  = 1:4,
  score1 = c(2, 1, 3, 0),
  score2 = c(1, 0, 2, 1)
)

trn <- trn |> results("groups", results_df)
```

Columns must be `match`, `score1`, `score2`. Batch entry triggers
auto-advance when the final result completes the stage.

### Auto-advance

By default, entering the last match in a stage automatically
materializes all downstream stages — no explicit
[`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
call needed.

``` r
# All group matches entered → finals stage materializes automatically.
group_ms <- matches(trn, "groups")

for (i in seq_len(nrow(group_ms))) {
  trn <- trn |> result("groups", match = group_ms$id[i], score = c(1, 0))
}

stage_status(trn)  # finals is now "active"
```

### Manual advance (opt-in)

``` r
trn <- tournament(teams, auto_advance = FALSE) |>
  round_robin("groups") |>
  single_elim("finals", take = top_n(8))

# ... enter results ...

trn <- trn |> advance("groups")  # explicit trigger
```

### Overwrite policy

- Overwriting a result **before** downstream stages materialize is
  allowed; standings recalculate automatically.
- Overwriting a result **after** downstream materialization is blocked
  with an error that identifies the blocking downstream stage ID.
- Use
  [`teardown()`](https://bbtheo.github.io/bracketeer/reference/teardown.md)
  to un-materialize downstream stages and unlock the upstream result for
  editing.

------------------------------------------------------------------------

## 5) Inspect state

All inspection functions return plain data frames or scalar values.

``` r
# ── Print ───────────────────────────────────────────────────────────────────
trn
# Tournament [2 stages]
#   groups   in_progress   90/120 matches
#   finals   blocked

# ── Stage overview ──────────────────────────────────────────────────────────
stage_status(trn)
#   stage    status        complete  total  materialized
#   groups   in_progress         90    120          TRUE
#   finals   blocked              0      0         FALSE

# ── Matches ─────────────────────────────────────────────────────────────────
matches(trn)                           # all stages, pending (default)
matches(trn, "groups")                 # one stage, pending
matches(trn, "groups", status = "all") # one stage, all

# ── Standings ───────────────────────────────────────────────────────────────
standings(trn, "groups")   # one stage
standings(trn)              # all stages

# ── Outcomes ────────────────────────────────────────────────────────────────
winner(trn)       # tournament winner (NA until complete)
rankings(trn)     # final placement table
routing_log(trn)  # transition audit trail
```

------------------------------------------------------------------------

## 6) Teardown

`teardown(trn, stage)` un-materializes a stage and all downstream
dependents, cascading along the DAG. Source-stage results and standings
are preserved.

``` r
trn <- tournament(teams) |>
  swiss("open", rounds = 5) |>
  single_elim("top_cut", take = top_n(8))

# ... enter all open results, top_cut materializes automatically ...

# Decide to revise an open result:
trn <- teardown(trn, "top_cut")     # un-materializes top_cut
trn <- result(trn, "open", match = 3, score = c(0, 2))  # overwrite now allowed
# top_cut will re-materialize when open is completed again.
```

After teardown, torn-down stages revert to `"blocked"` in
[`stage_status()`](https://bbtheo.github.io/bracketeer/reference/stage_status.md).

------------------------------------------------------------------------

## 7) Full lifecycle example

``` r
library(bracketeer)

teams <- paste("Team", LETTERS[1:16])

# ── Define ────────────────────────────────────────────────────────────────────
trn <- tournament(teams) |>
  swiss("open", rounds = 5) |>
  single_elim("top_cut", take = top_n(8))

# ── Enter results ─────────────────────────────────────────────────────────────
open_ms <- matches(trn, "open")

for (i in seq_len(nrow(open_ms))) {
  trn <- trn |> result("open", match = open_ms$id[i], score = c(1, 0))
}

# top_cut auto-materialized after final open result.

# ── Inspect ───────────────────────────────────────────────────────────────────
stage_status(trn)
matches(trn, "top_cut")
standings(trn, "open")

# ── Run top_cut ───────────────────────────────────────────────────────────────
cut_ms <- matches(trn, "top_cut")

for (i in seq_len(nrow(cut_ms))) {
  trn <- trn |> result("top_cut", match = cut_ms$id[i], score = c(1, 0))
}

# ── Outcomes ──────────────────────────────────────────────────────────────────
winner(trn)
rankings(trn)
routing_log(trn)
```
