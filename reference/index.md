# Package index

## All functions

- [`add_stage()`](https://bbtheo.github.io/bracketeer/reference/add_stage.md)
  : Add a stage to a tournament specification

- [`add_transition()`](https://bbtheo.github.io/bracketeer/reference/add_transition.md)
  : Add a transition between stages

- [`advance()`](https://bbtheo.github.io/bracketeer/reference/advance.md)
  : Advance tournament to next round

- [`bottom_n()`](https://bbtheo.github.io/bracketeer/reference/bottom_n.md)
  : Select bottom ranked participants from source standings

- [`bottom_per_group()`](https://bbtheo.github.io/bracketeer/reference/bottom_per_group.md)
  : Select bottom ranked participants per group

- [`build()`](https://bbtheo.github.io/bracketeer/reference/build.md) :
  Build a live tournament runtime from a specification

- [`build_tournament()`](https://bbtheo.github.io/bracketeer/reference/build_tournament.md)
  : Build a tournament runtime from a tournament specification

- [`compute_tournament_rankings()`](https://bbtheo.github.io/bracketeer/reference/compute_tournament_rankings.md)
  : Compute tournament rankings

- [`double_elim()`](https://bbtheo.github.io/bracketeer/reference/double_elim.md)
  : Create a double elimination bracket

- [`export_matches()`](https://bbtheo.github.io/bracketeer/reference/export_matches.md)
  : Export tournament matches across materialized stages

- [`export_standings()`](https://bbtheo.github.io/bracketeer/reference/export_standings.md)
  : Export tournament standings across materialized stages

- [`export_tournament_log()`](https://bbtheo.github.io/bracketeer/reference/export_tournament_log.md)
  : Export tournament routing log entries

- [`filter_by()`](https://bbtheo.github.io/bracketeer/reference/filter_by.md)
  : Select participants using a custom predicate function

- [`from_previous()`](https://bbtheo.github.io/bracketeer/reference/from_previous.md)
  : Resolve source stage from most recently defined stage order

- [`get_ready_stages()`](https://bbtheo.github.io/bracketeer/reference/get_ready_stages.md)
  : Get stage IDs currently ready to advance

- [`get_routing_log()`](https://bbtheo.github.io/bracketeer/reference/get_routing_log.md)
  : Get transition routing log entries

- [`group_stage_knockout()`](https://bbtheo.github.io/bracketeer/reference/group_stage_knockout.md)
  : Create a group stage followed by knockout bracket

- [`is_stage_complete()`](https://bbtheo.github.io/bracketeer/reference/is_stage_complete.md)
  : Check whether a stage is complete

- [`losers()`](https://bbtheo.github.io/bracketeer/reference/losers.md)
  : Select losers from a source stage by elimination round

- [`matches()`](https://bbtheo.github.io/bracketeer/reference/matches.md)
  : Inspect tournament matches

- [`new_selector()`](https://bbtheo.github.io/bracketeer/reference/new_selector.md)
  :

  Construct a selector object for transition `take =` routing

- [`previous_stage()`](https://bbtheo.github.io/bracketeer/reference/previous_stage.md)
  : Resolve source stage from the immediately preceding stage

- [`print(`*`<group_stage_knockout>`*`)`](https://bbtheo.github.io/bracketeer/reference/print.md)
  [`print(`*`<bracket_match>`*`)`](https://bbtheo.github.io/bracketeer/reference/print.md)
  [`print(`*`<bracket>`*`)`](https://bbtheo.github.io/bracketeer/reference/print.md)
  [`print(`*`<double_elim_bracket>`*`)`](https://bbtheo.github.io/bracketeer/reference/print.md)
  [`print(`*`<tournament>`*`)`](https://bbtheo.github.io/bracketeer/reference/print.md)
  : Print bracketeer objects

- [`qualify_losers()`](https://bbtheo.github.io/bracketeer/reference/qualify_losers.md)
  : Select losers from a source stage by elimination round

- [`qualify_remaining()`](https://bbtheo.github.io/bracketeer/reference/qualify_remaining.md)
  : Select all entrants remaining in the transition source pool

- [`rankings()`](https://bbtheo.github.io/bracketeer/reference/rankings.md)
  : Get tournament rankings

- [`remaining()`](https://bbtheo.github.io/bracketeer/reference/remaining.md)
  : Select entrants remaining in the current transition source pool

- [`result()`](https://bbtheo.github.io/bracketeer/reference/result.md)
  : Fluent tournament result entry helper

- [`results()`](https://bbtheo.github.io/bracketeer/reference/results.md)
  : Fluent tournament batch result entry helper

- [`round_robin()`](https://bbtheo.github.io/bracketeer/reference/round_robin.md)
  : Create a round robin tournament

- [`routing_log()`](https://bbtheo.github.io/bracketeer/reference/routing_log.md)
  : Get transition routing log

- [`set_outcome()`](https://bbtheo.github.io/bracketeer/reference/set_outcome.md)
  : Configure tournament outcome depth

- [`single_elim()`](https://bbtheo.github.io/bracketeer/reference/single_elim.md)
  : Create a single elimination bracket

- [`single_elim_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  [`double_elim_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  [`round_robin_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  [`swiss_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  [`group_stage_knockout_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  [`two_leg_stage()`](https://bbtheo.github.io/bracketeer/reference/single_elim_stage.md)
  : Create a stage specification

- [`slice_per_group()`](https://bbtheo.github.io/bracketeer/reference/slice_per_group.md)
  : Select an inclusive standings slice per group

- [`slice_range()`](https://bbtheo.github.io/bracketeer/reference/slice_range.md)
  : Select an inclusive standings slice

- [`spec()`](https://bbtheo.github.io/bracketeer/reference/spec.md) :
  Create a bracketeer tournament specification

- [`split_stage()`](https://bbtheo.github.io/bracketeer/reference/split_stage.md)
  : Add multiple transitions from one source stage

- [`stage_status()`](https://bbtheo.github.io/bracketeer/reference/stage_status.md)
  : Inspect tournament stage status

- [`standings()`](https://bbtheo.github.io/bracketeer/reference/standings.md)
  : Inspect tournament standings

- [`summary(`*`<bracket>`*`)`](https://bbtheo.github.io/bracketeer/reference/summary.md)
  : Summarize bracketeer objects

- [`swiss()`](https://bbtheo.github.io/bracketeer/reference/swiss.md) :
  Create a Swiss-system tournament

- [`teardown()`](https://bbtheo.github.io/bracketeer/reference/teardown.md)
  : Teardown tournament state

- [`top_n()`](https://bbtheo.github.io/bracketeer/reference/top_n.md) :
  Select top ranked participants from source standings

- [`top_per_group()`](https://bbtheo.github.io/bracketeer/reference/top_per_group.md)
  : Select top ranked participants per group

- [`tournament()`](https://bbtheo.github.io/bracketeer/reference/tournament.md)
  : Create an empty live tournament pipeline

- [`tournament_spec()`](https://bbtheo.github.io/bracketeer/reference/tournament_spec.md)
  : Create a tournament specification graph

- [`two_leg()`](https://bbtheo.github.io/bracketeer/reference/two_leg.md)
  : Create a two-leg stage or bracket

- [`validate()`](https://bbtheo.github.io/bracketeer/reference/validate.md)
  : Validate a tournament spec preflight

- [`validate_tournament()`](https://bbtheo.github.io/bracketeer/reference/validate_tournament.md)
  : Dry-run preflight validation for tournament flow feasibility

- [`validate_tournament_spec()`](https://bbtheo.github.io/bracketeer/reference/validate_tournament_spec.md)
  : Validate a tournament specification

- [`winner()`](https://bbtheo.github.io/bracketeer/reference/winner.md)
  : Get tournament winner
