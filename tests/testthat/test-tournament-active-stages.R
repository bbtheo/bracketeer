complete_materialized_stage <- function(tournament, stage_id, score1 = 1, score2 = 0,
                                        auto_advance = FALSE) {
    repeat {
        matches <- tournament$stage_state[[stage_id]]$bracket$matches
        playable_ids <- vapply(Filter(function(match) {
            match$status != "complete" &&
                !is.na(match$participant1) &&
                !is.na(match$participant2)
        }, matches), `[[`, integer(1), "id")

        if (length(playable_ids) == 0L) {
            break
        }

        for (match_id in playable_ids) {
            tournament <- set_result(
                tournament,
                stage_id = stage_id,
                match_id = match_id,
                score1 = score1,
                score2 = score2,
                auto_advance = auto_advance
            )
        }
    }

    tournament
}

test_that("advance materializes all downstream stages for a completed split source", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("championship", take = slice_range(1, 2), consume = FALSE) |>
        single_elim("consolation", from = "groups", take = slice_range(3, 4), consume = FALSE)

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, stage_id = "groups")

    tournament <- advance(tournament, stage_id = "groups")

    expect_true(tournament$stage_state$championship$materialized)
    expect_true(tournament$stage_state$consolation$materialized)
    expect_setequal(tournament$active_stage_ids, c("consolation", "championship"))
})

test_that("stage_status can be filtered to deterministic ready-stage order", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("sink_b", take = top_n(2)) |>
        single_elim("sink_a", from = "groups", take = remaining()) |>
        single_elim("final_b", from = "sink_b") |>
        single_elim("final_a", from = "sink_a")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))

    ready <- stage_status(tournament)
    ready <- ready$stage[ready$status == "complete" & ready$materialized]
    ready <- ready[!vapply(ready, function(stage_id) {
        isTRUE(tournament$stage_state[[stage_id]]$advanced)
    }, logical(1))]
    expect_equal(ready, character(0))

    tournament <- complete_materialized_stage(tournament, stage_id = "groups")
    tournament <- advance(tournament, stage_id = "groups")
    tournament <- complete_materialized_stage(tournament, stage_id = "sink_a")
    tournament <- complete_materialized_stage(tournament, stage_id = "sink_b")

    ready <- stage_status(tournament)
    ready <- ready$stage[ready$status == "complete" & ready$materialized]
    ready <- ready[!vapply(ready, function(stage_id) {
        isTRUE(tournament$stage_state[[stage_id]]$advanced)
    }, logical(1))]
    expect_equal(ready, c("sink_b", "sink_a"))
})
