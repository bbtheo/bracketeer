
test_that("explicit branching creates deterministic transition IDs", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("championship", take = top_n(2)) |>
        single_elim("consolation", from = "groups", take = remaining())

    expect_equal(
        names(spec$edges),
        c("groups_to_championship", "groups_to_consolation")
    )
})

test_that("split_stage fan-out materializes both downstream stages", {
    complete_materialized_stage <- function(tournament, stage_id, score1 = 1, score2 = 0) {
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
                    score2 = score2
                )
            }
        }

        tournament
    }

    spec <- spec() |>
        round_robin("groups") |>
        single_elim("championship", take = top_n(2)) |>
        single_elim("consolation", from = "groups", take = remaining())

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_true(tournament$stage_state$championship$materialized)
    expect_true(tournament$stage_state$consolation$materialized)
})
