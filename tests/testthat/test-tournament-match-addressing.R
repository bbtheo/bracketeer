build_two_sink_runtime <- function() {
    tournament <- tournament(paste("Team", LETTERS[1:4]), auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("A", take = top_n(2)) |>
        single_elim("B", from = "groups", take = remaining())

    group_matches <- matches(tournament, "groups", status = "all")
    for (match_id in group_matches$id) {
        tournament <- result(
            tournament,
            stage = "groups",
            match = match_id,
            score = c(1, 0),
            auto_advance = FALSE
        )
    }

    advance(tournament, stage_id = "groups")
}

test_that("result supports stage-scoped addressing with stage + match", {
    tournament <- build_two_sink_runtime()
    tournament <- result(tournament, stage = "A", match = 1, score = c(1, 0))

    expect_equal(tournament$stage_state$A$bracket$matches[[1]]$status, "complete")
    expect_equal(tournament$stage_state$B$bracket$matches[[1]]$status, "pending")
})

test_that("set_result supports compound match IDs using stage_id::match_id", {
    tournament <- build_two_sink_runtime()
    tournament <- set_result(tournament, match_id = "B::1", score1 = 0, score2 = 1)

    expect_equal(tournament$stage_state$B$bracket$matches[[1]]$status, "complete")
    expect_equal(tournament$stage_state$A$bracket$matches[[1]]$status, "pending")
})

test_that("set_result detects ambiguity when stage_id is omitted", {
    tournament <- build_two_sink_runtime()

    expect_error(
        set_result(tournament, match_id = 1, score1 = 1, score2 = 0),
        "Ambiguous match_id"
    )
})
