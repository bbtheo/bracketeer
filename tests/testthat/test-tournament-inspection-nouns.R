complete_stage_results <- function(tournament, stage_id, score1 = 1, score2 = 0,
                                   auto_advance = FALSE) {
    match_ids <- vapply(
        tournament$stage_state[[stage_id]]$bracket$matches,
        `[[`,
        integer(1),
        "id"
    )

    for (match_id in match_ids) {
        tournament <- set_result(
            tournament,
            stage_id = stage_id,
            match_id = match_id,
            score1 = score1,
            score2 = score2,
            auto_advance = auto_advance
        )
    }

    tournament
}

test_that("inspection nouns expose stage and tournament views", {
    trn <- tournament(paste("Team", LETTERS[1:4]), auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(2))

    expect_s3_class(stage_status(trn), "data.frame")
    expect_true(all(c("stage", "status", "complete", "total", "materialized") %in% names(stage_status(trn))))

    expect_s3_class(matches(trn), "data.frame")
    expect_true(all(c("stage_id", "compound_match_id") %in% names(matches(trn))))

    expect_s3_class(matches(trn, "groups", status = "pending"), "data.frame")
    expect_s3_class(standings(trn, "groups"), "data.frame")
    expect_s3_class(standings(trn), "data.frame")

    trn <- complete_stage_results(trn, "groups", auto_advance = TRUE)
    trn <- complete_stage_results(trn, "playoffs", auto_advance = TRUE)

    expect_true(nzchar(winner(trn)))
    expect_s3_class(rankings(trn), "data.frame")
    expect_s3_class(routing_log(trn), "data.frame")
})

test_that("inspection helpers validate arguments", {
    trn <- tournament(paste("Team", LETTERS[1:4])) |>
        round_robin("groups")

    expect_error(matches(trn, status = "bad"), "should be one of")
    expect_error(matches(trn, stage = "missing"), "Unknown stage_id")
    expect_error(standings(trn, stage = "missing"), "Unknown stage_id")
    expect_error(stage_status(list()), "requires a tournament object")
    expect_error(winner(list()), "requires a tournament object")
    expect_error(rankings(list()), "requires a tournament object")
    expect_error(routing_log(list()), "requires a tournament object")
})

test_that("print.tournament shows concise stage summary", {
    trn <- tournament(paste("Team", LETTERS[1:4])) |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(2))

    expect_snapshot_output(print(trn))
})
