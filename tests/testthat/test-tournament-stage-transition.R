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

test_that("build_tournament materializes source stages and blocks downstream stages", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))

    expect_s3_class(tournament, "tournament")
    expect_equal(tournament$active_stage_ids, "groups")
    expect_true(tournament$stage_state$groups$materialized)
    expect_equal(tournament$stage_state$groups$status, "in_progress")
    expect_false(tournament$stage_state$playoffs$materialized)
    expect_equal(tournament$stage_state$playoffs$status, "blocked")
})

test_that("completing a source stage then advance materializes downstream stage", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_stage_results(tournament, stage_id = "groups")

    expect_equal(tournament$stage_state$groups$status, "complete")
    expect_false(tournament$stage_state$playoffs$materialized)

    tournament <- advance(tournament, stage_id = "groups")

    expect_true(tournament$stage_state$playoffs$materialized)
    expect_equal(tournament$stage_state$playoffs$status, "in_progress")
    expect_equal(tournament$active_stage_ids, "playoffs")
})

test_that("auto_advance materializes downstream stage when source completes", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_stage_results(
        tournament,
        stage_id = "groups",
        auto_advance = TRUE
    )

    expect_equal(tournament$stage_state$groups$status, "complete")
    expect_true(tournament$stage_state$groups$advanced)
    expect_true(tournament$stage_state$playoffs$materialized)
    expect_equal(tournament$stage_state$playoffs$status, "in_progress")
    expect_equal(tournament$active_stage_ids, "playoffs")
})

test_that("set_result defaults auto_advance to TRUE for tournament flow", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    matches <- get_matches(tournament$stage_state$groups$bracket, status = "all")
    for (match_id in matches$id) {
        tournament <- set_result(
            tournament,
            stage_id = "groups",
            match_id = match_id,
            score1 = 1,
            score2 = 0
        )
    }

    expect_true(tournament$stage_state$groups$advanced)
    expect_true(tournament$stage_state$playoffs$materialized)
})

test_that("track_placements controls tournament ranking depth", {
    spec <- spec() |>
        single_elim("finals") |>
        set_outcome(track_placements = 2)

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))

    tournament <- set_result(tournament, stage_id = "finals", match_id = 1, score1 = 1, score2 = 0)
    tournament <- set_result(tournament, stage_id = "finals", match_id = 2, score1 = 0, score2 = 1)
    tournament <- set_result(tournament, stage_id = "finals", match_id = 3, score1 = 1, score2 = 0)

    expect_true(tournament$completed)
    expect_s3_class(tournament$rankings, "data.frame")
    expect_equal(nrow(tournament$rankings), 2L)
    expect_equal(as.integer(tournament$rankings$rank), c(1L, 2L))
})

test_that("is_stage_complete works for bracket and tournament stages", {
    bracket <- new_round_robin_bracket(paste("Team", LETTERS[1:4]))
    expect_false(is_stage_complete(bracket))

    for (match_id in seq_along(bracket$matches)) {
        bracket <- set_result(bracket, match_id = match_id, score1 = 1, score2 = 0)
    }
    expect_true(is_stage_complete(bracket))

    spec <- spec() |>
        round_robin("groups")
    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))

    status <- stage_status(tournament)
    expect_equal(status$status[status$stage == "groups"], "in_progress")
    tournament <- complete_stage_results(tournament, stage_id = "groups")
    status <- stage_status(tournament)
    expect_equal(status$status[status$stage == "groups"], "complete")
})

test_that("set_result overwrite is allowed before downstream stages materialize", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))

    tournament <- set_result(tournament, stage_id = "groups", match_id = 1, score1 = 3, score2 = 1)
    tournament <- set_result(
        tournament,
        stage_id = "groups",
        match_id = 1,
        score1 = 0,
        score2 = 2,
        overwrite = TRUE
    )

    match <- tournament$stage_state$groups$bracket$matches[[1]]
    expect_equal(match$score1, 0)
    expect_equal(match$score2, 2)
    expect_equal(match$winner, match$participant2)
})

test_that("set_result overwrite is blocked after dependent stages materialize", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_stage_results(tournament, stage_id = "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_error(
        set_result(
            tournament,
            stage_id = "groups",
            match_id = 1,
            score1 = 0,
            score2 = 2,
            overwrite = TRUE
        ),
        "blocking downstream stage_id `playoffs`"
    )
})

test_that("teardown un-materializes target stage and preserves source stage results", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_stage_results(tournament, stage_id = "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_true(tournament$stage_state$playoffs$materialized)
    groups_snapshot <- get_standings(tournament$stage_state$groups$bracket)

    tournament <- teardown(tournament, stage_id = "playoffs")

    expect_false(tournament$stage_state$playoffs$materialized)
    expect_equal(tournament$stage_state$playoffs$status, "blocked")
    expect_true(tournament$stage_state$groups$materialized)
    expect_equal(get_standings(tournament$stage_state$groups$bracket), groups_snapshot)
})

test_that("teardown enables upstream overwrite after dependent teardown", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_stage_results(tournament, stage_id = "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_error(
        set_result(
            tournament,
            stage_id = "groups",
            match_id = 1,
            score1 = 0,
            score2 = 2,
            overwrite = TRUE
        ),
        "blocking downstream stage_id `playoffs`"
    )

    tournament <- teardown(tournament, stage_id = "playoffs")
    tournament <- set_result(
        tournament,
        stage_id = "groups",
        match_id = 1,
        score1 = 0,
        score2 = 2,
        overwrite = TRUE,
        auto_advance = FALSE
    )

    match <- tournament$stage_state$groups$bracket$matches[[1]]
    expect_equal(match$score1, 0)
    expect_equal(match$score2, 2)
})

test_that("teardown on already blocked stage is a no-op", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    expect_false(tournament$stage_state$playoffs$materialized)

    updated <- teardown(tournament, stage_id = "playoffs")
    expect_equal(updated$stage_state$playoffs$status, "blocked")
    expect_false(updated$stage_state$playoffs$materialized)
    expect_equal(updated$stage_state$groups$status, tournament$stage_state$groups$status)
})

test_that("advance and teardown validate stage identifiers", {
    spec <- spec() |>
        round_robin("groups")
    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))

    expect_error(
        advance(tournament, stage_id = "missing"),
        "Unknown stage_id `missing`"
    )
    expect_error(
        teardown(tournament, stage_id = ""),
        "`stage_id` must be a non-empty string"
    )
    expect_error(
        teardown(tournament, stage_id = "missing"),
        "Unknown stage_id `missing`"
    )
})

test_that("multi-stage linear flow auto-advances through all stages", {
    trn <- tournament(paste("Team", sprintf("%02d", 1:8)), auto_advance = TRUE) |>
        round_robin("groups") |>
        single_elim("top_cut", take = top_n(4)) |>
        single_elim("finals", take = top_n(2))

    group_matches <- matches(trn, "groups", status = "all")
    for (match_id in group_matches$id) {
        trn <- result(trn, "groups", match_id, c(1, 0))
    }

    expect_true(trn$stage_state$top_cut$materialized)

    top_cut_matches <- matches(trn, "top_cut", status = "all")
    for (match_id in top_cut_matches$id) {
        trn <- result(trn, "top_cut", match_id, c(1, 0))
    }

    expect_true(trn$stage_state$finals$materialized)

    finals_matches <- matches(trn, "finals", status = "all")
    for (match_id in finals_matches$id) {
        trn <- result(trn, "finals", match_id, c(1, 0))
    }

    expect_true(isTRUE(trn$completed))
})

test_that("teardown on a shared source clears all downstream branches", {
    trn <- tournament(paste("Team", LETTERS[1:4]), auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("championship", take = top_n(2)) |>
        single_elim("consolation", from = "groups", take = remaining())

    trn <- complete_stage_results(trn, "groups")
    trn <- advance(trn, stage_id = "groups")
    expect_true(trn$stage_state$championship$materialized)
    expect_true(trn$stage_state$consolation$materialized)

    trn <- teardown(trn, stage_id = "groups")

    expect_equal(stage_status(trn)$status[stage_status(trn)$stage == "championship"], "blocked")
    expect_equal(stage_status(trn)$status[stage_status(trn)$stage == "consolation"], "blocked")
    expect_false(trn$stage_state$championship$materialized)
    expect_false(trn$stage_state$consolation$materialized)
})

test_that("after teardown, results can be replayed and downstream re-materialized", {
    trn <- tournament(paste("Team", LETTERS[1:4]), auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("playoffs")

    trn <- complete_stage_results(trn, "groups")
    trn <- advance(trn, stage_id = "groups")
    expect_true(trn$stage_state$playoffs$materialized)

    trn <- teardown(trn, stage_id = "playoffs")
    expect_false(trn$stage_state$playoffs$materialized)

    trn <- advance(trn, stage_id = "groups")
    expect_true(trn$stage_state$playoffs$materialized)
})
