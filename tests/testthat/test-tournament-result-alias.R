test_that("result helper applies tournament match results with score vector", {
    tournament <- spec() |>
        single_elim("finals") |>
        build(participants = paste("Team", LETTERS[1:4]))

    tournament <- result(
        tournament,
        stage = "finals",
        match = 1,
        score = c(1, 0)
    )

    match <- tournament$stage_state$finals$bracket$matches[[1]]
    expect_equal(match$status, "complete")
    expect_equal(match$winner, match$participant1)
})

test_that("result helper supports positional score vector entry", {
    tournament <- spec() |>
        single_elim("finals") |>
        build(participants = paste("Team", LETTERS[1:4]))

    tournament <- result(tournament, "finals", 2, c(0, 1))

    match <- tournament$stage_state$finals$bracket$matches[[2]]
    expect_equal(match$status, "complete")
    expect_equal(match$winner, match$participant2)
})

test_that("result helper rejects legacy score1/score2 single-entry args", {
    tournament <- spec() |>
        single_elim("finals") |>
        build(participants = paste("Team", LETTERS[1:4]))

    expect_error(
        result(tournament, stage = "finals", match = 1, score1 = 1, score2 = 0),
        "unused arguments"
    )
})

test_that("result helper uses tournament auto_advance default", {
    tournament <- tournament(paste("Team", LETTERS[1:4]), auto_advance = TRUE) |>
        round_robin("groups") |>
        single_elim("playoffs")
    matches <- get_matches(tournament$stage_state$groups$bracket, status = "all")

    for (match_id in matches$id) {
        tournament <- result(
            tournament,
            stage = "groups",
            match = match_id,
            score = c(1, 0)
        )
    }

    expect_true(tournament$stage_state$groups$advanced)
    expect_true(tournament$stage_state$playoffs$materialized)
})

test_that("result helper supports auto_advance override", {
    tournament <- tournament(paste("Team", LETTERS[1:4]), auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("playoffs")
    matches <- get_matches(tournament$stage_state$groups$bracket, status = "all")

    for (match_id in matches$id) {
        tournament <- result(
            tournament,
            stage = "groups",
            match = match_id,
            score = c(1, 0)
        )
    }

    expect_false(tournament$stage_state$groups$advanced)
    expect_false(tournament$stage_state$playoffs$materialized)
})

test_that("results helper enters batch results from match/score1/score2 columns", {
    tournament <- spec() |>
        single_elim("finals") |>
        build(participants = paste("Team", LETTERS[1:4]))

    batch <- data.frame(
        match = c(1, 2),
        score1 = c(1, 0),
        score2 = c(0, 1)
    )

    tournament <- results(tournament, stage = "finals", df = batch, auto_advance = FALSE)

    expect_equal(tournament$stage_state$finals$bracket$matches[[1]]$status, "complete")
    expect_equal(tournament$stage_state$finals$bracket$matches[[2]]$status, "complete")
})

test_that("results helper triggers auto-advance when final rows complete a stage", {
    tournament <- tournament(paste("Team", LETTERS[1:4]), auto_advance = TRUE) |>
        round_robin("groups") |>
        single_elim("playoffs")

    matches <- get_matches(tournament$stage_state$groups$bracket, status = "all")
    batch <- data.frame(
        match = matches$id,
        score1 = rep(1, nrow(matches)),
        score2 = rep(0, nrow(matches))
    )

    tournament <- results(tournament, stage = "groups", df = batch)

    expect_true(tournament$stage_state$groups$advanced)
    expect_true(tournament$stage_state$playoffs$materialized)
})

test_that("result helper accepts per-game score vectors for best-of stages", {
    tournament <- spec() |>
        single_elim("finals", best_of = 5) |>
        build(participants = paste("Team", LETTERS[1:4]))

    tournament <- result(
        tournament,
        stage = "finals",
        match = 1,
        score = c(1, 0, 2, 1, 1, 0),
        auto_advance = FALSE
    )

    match <- tournament$stage_state$finals$bracket$matches[[1]]
    expect_equal(match$status, "complete")
    expect_equal(match$winner, match$participant1)
})

test_that("result helper accepts odd-length per-game score vectors", {
    tournament <- spec() |>
        single_elim("finals", best_of = 5) |>
        build(participants = paste("Team", LETTERS[1:4]))

    expect_no_error({
        tournament <- result(
            tournament,
            stage = "finals",
            match = 1,
            score = c(3, 1, 2, 0, 3),
            auto_advance = FALSE
        )
    })
    expect_equal(tournament$stage_state$finals$bracket$matches[[1]]$status, "complete")
})

test_that("results helper validates batch data-frame payload", {
    tournament <- spec() |>
        single_elim("finals") |>
        build(participants = paste("Team", LETTERS[1:4]))

    expect_error(
        results(tournament, "finals", list(match = 1, score1 = 1, score2 = 0)),
        "`df` must be a data.frame"
    )
    expect_error(
        results(tournament, "finals", data.frame(match = 1, score1 = 1)),
        "`df` must include columns"
    )
    expect_error(
        results(
            tournament,
            "finals",
            data.frame(match = NA, score1 = 1, score2 = 0)
        ),
        "`df\\$match` must not contain NA"
    )
    expect_error(
        results(
            tournament,
            "finals",
            data.frame(match = 1, score1 = "x", score2 = 0)
        ),
        "`df\\$score1` and `df\\$score2` must be numeric"
    )
    expect_error(
        results(
            tournament,
            "finals",
            data.frame(match = 1, score1 = 1, score2 = NA_real_)
        ),
        "`df\\$score1` and `df\\$score2` must be finite and non-missing"
    )
})
