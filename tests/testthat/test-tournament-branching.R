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

test_that("advance fan-out materializes all eligible downstream stages in one call", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("championship", take = top_n(2), consume = TRUE, priority = 1L) |>
        single_elim("consolation", from = "groups", take = remaining(), consume = TRUE, priority = 2L)

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_true(tournament$stage_state$championship$materialized)
    expect_true(tournament$stage_state$consolation$materialized)
    expect_equal(sort(tournament$active_stage_ids), c("championship", "consolation"))
})

test_that("transition resolution order is deterministic by declaration order", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "priority_sink",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 1L
        ) |>
        single_elim(
            "b_sink",
            from = "groups",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 2L
        ) |>
        single_elim(
            "a_sink",
            from = "groups",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 2L
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:6]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_equal(
        tournament$stage_state$priority_sink$participants,
        c("Team A", "Team B")
    )
    expect_equal(
        tournament$stage_state$b_sink$participants,
        c("Team C", "Team D")
    )
    expect_equal(
        tournament$stage_state$a_sink$participants,
        c("Team E", "Team F")
    )
})

test_that("consume FALSE keeps pool available while consume TRUE removes selected entrants", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "non_consuming",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            allow_overlap = TRUE,
            consume = FALSE,
            priority = 1L
        ) |>
        single_elim(
            "first_consuming",
            from = "groups",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            allow_overlap = TRUE,
            consume = TRUE,
            priority = 2L
        ) |>
        single_elim(
            "second_consuming",
            from = "groups",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 3L
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_equal(tournament$stage_state$non_consuming$participants, c("Team A", "Team B"))
    expect_equal(tournament$stage_state$first_consuming$participants, c("Team A", "Team B"))
    expect_equal(tournament$stage_state$second_consuming$participants, c("Team C", "Team D"))
})

test_that("qualify_remaining selects entrants left in the source pool", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "winners",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 1L
        ) |>
        single_elim("remaining", from = "groups", take = remaining(), consume = TRUE, priority = 2L)

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    source_pool <- as.character(tournament$stage_state$groups$participants)
    expected_winners <- head(source_pool, 2)
    expected_remaining <- source_pool[!source_pool %in% expected_winners]
    tournament <- advance(tournament, stage_id = "groups")

    expect_equal(tournament$stage_state$winners$participants, expected_winners)
    expect_equal(tournament$stage_state$remaining$participants, expected_remaining)
})
