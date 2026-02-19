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

test_that("overlap conflicts error by default when allow_overlap is FALSE", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "primary",
            take = top_n(2),
            consume = FALSE,
            allow_overlap = FALSE,
            transition_id = "to_primary"
        ) |>
        single_elim(
            "secondary",
            from = "groups",
            take = top_n(2),
            consume = FALSE,
            allow_overlap = FALSE,
            transition_id = "to_secondary"
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")

    expect_error(
        advance(tournament, stage_id = "groups"),
        "overlap conflict"
    )
})

test_that("overlap is allowed only when transitions explicitly opt in", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "primary",
            take = top_n(2),
            consume = FALSE,
            allow_overlap = TRUE
        ) |>
        single_elim(
            "secondary",
            from = "groups",
            take = top_n(2),
            consume = FALSE,
            allow_overlap = TRUE
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    expected <- as.character(head(
        get_standings(tournament$stage_state$groups$bracket)$participant,
        2
    ))
    tournament <- advance(tournament, stage_id = "groups")

    expect_true(tournament$stage_state$primary$materialized)
    expect_true(tournament$stage_state$secondary$materialized)
    expect_equal(tournament$stage_state$primary$participants, expected)
    expect_equal(tournament$stage_state$secondary$participants, expected)
})
