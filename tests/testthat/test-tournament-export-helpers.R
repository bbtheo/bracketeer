complete_stage_results_for_export <- function(tournament, stage_id, score1 = 1, score2 = 0) {
    repeat {
        pending <- matches(tournament, stage = stage_id, status = "pending")
        ready <- pending[
            !is.na(pending$participant1) & !is.na(pending$participant2),
            ,
            drop = FALSE
        ]

        if (nrow(ready) == 0L) {
            break
        }

        for (match_id in ready$id) {
            tournament <- result(
                tournament,
                stage = stage_id,
                match = match_id,
                score = c(score1, score2)
            )
        }
    }

    tournament
}

build_export_fixture <- function() {
    tournament <- tournament(paste("Team", LETTERS[1:4])) |>
        round_robin("groups") |>
        single_elim(
            "playoffs",
            from = "groups",
            transition_id = "groups_to_playoffs",
            take = top_n(4)
        )

    tournament <- complete_stage_results_for_export(tournament, "groups")
    tournament
}

test_that("routing_log returns flattened routing log rows", {
    empty_tournament <- tournament(paste("Team", LETTERS[1:4])) |>
        round_robin("groups") |>
        single_elim(
            "playoffs",
            from = "groups",
            transition_id = "groups_to_playoffs",
            take = top_n(4)
        )

    empty_export <- routing_log(empty_tournament)
    expect_s3_class(empty_export, "data.frame")
    expect_equal(nrow(empty_export), 0L)
    expect_true(all(c(
        "source_stage_id", "transition_id", "rule_applied", "selected",
        "selected_count", "pool_before", "pool_after", "timestamp"
    ) %in% names(empty_export)))

    tournament <- build_export_fixture()
    exported <- routing_log(tournament)

    expect_equal(nrow(exported), 1L)
    expect_equal(exported$source_stage_id[[1]], "groups")
    expect_equal(exported$transition_id[[1]], "groups_to_playoffs")
    expect_equal(exported$selected_count[[1]], 4L)
    expect_equal(exported$pool_before[[1]], 4L)
    expect_equal(exported$pool_after[[1]], 0L)
    expect_type(exported$selected, "character")
    expect_s3_class(exported$timestamp, "POSIXct")
})

test_that("matches returns stage-tagged match rows with compound IDs", {
    tournament <- build_export_fixture()
    exported <- matches(tournament, status = "all")

    expect_s3_class(exported, "data.frame")
    expect_true(nrow(exported) > 0L)
    expect_true(all(c("stage_id", "match_id", "compound_match_id", "status") %in% names(exported)))
    expect_true(all(grepl("::", exported$compound_match_id, fixed = TRUE)))
    expect_setequal(unique(exported$stage_id), c("groups", "playoffs"))

    expected_count <- nrow(matches(tournament, "groups", status = "all")) +
        nrow(matches(tournament, "playoffs", status = "all"))
    expect_equal(nrow(exported), expected_count)
})

test_that("standings returns stage-tagged standings rows", {
    tournament <- build_export_fixture()
    exported <- standings(tournament)

    expect_s3_class(exported, "data.frame")
    expect_true(nrow(exported) > 0L)
    expect_true(all(c("stage_id", "rank", "participant") %in% names(exported)))
    expect_setequal(unique(exported$stage_id), c("groups", "playoffs"))

    expected_count <- nrow(standings(tournament, "groups")) +
        nrow(standings(tournament, "playoffs"))
    expect_equal(nrow(exported), expected_count)
})

test_that("inspection nouns require tournament objects", {
    expect_error(routing_log(NULL), "tournament object")
    expect_error(matches(NULL))
    expect_error(standings(NULL))
})
