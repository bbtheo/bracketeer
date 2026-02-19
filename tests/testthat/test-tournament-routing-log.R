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

test_that("advance appends routing log entries for resolved transitions", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "winners",
            transition_id = "groups_to_winners",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 1L
        ) |>
        single_elim(
            "remaining",
            from = "groups",
            transition_id = "groups_to_remaining",
            take = remaining(),
            consume = TRUE,
            priority = 2L
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    log_entries <- routing_log(tournament)

    expect_equal(nrow(log_entries), 2L)
    expect_equal(log_entries$transition_id,
        c("groups_to_winners", "groups_to_remaining"))

    required_fields <- c(
        "source_stage_id", "transition_id", "rule_applied", "selected",
        "selected_count", "pool_before", "pool_after", "timestamp"
    )
    expect_true(all(required_fields %in% names(log_entries)))

    expect_equal(log_entries$source_stage_id[[1]], "groups")
    expect_equal(log_entries$selected_count[[1]], 2L)
    expect_equal(log_entries$pool_before[[1]], 4L)
    expect_equal(log_entries$pool_after[[1]], 2L)

    selected_first <- strsplit(log_entries$selected[[1]], ", ", fixed = TRUE)[[1]]
    selected_second <- strsplit(log_entries$selected[[2]], ", ", fixed = TRUE)[[1]]
    expect_equal(length(selected_second), 2L)
    expect_true(length(intersect(selected_first, selected_second)) == 0L)
    expect_setequal(c(selected_first, selected_second), c("Team A", "Team B", "Team C", "Team D"))
    expect_equal(log_entries$pool_before[[2]], 2L)
    expect_equal(log_entries$pool_after[[2]], 0L)

    expect_s3_class(log_entries$timestamp, "POSIXct")
    expect_type(log_entries$rule_applied, "character")
    expect_true(nzchar(log_entries$rule_applied[[1]]))
})

test_that("routing log order is deterministic by declaration order", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "b_sink",
            transition_id = "b_transition",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 1L
        ) |>
        single_elim(
            "a_sink",
            from = "groups",
            transition_id = "a_transition",
            take = filter_by(function(source_pool, ...) head(source_pool, 2)),
            consume = TRUE,
            priority = 1L
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    log_entries <- routing_log(tournament)
    expect_equal(
        log_entries$transition_id,
        c("b_transition", "a_transition")
    )
})
