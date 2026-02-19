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

order_by_source_rank <- function(tournament, stage_id, selected_names) {
    standings <- get_standings(tournament$stage_state[[stage_id]]$bracket)
    if (!"rank" %in% names(standings)) {
        standings$rank <- seq_len(nrow(standings))
    }
    rank_lookup <- as.integer(standings$rank)
    names(rank_lookup) <- as.character(standings$participant)

    selected_names[order(rank_lookup[selected_names], selected_names)]
}

snake_values <- function(values, block_size = 2L) {
    idx <- seq_along(values)
    blocks <- split(idx, ceiling(idx / block_size))
    for (i in seq_along(blocks)) {
        if (i %% 2 == 0L) {
            blocks[[i]] <- rev(blocks[[i]])
        }
    }
    values[unlist(blocks, use.names = FALSE)]
}

test_that("transition seeding defaults to by_source_rank", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "playoffs",
            seed = FALSE,
            take = filter_by(function(source_pool, ...) rev(source_pool))
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    expected <- order_by_source_rank(
        tournament = tournament,
        stage_id = "groups",
        selected_names = rev(tournament$stage_state$groups$participants)
    )
    tournament <- advance(tournament, stage_id = "groups")

    expect_equal(
        tournament$stage_state$playoffs$participants,
        expected
    )
})

test_that("transition seeding policy as_is preserves selection order", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "playoffs",
            seed = FALSE,
            seeding = "as_is",
            take = filter_by(function(source_pool, ...) rev(source_pool))
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    tournament <- advance(tournament, stage_id = "groups")

    expect_equal(
        tournament$stage_state$playoffs$participants,
        rev(tournament$stage_state$groups$participants)
    )
})

test_that("transition seeding policies cross_group and snake are deterministic", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "cross_group_sink",
            seed = FALSE,
            seeding = "cross_group",
            allow_overlap = TRUE,
            consume = FALSE,
            take = filter_by(function(source_pool, ...) rev(source_pool))
        ) |>
        single_elim(
            "snake_sink",
            from = "groups",
            seed = FALSE,
            seeding = "snake",
            allow_overlap = TRUE,
            consume = FALSE,
            take = filter_by(function(source_pool, ...) rev(source_pool))
        )

    tournament <- build(spec, participants = paste("Team", LETTERS[1:4]))
    tournament <- complete_materialized_stage(tournament, "groups")
    expected_rank <- order_by_source_rank(
        tournament = tournament,
        stage_id = "groups",
        selected_names = rev(tournament$stage_state$groups$participants)
    )
    expected_snake <- snake_values(expected_rank)
    tournament <- advance(tournament, stage_id = "groups")

    expect_equal(
        tournament$stage_state$cross_group_sink$participants,
        expected_rank
    )
    expect_equal(
        tournament$stage_state$snake_sink$participants,
        expected_snake
    )
})
