complete_all_playable_matches <- function(bracket, score1 = 1, score2 = 0) {
    repeat {
        playable_ids <- vapply(Filter(function(match) {
            match$status != "complete" &&
                !is.na(match$participant1) &&
                !is.na(match$participant2)
        }, bracket$matches), `[[`, integer(1), "id")

        if (length(playable_ids) == 0L) {
            break
        }

        for (match_id in playable_ids) {
            bracket <- set_result(
                bracket,
                match_id = match_id,
                score1 = score1,
                score2 = score2
            )
        }
    }

    bracket
}

elimination_table <- function(bracket) {
    completed <- Filter(function(match) {
        identical(match$status, "complete") && !is.na(match$loser)
    }, bracket$matches)

    if (length(completed) == 0L) {
        return(data.frame(
            participant = character(0),
            elimination_round = integer(0),
            event_index = integer(0),
            seed_rank = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    event_table <- data.frame(
        participant = vapply(completed, `[[`, character(1), "loser"),
        elimination_round = vapply(completed, `[[`, integer(1), "round"),
        event_index = seq_along(completed),
        stringsAsFactors = FALSE
    )

    participants <- bracket$participants_df[, c("name", "seed_rank"), drop = FALSE]
    names(participants) <- c("participant", "seed_rank")

    merged <- merge(event_table, participants, by = "participant", all.x = TRUE, sort = FALSE)
    merged$seed_rank <- as.integer(merged$seed_rank)
    merged
}

test_that("qualify_losers round = all is deterministic by elimination_round", {
    bracket <- new_single_elim_bracket(paste("Team", LETTERS[1:8]))
    bracket <- complete_all_playable_matches(bracket)

    selector <- losers(round = "all")
    selected <- evaluate_selector(
        selector,
        source_bracket = bracket,
        standings = get_standings(bracket)
    )

    eliminated <- elimination_table(bracket)
    expected <- eliminated$participant[
        order(eliminated$elimination_round, eliminated$seed_rank, eliminated$participant)
    ]

    expect_equal(selected, expected)
})

test_that("qualify_losers round = latest selects only latest elimination set", {
    bracket <- new_single_elim_bracket(paste("Team", LETTERS[1:8]), third_place = TRUE)
    bracket <- complete_all_playable_matches(bracket)

    selector <- losers(round = "latest")
    selected <- evaluate_selector(
        selector,
        source_bracket = bracket,
        standings = get_standings(bracket)
    )

    eliminated <- elimination_table(bracket)
    latest_round <- max(eliminated$elimination_round)
    latest <- eliminated[eliminated$elimination_round == latest_round, , drop = FALSE]
    expected <- latest$participant[order(latest$seed_rank, latest$participant)]

    expect_equal(selected, expected)
})

test_that("qualify_losers round integer vector filters rounds", {
    bracket <- new_single_elim_bracket(paste("Team", LETTERS[1:8]))
    bracket <- complete_all_playable_matches(bracket)

    selector <- losers(round = c(1, 2))
    selected <- evaluate_selector(
        selector,
        source_bracket = bracket,
        standings = get_standings(bracket)
    )

    eliminated <- elimination_table(bracket)
    filtered <- eliminated[eliminated$elimination_round %in% c(1L, 2L), , drop = FALSE]
    expected <- filtered$participant[
        order(filtered$elimination_round, filtered$seed_rank, filtered$participant)
    ]

    expect_equal(selected, expected)
})

test_that("qualify_losers ordering modes are deterministic", {
    bracket <- new_single_elim_bracket(paste("Team", LETTERS[1:8]))
    bracket <- complete_all_playable_matches(bracket)

    eliminated <- elimination_table(bracket)
    round_one <- eliminated[eliminated$elimination_round == 1L, , drop = FALSE]

    by_seed <- evaluate_selector(
        losers(round = 1, ordering = "source_seed"),
        source_bracket = bracket,
        standings = get_standings(bracket)
    )
    as_recorded <- evaluate_selector(
        losers(round = 1, ordering = "as_recorded"),
        source_bracket = bracket,
        standings = get_standings(bracket)
    )

    expected_by_seed <- round_one$participant[order(round_one$seed_rank, round_one$participant)]
    expected_as_recorded <- round_one$participant[order(round_one$event_index)]

    expect_equal(by_seed, expected_by_seed)
    expect_equal(as_recorded, expected_as_recorded)
    expect_false(identical(by_seed, as_recorded))
})

test_that("qualify_losers round validation reports available rounds", {
    bracket <- new_single_elim_bracket(paste("Team", LETTERS[1:8]))
    bracket <- complete_all_playable_matches(bracket)

    selector <- losers(round = 99)
    expect_error(
        evaluate_selector(
            selector,
            source_bracket = bracket,
            standings = get_standings(bracket)
        ),
        "available rounds"
    )
})
