test_that("double elimination reseeds winners bracket", {
    teams <- data.frame(
        name = paste("Team", 1:8),
        seed = 1:8,
        stringsAsFactors = FALSE
    )
    b <- new_double_elim_bracket(teams, reseed = TRUE)

    round1 <- Filter(function(m) m$bracket_type == "winners" && m$round == 1, b$matches)
    seeds <- setNames(b$participants_df$seed_rank, b$participants_df$name)
    winners <- character(0)

    for (m in round1) {
        p1 <- m$participant1
        p2 <- m$participant2
        winner <- if (seeds[p1] <= seeds[p2]) p1 else p2
        winners <- c(winners, winner)
        b <- set_winner(b, m$id, winner)
    }

    b <- advance(b)

    round2 <- Filter(function(m) m$bracket_type == "winners" && m$round == 2, b$matches)
    expect_equal(length(round2), 2L)

    winners <- winners[order(seeds[winners])]
    expected_pairs <- list(
        sort(c(winners[1], winners[2])),
        sort(c(winners[3], winners[4]))
    )
    expected_pairs <- sort(vapply(expected_pairs, paste, character(1), collapse = ","))

    pairs <- lapply(round2, function(m) sort(c(m$participant1, m$participant2)))
    pairs <- sort(vapply(pairs, paste, character(1), collapse = ","))

    expect_equal(pairs, expected_pairs)
})
