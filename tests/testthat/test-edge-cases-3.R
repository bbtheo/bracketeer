test_that("swiss bye assignment is fair across rounds", {
    teams <- paste("Team", LETTERS[1:5])
    b <- new_swiss_bracket(teams, rounds = 3)

    # Track bye recipients across rounds
    bye_recipients <- character(0)

    for (r in 1:3) {
        round_matches <- Filter(function(m) m$round == r, b$matches)
        bye <- Filter(function(m) is.na(m$participant2), round_matches)
        if (length(bye) == 1) {
            bye_recipients <- c(bye_recipients, bye[[1]]$participant1)
        }

        # Complete all matches
        for (m in round_matches) {
            if (is.na(m$participant2)) next
            b <- set_result(b, m$id, 1, 0)
        }

        if (r < 3) {
            b <- advance(b)
        }
    }

    expect_true(length(unique(bye_recipients)) >= 2)
})

test_that("tiebreakers are stable when matches are incomplete", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_round_robin_bracket(teams, tiebreakers = c("points", "score_diff", "alphabetical"))

    # Only complete one match
    b <- set_result(b, 1, 1, 0)

    standings <- get_standings(b)
    expect_equal(length(standings$participant), 4L)
})

test_that("double elimination grand final reset behavior", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_double_elim_bracket(teams, grand_final_reset = TRUE)

    # Complete winners bracket to establish winner
    wb_match_ids <- vapply(Filter(function(m) m$bracket_type == "winners", b$matches), `[[`, integer(1), "id")
    for (match_id in wb_match_ids) {
        match <- b$matches[[match_id]]
        b <- set_winner(b, match_id, match$participant1)
    }

    # Complete losers bracket to set challenger
    lb_match_ids <- vapply(Filter(function(m) m$bracket_type == "losers", b$matches), `[[`, integer(1), "id")
    for (match_id in lb_match_ids) {
        match <- b$matches[[match_id]]
        b <- set_winner(b, match_id, match$participant1)
    }

    gf_id <- b$grand_final_id
    gf <- b$matches[[gf_id]]

    # Force winners bracket champ to lose to trigger reset
    loser <- gf$participant2
    b <- set_winner(b, gf_id, loser)

    expect_true(!is.na(b$grand_final_reset_id))

    reset <- b$matches[[b$grand_final_reset_id]]
    b <- set_winner(b, reset$id, reset$participant1)

    expect_true(is_complete(b))
})
