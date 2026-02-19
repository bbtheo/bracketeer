test_that("double elimination match count for 4 teams", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_double_elim_bracket(teams)

    expect_equal(length(b$matches), 6L)
    expect_equal(b$winners_rounds, 2L)
    expect_equal(b$losers_rounds, 2L)
})

test_that("double elimination winners round advances to winners final", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_double_elim_bracket(teams)

    round1_ids <- vapply(Filter(function(m) {
        m$bracket_type == "winners" && m$round == 1L
    }, b$matches), `[[`, integer(1), "id")

    for (match_id in round1_ids) {
        match <- b$matches[[match_id]]
        b <- set_winner(b, match_id, match$participant1)
    }

    winners_final <- b$matches[[b$winners_final_id]]
    expect_false(is.na(winners_final$participant1))
    expect_false(is.na(winners_final$participant2))
})
