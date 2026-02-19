test_that("two-leg knockout uses away goals as tiebreaker", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_two_leg_bracket(teams, away_goals = TRUE)

    # Aggregate 2-2, away goals favor participant1 (leg2)
    b <- set_result(b, 1, c(1, 1), c(0, 2))
    m <- b$matches[[1]]

    expect_equal(m$score1, 2)
    expect_equal(m$score2, 2)
    expect_equal(m$winner, m$participant1)
})
