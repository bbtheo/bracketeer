test_that("best-of supports per-game scores", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_single_elim_bracket(teams, best_of = 3)

    b <- set_result(b, 1, c(1, 0, 3), c(0, 2, 1))
    m <- b$matches[[1]]

    expect_equal(m$score1, 2)
    expect_equal(m$score2, 1)
    expect_equal(m$winner, m$participant1)
})
