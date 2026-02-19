test_that("two-leg knockout enforces two-leg scores", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_two_leg_bracket(teams)

    expect_equal(b$type, "two_leg_knockout")
    expect_equal(b$legs, 2L)
    expect_true(isTRUE(b$home_away))

    expect_error(set_result(b, 1, 2, 1), "requires 2 legs")
})

test_that("two-leg knockout advances by aggregate", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_two_leg_bracket(teams)

    b <- set_result(b, 1, c(2, 0), c(0, 1))
    m <- b$matches[[1]]

    expect_equal(m$score1, 2)
    expect_equal(m$score2, 1)
    expect_equal(m$winner, m$participant1)

    next_id <- m$next_match
    next_slot <- m$next_slot
    advanced <- if (next_slot == 1L) {
        b$matches[[next_id]]$participant1
    } else {
        b$matches[[next_id]]$participant2
    }
    expect_equal(advanced, m$winner)
})
