test_that("participant validation rejects duplicates and empty names", {
    expect_error(
        new_single_elim_bracket(c("A", "A")),
        "unique"
    )

    expect_error(
        new_single_elim_bracket(c("A", "")),
        "empty"
    )

    expect_error(
        new_single_elim_bracket(c("A", NA)),
        "NA"
    )
})

test_that("best_of must be odd integer", {
    teams <- paste("Team", LETTERS[1:4])

    expect_error(
        new_single_elim_bracket(teams, best_of = 2),
        "odd"
    )

    expect_error(
        new_double_elim_bracket(teams, best_of = 0),
        "odd"
    )
})

test_that("two-leg knockout enforces leg length and away-goals tie handling", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_two_leg_bracket(teams, away_goals = TRUE)

    expect_error(
        set_result(b, 1, 2, 1),
        "requires 2 legs"
    )

    # Aggregate tied and away goals equal should error
    expect_error(
        set_result(b, 1, c(1, 1), c(1, 1)),
        "away goals equal"
    )
})

test_that("elimination formats reject tied scores", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_single_elim_bracket(teams)

    expect_error(
        set_result(b, 1, 1, 1),
        "cannot be tied"
    )
})

test_that("round robin handles odd participants without NA matches", {
    teams <- paste("Team", LETTERS[1:5])
    b <- new_round_robin_bracket(teams)

    expect_equal(length(b$matches), 10L)
    expect_true(all(!is.na(vapply(b$matches, `[[`, character(1), "participant1"))))
    expect_true(all(!is.na(vapply(b$matches, `[[`, character(1), "participant2"))))
})

test_that("group stage advance requires completion", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_group_stage_knockout_bracket(teams, groups = 2, advance_per_group = 1)

    expect_error(
        advance(b),
        "not complete"
    )
})

test_that("swiss rounds default and tie rules", {
    teams <- paste("Team", LETTERS[1:8])
    b <- new_swiss_bracket(teams)
    expect_equal(b$rounds, 3L)

    b2 <- new_swiss_bracket(teams, rounds = 2, allow_ties = FALSE)
    expect_error(
        set_result(b2, 1, 1, 1),
        "cannot be tied"
    )
})

test_that("bracket rejects stage specs directly", {
    spec <- single_elim_stage()
    expect_error(
        new_bracket(spec),
        "no longer accepts stage specs directly"
    )
})

test_that("group assignment rejects too many groups", {
    teams <- paste("Team", LETTERS[1:4])
    expect_error(
        new_group_stage_knockout_bracket(teams, groups = 5, advance_per_group = 1),
        "cannot exceed"
    )
})

test_that("round robin rejects unknown tiebreaker", {
    teams <- paste("Team", LETTERS[1:4])
    expect_error(
        new_round_robin_bracket(teams, tiebreakers = c("points", "unknown")),
        "Unknown tiebreakers"
    )
})

test_that("single elim reseed delays next round assignment", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_single_elim_bracket(teams, reseed = TRUE)

    b <- set_winner(b, 1, b$matches[[1]]$participant1)
    b <- set_winner(b, 2, b$matches[[2]]$participant1)

    final <- b$matches[[length(b$matches)]]
    expect_true(is.na(final$participant1) || is.na(final$participant2))

    b <- advance(b)
    final2 <- b$matches[[length(b$matches)]]
    expect_true(!is.na(final2$participant1) && !is.na(final2$participant2))
})
