test_that("swiss avoids immediate rematches when possible", {
    teams <- paste("Team", LETTERS[1:6])
    b <- new_swiss_bracket(teams, rounds = 2)

    # Complete round 1 with deterministic winners
    r1 <- Filter(function(m) m$round == 1, b$matches)
    for (m in r1) {
        b <- set_result(b, m$id, 1, 0)
    }

    b <- advance(b)
    r2 <- Filter(function(m) m$round == 2, b$matches)

    # Build set of round-1 pairings
    r1_pairs <- vapply(r1, function(m) {
        paste(sort(c(m$participant1, m$participant2)), collapse = "-")
    }, character(1))

    r2_pairs <- vapply(r2, function(m) {
        paste(sort(c(m$participant1, m$participant2)), collapse = "-")
    }, character(1))

    expect_true(all(!r2_pairs %in% r1_pairs))
})

test_that("group assignment uses snake distribution", {
    teams <- data.frame(
        name = paste("Team", 1:8),
        seed = 1:8,
        stringsAsFactors = FALSE
    )

    b <- new_group_stage_knockout_bracket(teams, groups = 2, advance_per_group = 2)
    groups <- b$participants_df$group
    # Expect alternating groups due to snake distribution
    expect_equal(groups[1:4], c("A", "B", "B", "A"))
})

test_that("two-leg knockout with away_goals FALSE rejects aggregate ties", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_two_leg_bracket(teams, away_goals = FALSE)

    expect_error(
        set_result(b, 1, c(1, 1), c(1, 1)),
        "cannot be tied"
    )
})
