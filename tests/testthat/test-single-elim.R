test_that("single elimination byes only resolve round 1", {
    teams <- paste("Team", LETTERS[1:6])
    b <- new_single_elim_bracket(teams)

    round2 <- get_matches(b, round = 2)
    expect_true(all(round2$status == "pending"))
})

test_that("single elimination routes winners to the next round without mutating completed matches", {
    teams <- paste("Team", LETTERS[1:8])
    b <- new_single_elim_bracket(teams, seed = FALSE)

    round1_before <- get_matches(b, round = 1)
    for (mid in round1_before$id) {
        b <- set_result(b, mid, 1, 0)
    }

    round1_after <- get_matches(b, round = 1)
    expect_equal(round1_after$participant1, round1_before$participant1)
    expect_equal(round1_after$participant2, round1_before$participant2)
    expect_true(all(round1_after$status == "complete"))

    round2 <- get_matches(b, round = 2)
    expect_true(all(!is.na(round2$participant1)))
    expect_true(all(!is.na(round2$participant2)))
})
