test_that("group stage advances to knockout", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_group_stage_knockout_bracket(teams, groups = 2, advance_per_group = 1)

    matches <- get_matches(b)
    for (i in seq_len(nrow(matches))) {
        b <- set_result(b, matches$id[i], 1, 0)
    }

    b <- advance(b)
    expect_equal(b$phase, "knockout")
    expect_equal(length(b$knockout_bracket$participants), 2L)
})
