test_that("swiss assigns a bye for odd participants", {
    teams <- paste("Team", LETTERS[1:5])
    b <- new_swiss_bracket(teams, rounds = 3)

    bye_matches <- Filter(function(m) is.na(m$participant2), b$matches)
    expect_equal(length(bye_matches), 1L)
    expect_equal(bye_matches[[1]]$status, "complete")
})
