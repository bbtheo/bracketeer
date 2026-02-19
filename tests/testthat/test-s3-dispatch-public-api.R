test_that("public generics dispatch for bracket objects", {
    b <- new_single_elim_bracket(paste("Team", 1:4))

    expect_s3_class(get_matches(b), "data.frame")
    expect_s3_class(get_standings(b), "data.frame")
    expect_false(is_complete(b))

    b <- set_result(b, 1, 1, 0)
    expect_s3_class(get_matches(b), "data.frame")
})

test_that("public generics dispatch for group_stage_knockout objects", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_group_stage_knockout_bracket(teams, groups = 2, advance_per_group = 1)

    expect_s3_class(get_matches(b), "data.frame")
    expect_s3_class(get_standings(b), "data.frame")
    expect_false(is_complete(b))
})

test_that("tournament inspection verbs dispatch for tournament objects", {
    trn <- tournament(paste("Team", LETTERS[1:4])) |>
        single_elim("finals")

    expect_true(is.na(winner(trn)))
    expect_s3_class(matches(trn, "finals", status = "all"), "data.frame")
    expect_s3_class(standings(trn, "finals"), "data.frame")

    trn <- result(trn, stage = "finals", match = 1, score = c(1, 0), auto_advance = FALSE)
    trn <- result(trn, stage = "finals", match = 2, score = c(0, 1), auto_advance = FALSE)
    trn <- result(trn, stage = "finals", match = 3, score = c(1, 0), auto_advance = FALSE)

    expect_equal(winner(trn), "Team A")
})
