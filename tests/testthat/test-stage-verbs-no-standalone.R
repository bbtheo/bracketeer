test_that("stage verbs do not expose standalone bracket constructors", {
    teams <- paste("Team", LETTERS[1:8])

    expect_error(single_elim(teams), "expected a `bracketeer_spec` input")
    expect_error(double_elim(teams), "expected a `bracketeer_spec` input")
    expect_error(round_robin(teams), "expected a `bracketeer_spec` input")
    expect_error(swiss(teams), "expected a `bracketeer_spec` input")
    expect_error(two_leg(teams), "expected a `bracketeer_spec` input")
    expect_error(group_stage_knockout(teams), "expected a `bracketeer_spec` input")
})
