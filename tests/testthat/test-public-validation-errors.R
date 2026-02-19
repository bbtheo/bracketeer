test_that("runtime errors include actionable stage_id and match_id identifiers", {
    trn <- tournament(paste("Team", LETTERS[1:4]), auto_advance = FALSE) |>
        round_robin("groups")

    expect_error(matches(trn, stage = "missing"), "Unknown stage_id `missing`")
    expect_error(standings(trn, stage = "missing"), "Unknown stage_id `missing`")
    expect_error(advance(trn, stage_id = "missing"), "Unknown stage_id `missing`")
    expect_error(teardown(trn, stage_id = "missing"), "Unknown stage_id `missing`")

    expect_error(
        result(trn, stage = "groups", match = "bad", score = c(1, 0)),
        "Unknown match_id `bad` for stage_id `groups`"
    )
})

test_that("public entry validation messages include expected and actual details", {
    teams <- paste("Team", LETTERS[1:4])

    expect_error(
        build(123, teams),
        "expected `bracketeer_spec` or `tournament_spec`.*actual class `numeric`"
    )
    expect_error(
        validate(123, n = 4),
        "expected `bracketeer_spec` or `tournament_spec`.*actual class `numeric`"
    )
    expect_error(
        tournament(teams, auto_advance = "yes"),
        "expected logical scalar TRUE/FALSE.*actual class `character`"
    )
})
