test_that("validate_tournament errors include IDs, counts, and fix hints", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(1))

    expect_error(
        validate(spec, n = 8),
        paste(
            "stage_id `playoffs`",
            ".*selected 1 participant\\(s\\)",
            ".*transition_id\\(s\\) `groups_to_playoffs`",
            ".*requires at least 2",
            ".*Hint:",
            sep = ""
        )
    )
})

test_that("validate_tournament surfaces transition rule context with identifiers", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim(
            "playoffs",
            transition_id = "bad_rule_transition",
            take = filter_by(function(source_pool, ...) {
                stop("rule boom")
            })
        )

    expect_error(
        validate(spec, n = 8),
        paste(
            "transition_id `bad_rule_transition`",
            ".*stage_id `groups`",
            ".*rule boom",
            ".*Hint:",
            sep = ""
        )
    )
})
