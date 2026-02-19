test_that("validate_tournament dry-run succeeds on a feasible linear flow", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(4))

    preflight <- validate(spec, n = 16)

    expect_true(is.list(preflight))
    expect_true(isTRUE(preflight$ok))
    expect_equal(preflight$n_participants, 16L)
    expect_equal(unname(preflight$stage_counts[c("groups", "playoffs")]), c(16L, 4L))
})

test_that("validate_tournament detects infeasible transition flow", {
    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(1))

    expect_error(
        validate(spec, n = 8),
        "stage_id `playoffs`.*selected 1 participant\\(s\\).*transition_id\\(s\\) `groups_to_playoffs`.*requires at least 2"
    )
})

test_that("validate_tournament detects qualifier-count and group-size mismatches with hints", {
    qualifier_mismatch_spec <- spec() |>
        round_robin("groups") |>
        group_stage_knockout(
            "finals",
            groups = 2,
            advance_per_group = 2,
            take = top_n(3)
        )

    expect_error(
        validate(qualifier_mismatch_spec, n = 8),
        "stage_id `finals`.*selected 3 participant\\(s\\).*requires at least 4"
    )

    source_group_size_spec <- spec() |>
        group_stage_knockout("group_phase", groups = 4, advance_per_group = 1)

    expect_error(
        validate(source_group_size_spec, n = 3),
        "stage_id `group_phase`.*requires at least 4 participant\\(s\\).*Hint: increase `n_participants` or reduce `groups`/`advance_per_group`"
    )
})
