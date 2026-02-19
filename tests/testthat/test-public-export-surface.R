test_that("public export surface matches planned tournament API", {
    exports <- sort(getNamespaceExports("bracketeer"))

    expected <- sort(c(
        "advance", "bottom_n", "bottom_per_group", "build",
        "double_elim", "filter_by", "group_stage_knockout", "losers",
        "matches", "previous_stage", "rankings", "remaining",
        "result", "results", "round_robin", "routing_log", "single_elim",
        "slice_per_group", "slice_range", "spec", "stage_status",
        "standings", "swiss", "teardown", "top_n", "top_per_group",
        "tournament", "two_leg", "validate", "winner"
    ))

    expect_setequal(exports, expected)
})

test_that("runtime verbs expose stage-based argument names", {
    advance_args <- names(formals(advance))
    teardown_args <- names(formals(teardown))

    expect_equal(advance_args[[1]], "x")
    expect_equal(teardown_args[[1]], "x")
    expect_equal(advance_args[[2]], "stage")
    expect_equal(teardown_args[[2]], "stage")
    expect_false("stage_id" %in% advance_args)
    expect_false("stage_id" %in% teardown_args)
})
