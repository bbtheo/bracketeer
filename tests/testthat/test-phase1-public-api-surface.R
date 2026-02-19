test_that("phase1 public API: legacy transition helpers are not exported", {
    exports <- getNamespaceExports("bracketeer")

    expect_false("add_transition" %in% exports)
    expect_false("split_stage" %in% exports)
})

test_that("phase1 public API: legacy routing/export helpers are not exported", {
    exports <- getNamespaceExports("bracketeer")

    expect_false("get_ready_stages" %in% exports)
    expect_false("get_routing_log" %in% exports)
    expect_false("export_matches" %in% exports)
    expect_false("export_standings" %in% exports)
    expect_false("export_tournament_log" %in% exports)
})

test_that("phase1 public API: legacy spec entry points are not exported", {
    exports <- getNamespaceExports("bracketeer")

    expect_false("tournament_spec" %in% exports)
    expect_false("add_stage" %in% exports)
    expect_false("from_previous" %in% exports)
    expect_false("build_tournament" %in% exports)
    expect_false("validate_tournament_spec" %in% exports)
    expect_false("validate_tournament" %in% exports)
    expect_false("set_outcome" %in% exports)
})

test_that("phase1 public API: deprecated helper verbs are not exported", {
    exports <- getNamespaceExports("bracketeer")

    expect_false("is_stage_complete" %in% exports)
    expect_false("qualify_remaining" %in% exports)
    expect_false("qualify_losers" %in% exports)
})

test_that("phase1 public API: legacy stage constructors are not exported", {
    exports <- getNamespaceExports("bracketeer")

    expect_false("single_elim_stage" %in% exports)
    expect_false("double_elim_stage" %in% exports)
    expect_false("round_robin_stage" %in% exports)
    expect_false("swiss_stage" %in% exports)
    expect_false("group_stage_knockout_stage" %in% exports)
    expect_false("two_leg_stage" %in% exports)
})
