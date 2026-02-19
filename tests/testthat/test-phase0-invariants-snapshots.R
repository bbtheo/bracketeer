test_that("phase0 snapshot: transition IDs remain deterministic", {
    s <- tournament_spec() |>
        add_stage("groups", round_robin_stage()) |>
        add_stage("playoffs", single_elim_stage()) |>
        add_stage("consolation", single_elim_stage()) |>
        add_transition(from = "groups", to = "playoffs") |>
        add_transition(from = "groups", to = "playoffs") |>
        add_transition(from = "groups", to = "consolation")

    expect_snapshot_value(names(s$edges), style = "json2")
})

test_that("phase0 snapshot: print.tournament_spec graph stays stable", {
    s <- tournament_spec() |>
        add_stage("groups", round_robin_stage()) |>
        add_stage("playoffs", single_elim_stage()) |>
        add_transition(from = "groups", to = "playoffs")

    expect_snapshot(print(s))
})
