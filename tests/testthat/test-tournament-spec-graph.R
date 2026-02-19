test_that("spec creates an empty validated graph", {
    s <- spec()

    expect_s3_class(s, "bracketeer_spec")
    expect_s3_class(s, "tournament_spec")
    expect_length(s$stages, 0L)
    expect_length(s$edges, 0L)
    expect_equal(s$metadata$source_stage_ids, character(0))
    expect_equal(s$metadata$topological_order, character(0))
})

test_that("stage verbs enforce unique ids and reserved separator constraints", {
    s <- spec() |>
        round_robin("groups")

    expect_true("groups" %in% names(s$stages))
    expect_error(
        s |> single_elim("groups"),
        "already exists"
    )
    expect_error(
        spec() |> round_robin("group::phase"),
        "\\:\\:"
    )
})

test_that("stage verbs register deterministic transition IDs", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs") |>
        single_elim("consolation", from = "groups")

    expect_true("groups_to_playoffs" %in% names(s$edges))
    expect_true("groups_to_consolation" %in% names(s$edges))
    expect_equal(s$edges$groups_to_playoffs$from_stage_id, "groups")
    expect_equal(s$edges$groups_to_playoffs$to_stage_id, "playoffs")
})

test_that("from validation errors remain actionable", {
    expect_error(
        spec() |>
            round_robin("groups") |>
            single_elim("playoffs", from = "missing"),
        "Unknown source stage_id: missing"
    )
})

test_that("previous_stage resolves and errors clearly", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", from = previous_stage())

    expect_equal(s$edges$groups_to_playoffs$from_stage_id, "groups")
    expect_error(
        spec() |> single_elim("only_stage", from = previous_stage()),
        "Cannot resolve from_previous\\(\\)"
    )
})

test_that("print.tournament_spec shows a readable graph", {
    s <- spec() |>
        round_robin("group_phase") |>
        single_elim("championship") |>
        single_elim("consolation", from = "group_phase")

    out <- capture.output(print(s))
    expect_true(any(grepl("\\[ group_phase \\] ---> \\[ championship \\]", out)))
    expect_true(any(grepl("\\[ group_phase \\] ---> \\[ consolation \\]", out)))
})

