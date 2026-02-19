test_that("tournament graph works with stage constructors", {
    teams <- paste("Team", LETTERS[1:4])

    spec <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(2))

    trn <- build(spec, teams)
    expect_s3_class(trn, "tournament")
    expect_true("groups" %in% names(trn$stage_state))
    expect_true("playoffs" %in% names(trn$stage_state))
})

test_that("spec() creates a bracketeer_spec backed by tournament_spec", {
    x <- spec()

    expect_s3_class(x, "bracketeer_spec")
    expect_s3_class(x, "tournament_spec")
    expect_length(x$stages, 0)
    expect_length(x$edges, 0)
})

test_that("build() materializes a spec into a tournament runtime", {
    teams <- paste("Team", LETTERS[1:4])
    x <- spec() |>
        single_elim("finals")

    trn <- build(x, teams)
    expect_s3_class(trn, "tournament")
    expect_s3_class(trn, "bracketeer_spec")
    expect_true("finals" %in% names(trn$stage_state))
})

test_that("tournament() is sugar for a tournament runtime seed", {
    teams <- paste("Team", LETTERS[1:4])
    trn <- tournament(teams)

    expect_s3_class(trn, "tournament")
    expect_s3_class(trn, "bracketeer_spec")
    expect_true(isTRUE(trn$auto_advance))
    expect_equal(names(trn$stage_state), character(0))
})

test_that("stage verbs work on spec() and tournament() pipes", {
    s <- spec() |>
        round_robin("rr") |>
        single_elim("se") |>
        double_elim("de") |>
        swiss("sw") |>
        two_leg("tl") |>
        group_stage_knockout("gsk")

    expect_true(all(c("rr", "se", "de", "sw", "tl", "gsk") %in% names(s$stages)))

    teams <- paste("Team", LETTERS[1:4])
    trn <- tournament(teams) |>
        round_robin("groups")

    expect_true("groups" %in% names(trn$stage_state))
    expect_true(isTRUE(trn$stage_state$groups$materialized))
    expect_gt(nrow(get_matches(trn$stage_state$groups$bracket)), 0L)
})

test_that("stage verbs auto-wire from previous stage in linear specs", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    expect_equal(length(s$edges), 1L)
    edge <- s$edges[[1]]
    expect_equal(edge$from_stage_id, "groups")
    expect_equal(edge$to_stage_id, "playoffs")
})

test_that("stage verbs accept explicit from and previous_stage()", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", from = previous_stage()) |>
        swiss("consolation", from = "groups")

    expect_equal(length(s$edges), 2L)
    expect_true(any(vapply(s$edges, function(e) {
        identical(e$from_stage_id, "groups") && identical(e$to_stage_id, "playoffs")
    }, logical(1))))
    expect_true(any(vapply(s$edges, function(e) {
        identical(e$from_stage_id, "groups") && identical(e$to_stage_id, "consolation")
    }, logical(1))))

    expect_error(
        spec() |> single_elim("only_stage", from = previous_stage()),
        "Cannot resolve from_previous\\(\\) for destination stage_id `only_stage`"
    )
    expect_error(
        spec() |>
            round_robin("groups") |>
            single_elim("playoffs", from = "missing_stage"),
        "Unknown source stage_id: missing_stage"
    )
})

test_that("new constructors and stage verbs validate inputs at entry", {
    teams <- paste("Team", LETTERS[1:4])

    expect_error(
        build(list(), teams),
        "expected `bracketeer_spec` or `tournament_spec`"
    )
    expect_error(
        tournament(teams, auto_advance = NA),
        "expected logical scalar TRUE/FALSE"
    )
    expect_error(
        spec() |> round_robin("", groups = 2),
        "`id` must be a non-empty string"
    )
    expect_error(
        spec() |> round_robin("groups", groups = 0),
        "`groups` must be a positive integer"
    )
    expect_error(
        spec() |>
            round_robin("groups") |>
            single_elim("playoffs", take = 123),
        "`take` must be NULL, a function, or a bracketeer_selector"
    )
})

test_that("validate() runs preflight checks for a spec", {
    x <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(2))

    out <- validate(x, n = 4)
    expect_true(is.list(out))
    expect_true(isTRUE(out$ok))
})

test_that("build() runs validate() implicitly before materialization", {
    x <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(8))

    expect_error(
        build(x, participants = paste("Team", LETTERS[1:4])),
        "requests top_n\\(8\\)"
    )
})
