test_that("spec stage verbs are functional and do not mutate previous specs", {
    base <- spec()
    s1 <- base |>
        round_robin("groups")
    s2 <- s1 |>
        single_elim("playoffs")

    expect_length(base$stages, 0L)
    expect_length(base$edges, 0L)

    expect_equal(names(s1$stages), "groups")
    expect_length(s1$edges, 0L)

    expect_equal(names(s2$stages), c("groups", "playoffs"))
    expect_equal(names(s2$edges), "groups_to_playoffs")
})

test_that("linear stage chains wire from prior stage deterministically", {
    s <- spec() |>
        round_robin("stage1") |>
        swiss("stage2", rounds = 1) |>
        single_elim("stage3", take = top_n(8))

    expect_equal(s$metadata$stage_order, c("stage1", "stage2", "stage3"))
    expect_equal(names(s$edges), c("stage1_to_stage2", "stage2_to_stage3"))
    expect_equal(s$edges$stage1_to_stage2$from_stage_id, "stage1")
    expect_equal(s$edges$stage2_to_stage3$from_stage_id, "stage2")
})

test_that("default transition selector is all participants when take is omitted", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    edge <- s$edges$groups_to_playoffs
    expect_s3_class(edge, "transition_spec")
    expect_s3_class(edge$take, "bracketeer_selector")
    expect_equal(edge$take$kind, "all")
    expect_true(isTRUE(edge$consume))
    expect_equal(edge$seeding, "by_source_rank")
})

test_that("branching spec validation computes expected stage counts", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("championship", take = top_n(8)) |>
        single_elim("consolation", from = "groups", take = remaining())

    preflight <- validate(s, n = 16)

    expect_true(isTRUE(preflight$ok))
    expect_equal(
        unname(preflight$stage_counts[c("groups", "championship", "consolation")]),
        c(16L, 8L, 8L)
    )
})

test_that("the same spec can be built for multiple participant sizes independently", {
    s <- spec() |>
        single_elim("finals")

    trn4 <- build(s, paste("Team", 1:4))
    trn8 <- build(s, paste("Team", 1:8))

    expect_equal(nrow(matches(trn4, "finals", status = "all")), 3L)
    expect_equal(nrow(matches(trn8, "finals", status = "all")), 7L)

    expect_equal(names(s$stages), "finals")
    expect_length(s$edges, 0L)
})

test_that("explicit transition_id is preserved in spec graph and validation", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("top_cut", transition_id = "custom_groups_to_top_cut", take = top_n(4))

    expect_true("custom_groups_to_top_cut" %in% names(s$edges))
    expect_equal(s$edges$custom_groups_to_top_cut$from_stage_id, "groups")
    expect_equal(s$edges$custom_groups_to_top_cut$to_stage_id, "top_cut")

    preflight <- validate(s, n = 16)
    expect_true(isTRUE(preflight$ok))
    expect_equal(unname(preflight$stage_counts[c("groups", "top_cut")]), c(16L, 4L))
})
