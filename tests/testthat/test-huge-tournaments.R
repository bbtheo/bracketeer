test_that("single elimination supports tournaments with more than 10k participants", {
    teams <- sprintf("Team%03d", seq_len(101))

    trn <- tournament(teams, auto_advance = FALSE) |>
        single_elim("main")

    stage <- stage_status(trn)
    main_row <- stage[stage$stage == "main", , drop = FALSE]
    all_matches <- matches(trn, "main", status = "all")

    expect_equal(nrow(main_row), 1L)
    expect_true(main_row$materialized[[1]])
    expect_equal(main_row$total[[1]], 127L)
    expect_equal(nrow(all_matches), 127L)
})

test_that("grouped round robin supports tournaments with more than 10k participants", {
    teams <- sprintf("Team%03d", seq_len(100))

    trn <- tournament(teams, auto_advance = FALSE) |>
        round_robin("groups", groups = 50)

    group_matches <- matches(trn, "groups", status = "all")
    group_standings <- standings(trn, "groups")

    expect_equal(nrow(group_matches), 50L)
    expect_equal(nrow(group_standings), 100L)
    expect_true("group" %in% names(group_standings))
})

test_that("swiss supports tournaments with more than 10k participants", {
    teams <- sprintf("Team%03d", seq_len(100))

    trn <- tournament(teams, auto_advance = FALSE) |>
        swiss("open", rounds = 1)

    swiss_matches <- matches(trn, "open", status = "all")
    swiss_standings <- standings(trn, "open")

    expect_equal(nrow(swiss_matches), 50L)
    expect_equal(nrow(swiss_standings), 100L)
})

test_that("multi-stage specs with more than 10k participants materialize source stages", {
    teams <- sprintf("Team%03d", seq_len(100))

    x <- spec() |>
        round_robin("groups", groups = 50) |>
        single_elim("top_cut", take = top_n(32))

    trn <- build(x, teams)
    status <- stage_status(trn)
    groups_row <- status[status$stage == "groups", , drop = FALSE]
    top_cut_row <- status[status$stage == "top_cut", , drop = FALSE]

    expect_true(groups_row$materialized[[1]])
    expect_equal(groups_row$status[[1]], "in_progress")
    expect_false(top_cut_row$materialized[[1]])
    expect_equal(top_cut_row$status[[1]], "blocked")
})
