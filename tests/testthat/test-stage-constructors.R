test_that("stage verbs validate format arguments", {
    expect_error(
        spec() |> single_elim("finals", best_of = 2),
        "best_of"
    )
    expect_error(
        spec() |> group_stage_knockout("groups", groups = 0),
        "groups"
    )
    expect_error(
        spec() |> round_robin("groups", n_rounds = 0),
        "positive integer"
    )
    expect_error(
        spec() |> swiss("swiss", rounds = 0),
        "positive integer"
    )
})
