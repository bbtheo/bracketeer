complete_round_robin_by_rating <- function(tournament, stage_id, rating) {
    matches <- get_matches(tournament$stage_state[[stage_id]]$bracket, status = "all")

    for (i in seq_len(nrow(matches))) {
        match_id <- matches$id[[i]]
        p1 <- as.character(matches$participant1[[i]])
        p2 <- as.character(matches$participant2[[i]])

        if (is.na(p1) || is.na(p2)) {
            next
        }

        if (rating[[p1]] >= rating[[p2]]) {
            tournament <- set_result(
                tournament,
                stage_id = stage_id,
                match_id = match_id,
                score1 = 1,
                score2 = 0,
                auto_advance = FALSE
            )
        } else {
            tournament <- set_result(
                tournament,
                stage_id = stage_id,
                match_id = match_id,
                score1 = 0,
                score2 = 1,
                auto_advance = FALSE
            )
        }
    }

    tournament
}

test_that("stage verbs store take as selector object with default all-participants selector", {
    s <- spec() |>
        round_robin("groups") |>
        single_elim("playoffs")

    expect_equal(length(s$edges), 1L)
    edge <- s$edges[[1]]

    expect_s3_class(edge$take, "bracketeer_selector")
    expect_equal(edge$take$kind, "all")
})

test_that("top_n selector routes top-ranked participants", {
    teams <- paste("Team", LETTERS[1:4])
    rating <- c("Team A" = 4, "Team B" = 3, "Team C" = 2, "Team D" = 1)

    trn <- tournament(teams, auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("playoffs", take = top_n(2))

    trn <- complete_round_robin_by_rating(trn, "groups", rating)
    trn <- advance(trn, stage_id = "groups")

    expect_true(trn$stage_state$playoffs$materialized)
    expect_equal(trn$stage_state$playoffs$participants, c("Team A", "Team B"))
})

test_that("remaining selector routes participants left after consuming selectors", {
    teams <- paste("Team", LETTERS[1:4])
    rating <- c("Team A" = 4, "Team B" = 3, "Team C" = 2, "Team D" = 1)

    trn <- tournament(teams, auto_advance = FALSE) |>
        round_robin("groups") |>
        single_elim("championship", take = top_n(2), consume = TRUE) |>
        single_elim("consolation", from = "groups", take = remaining(), consume = TRUE)

    trn <- complete_round_robin_by_rating(trn, "groups", rating)
    trn <- advance(trn, stage_id = "groups")

    expect_true(trn$stage_state$championship$materialized)
    expect_true(trn$stage_state$consolation$materialized)
    expect_equal(trn$stage_state$championship$participants, c("Team A", "Team B"))
    expect_equal(trn$stage_state$consolation$participants, c("Team C", "Team D"))
})

test_that("grouped selectors evaluate deterministically by group standings", {
    standings <- data.frame(
        participant = c("A1", "A2", "A3", "B1", "B2", "B3"),
        group = c("A", "A", "A", "B", "B", "B"),
        stringsAsFactors = FALSE
    )

    expect_equal(
        evaluate_selector(top_per_group(2), standings = standings),
        c("A1", "A2", "B1", "B2")
    )
    expect_equal(
        evaluate_selector(bottom_per_group(1), standings = standings),
        c("A3", "B3")
    )
    expect_equal(
        evaluate_selector(slice_per_group(2, 3), standings = standings),
        c("A2", "A3", "B2", "B3")
    )
})

test_that("grouped selectors error clearly when source standings are not grouped", {
    standings <- data.frame(
        participant = c("A1", "A2", "A3", "A4"),
        stringsAsFactors = FALSE
    )

    expect_error(
        evaluate_selector(top_per_group(2), standings = standings),
        "requires grouped standings"
    )
    expect_error(
        evaluate_selector(bottom_per_group(2), standings = standings),
        "requires grouped standings"
    )
    expect_error(
        evaluate_selector(slice_per_group(1, 2), standings = standings),
        "requires grouped standings"
    )
})

complete_stage_all_home_wins <- function(tournament, stage_id) {
    matches <- get_matches(tournament$stage_state[[stage_id]]$bracket, status = "all")
    for (i in seq_len(nrow(matches))) {
        tournament <- set_result(
            tournament,
            stage_id = stage_id,
            match_id = matches$id[[i]],
            score1 = 1,
            score2 = 0,
            auto_advance = FALSE
        )
    }
    tournament
}

test_that("round_robin groups creates balanced groups and group standings", {
    teams <- paste("Team", sprintf("%02d", 1:16))
    trn <- tournament(teams, auto_advance = FALSE) |>
        round_robin("groups", groups = 4)

    participants_df <- trn$stage_state$groups$bracket$participants_df
    expect_true("group" %in% names(participants_df))
    expect_equal(as.integer(table(participants_df$group)), rep(4L, 4L))

    standings <- get_standings(trn$stage_state$groups$bracket)
    expect_true("group" %in% names(standings))
})

test_that("top_per_group selector integrates with grouped round robin transitions", {
    teams <- paste("Team", sprintf("%02d", 1:16))
    trn <- tournament(teams, auto_advance = FALSE) |>
        round_robin("groups", groups = 4) |>
        single_elim("knockout", take = top_per_group(2))

    trn <- complete_stage_all_home_wins(trn, "groups")
    standings <- get_standings(trn$stage_state$groups$bracket)
    expected <- evaluate_selector(top_per_group(2), standings = standings)

    trn <- advance(trn, stage_id = "groups")

    selected <- trn$stage_state$knockout$participants
    expect_setequal(selected, expected)
    expect_equal(length(selected), 8L)

    group_map <- setNames(
        trn$stage_state$groups$bracket$participants_df$group,
        trn$stage_state$groups$bracket$participants_df$name
    )
    expect_equal(as.integer(table(group_map[selected])), rep(2L, 4L))
})
