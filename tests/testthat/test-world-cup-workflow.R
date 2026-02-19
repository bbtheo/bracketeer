test_that("FIFA-style group stage to knockout workflow completes", {
    teams <- paste("Team", sprintf("%02d", 1:32))
    trn <- tournament(teams) |>
        round_robin("groups", groups = 8) |>
        single_elim("knockout", third_place = TRUE, take = top_per_group(2))

    # Complete all group-stage matches.
    group_matches <- matches(trn, "groups", status = "pending")
    for (i in seq_len(nrow(group_matches))) {
        trn <- result(
            trn,
            stage = "groups",
            match = group_matches$id[i],
            score = c(1, 0)
        )
    }

    expect_true(isTRUE(trn$stage_state$knockout$materialized))
    expect_equal(length(trn$stage_state$knockout$participants), 16L)
    expect_equal(nrow(matches(trn, "knockout", status = "pending")), 16L)

    # Play knockout rounds until complete.
    for (step in seq_len(32)) {
        if (isTRUE(trn$completed)) break
        km <- matches(trn, "knockout", status = "pending")
        ready <- !is.na(km$participant1) & !is.na(km$participant2)
        expect_true(any(ready))
        for (mid in km$id[ready]) {
            trn <- result(trn, stage = "knockout", match = mid, score = c(1, 0))
        }
    }

    expect_true(isTRUE(trn$completed))
    expect_false(is.na(winner(trn)))
})

test_that("World Cup control-room workflow uses tournament DSL helpers", {
    teams <- paste("Team", sprintf("%02d", 1:8))

    spec <- spec() |>
        round_robin("group_table") |>
        single_elim("championship", take = top_n(4)) |>
        single_elim("placement", from = "group_table", take = remaining())

    preflight <- validate(spec, n = length(teams))
    expect_true(isTRUE(preflight$ok))
    expect_equal(unname(preflight$stage_counts[c("group_table", "championship", "placement")]), c(8L, 4L, 4L))

    trn <- tournament(teams) |>
        round_robin("group_table") |>
        single_elim("championship", take = top_n(4)) |>
        single_elim("placement", from = "group_table", take = remaining())

    # Complete all source-stage matches using fluent result helper.
    group_matches <- matches(trn, "group_table", status = "pending")
    for (i in seq_len(nrow(group_matches))) {
        trn <- result(
            trn,
            stage = "group_table",
            match = group_matches$id[i],
            score = c(1, 0)
        )
    }

    expect_true(trn$stage_state$championship$materialized)
    expect_true(trn$stage_state$placement$materialized)

    log_entries <- routing_log(trn)
    expect_equal(nrow(log_entries), 2L)
    expect_equal(log_entries$transition_id, c("group_table_to_championship", "group_table_to_placement"))
    expect_equal(log_entries$pool_before, c(8L, 4L))
    expect_equal(log_entries$pool_after, c(4L, 0L))
})

simulate_tournament_to_completion <- function(tournament, score1 = 1, score2 = 0,
                                              max_steps = 256L) {
    for (step in seq_len(max_steps)) {
        if (isTRUE(tournament$completed)) {
            break
        }

        active_stage_ids <- tournament$active_stage_ids
        if (length(active_stage_ids) == 0L) {
            break
        }

        progressed <- FALSE

        for (stage_id in active_stage_ids) {
            stage_bracket <- tournament$stage_state[[stage_id]]$bracket
            pending <- get_matches(stage_bracket, status = "pending")
            ready <- pending[!is.na(pending$participant1) & !is.na(pending$participant2), , drop = FALSE]

            if (nrow(ready) > 0L) {
                for (match_id in ready$id) {
                    tournament <- result(
                        tournament,
                        stage = stage_id,
                        match = match_id,
                        score = c(score1, score2)
                    )
                }
                progressed <- TRUE
            }

            if (identical(tournament$stage_state[[stage_id]]$status, "complete")) {
                tournament <- advance(tournament, stage_id = stage_id)
                progressed <- TRUE
            }
        }

        if (!isTRUE(progressed)) {
            break
        }
    }

    tournament
}

test_that("LoL Worlds style Swiss-to-top-cut workflow can be fully simulated to a winner", {
    teams <- c(
        "Gen.G", "T1", "Hanwha Life", "Dplus KIA",
        "BLG", "TES", "LNG", "JDG",
        "G2", "Fnatic", "MAD Lions", "BDS",
        "Cloud9", "Team Liquid", "FlyQuest", "100 Thieves"
    )

    spec <- spec() |>
        swiss("worlds_swiss", rounds = 1, allow_ties = FALSE) |>
        single_elim(
            "worlds_knockout",
            from = "worlds_swiss",
            transition_id = "worlds_swiss_to_knockout",
            take = top_n(8)
        ) |>
        single_elim(
            "worlds_placement",
            from = "worlds_swiss",
            transition_id = "worlds_swiss_to_placement",
            take = remaining()
        )

    preflight <- validate(spec, n = length(teams))
    expect_true(isTRUE(preflight$ok))

    trn <- tournament(teams) |>
        swiss("worlds_swiss", rounds = 1, allow_ties = FALSE) |>
        single_elim(
            "worlds_knockout",
            from = "worlds_swiss",
            transition_id = "worlds_swiss_to_knockout",
            take = top_n(8)
        ) |>
        single_elim(
            "worlds_placement",
            from = "worlds_swiss",
            transition_id = "worlds_swiss_to_placement",
            take = remaining()
        )
    trn <- simulate_tournament_to_completion(trn, score1 = 1, score2 = 0)

    expect_true(isTRUE(trn$completed))
    expect_true(trn$stage_state$worlds_knockout$materialized)
    expect_true(trn$stage_state$worlds_placement$materialized)

    knockout_winner <- get_winner(trn$stage_state$worlds_knockout$bracket)
    expect_false(is.na(knockout_winner))
    expect_true(knockout_winner %in% teams)
})
