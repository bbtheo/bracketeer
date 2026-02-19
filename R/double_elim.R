#' Create a double elimination bracket
#'
#' Double elimination tournament with winners and losers brackets.
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' column.
#' @param ... Additional arguments passed to bracket constructors or
#'   tournament stage-verb dispatch methods.
#'
#' @return A double_elim_bracket object
#' @export
double_elim <- function(participants, ...) {
    if (inherits(participants, "bracketeer_spec")) {
        return(double_elim.bracketeer_spec(participants, ...))
    }
    stop(
        "`double_elim()` expected a `bracketeer_spec` input (from `spec()` or `tournament()`) ",
        "but got class `", paste(class(participants), collapse = "/"), "`. ",
        "For standalone bracket construction, use `new_double_elim_bracket()`."
    )
}

#' @keywords internal
double_elim.default <- function(participants, seed = TRUE,
                                grand_final_reset = TRUE, best_of = NULL,
                                reseed = FALSE) {
    info <- normalize_participants(participants)
    seeded <- apply_seed_method(info$data, seed)
    participant_names <- seeded$name
    best_of <- validate_best_of(best_of)

    n <- length(participant_names)
    if (n < 2) {
        stop("Need at least 2 participants")
    }

    seeding <- seed_participants(participant_names, seed = seed)
    bracket_size <- seeding$bracket_size
    winners_rounds <- calc_rounds_single_elim(bracket_size)

    matches <- list()
    match_id <- 1L

    # Winners bracket
    winners_match_ids <- vector("list", winners_rounds)
    for (round in seq_len(winners_rounds)) {
        n_matches_this_round <- as.integer(bracket_size / (2^round))
        winners_match_ids[[round]] <- integer(n_matches_this_round)

        for (pos in seq_len(n_matches_this_round)) {
            if (round < winners_rounds) {
                next_match <- as.integer(bracket_size * (1 - 1 / (2^round)) +
                    ceiling(pos / 2))
                next_slot <- if (pos %% 2 == 1) 1L else 2L
            } else {
                next_match <- NA_integer_
                next_slot <- NA_integer_
            }

            if (round == 1) {
                idx1 <- (pos - 1) * 2 + 1L
                idx2 <- (pos - 1) * 2 + 2L
                p1 <- seeding$slots[idx1]
                p2 <- seeding$slots[idx2]
            } else {
                p1 <- NA_character_
                p2 <- NA_character_
            }

            matches[[match_id]] <- create_match(
                id = match_id,
                round = round,
                position = pos,
                participant1 = p1,
                participant2 = p2,
                next_match = next_match,
                next_slot = next_slot,
                bracket_type = "winners"
            )
            winners_match_ids[[round]][pos] <- match_id
            match_id <- match_id + 1L
        }
    }

    losers_rounds <- if (winners_rounds > 1) 2L * (winners_rounds - 1L) else 0L
    losers_match_ids <- vector("list", losers_rounds)

    winners_final_id <- winners_match_ids[[winners_rounds]][1]

    if (winners_rounds == 1L) {
        # With two participants, the winners final is the grand final
        grand_final_id <- winners_final_id
        matches[[grand_final_id]]$bracket_type <- "grand_final"
    } else {
        # Losers bracket structure
        for (round in seq_len(losers_rounds)) {
            k <- ceiling(round / 2)
            n_matches_this_round <- as.integer(2^(winners_rounds - k - 1))
            losers_match_ids[[round]] <- integer(n_matches_this_round)

            for (pos in seq_len(n_matches_this_round)) {
                matches[[match_id]] <- create_match(
                    id = match_id,
                    round = round,
                    position = pos,
                    participant1 = NA_character_,
                    participant2 = NA_character_,
                    next_match = NA_integer_,
                    next_slot = NA_integer_,
                    bracket_type = "losers"
                )
                losers_match_ids[[round]][pos] <- match_id
                match_id <- match_id + 1L
            }
        }

        # Grand final
        grand_final_id <- match_id
        matches[[match_id]] <- create_match(
            id = match_id,
            round = winners_rounds + losers_rounds + 1L,
            position = 1L,
            participant1 = NA_character_,
            participant2 = NA_character_,
            next_match = NA_integer_,
            next_slot = NA_integer_,
            bracket_type = "grand_final"
        )

        # Wire winners final to grand final
        matches[[winners_final_id]]$next_match <- grand_final_id
        matches[[winners_final_id]]$next_slot <- 1L
    }

    # Wire losers bracket progression
    if (losers_rounds > 0) {
        for (round in seq_len(losers_rounds)) {
            for (pos in seq_along(losers_match_ids[[round]])) {
                id <- losers_match_ids[[round]][pos]
                if (round %% 2 == 1) {
                    # Odd rounds advance to next round, same position
                    next_id <- losers_match_ids[[round + 1L]][pos]
                    matches[[id]]$next_match <- next_id
                    matches[[id]]$next_slot <- 1L
                } else if (round < losers_rounds) {
                    next_id <- losers_match_ids[[round + 1L]][ceiling(pos / 2)]
                    matches[[id]]$next_match <- next_id
                    matches[[id]]$next_slot <- if (pos %% 2 == 1) 1L else 2L
                } else {
                    # Losers final advances to grand final
                    matches[[id]]$next_match <- grand_final_id
                    matches[[id]]$next_slot <- 2L
                }
            }
        }

        # Map winners bracket losers into losers bracket
        for (round in seq_len(winners_rounds)) {
            for (pos in seq_along(winners_match_ids[[round]])) {
                id <- winners_match_ids[[round]][pos]
                if (round == 1) {
                    lb_round <- 1L
                    lb_pos <- ceiling(pos / 2)
                    matches[[id]]$loser_next_match <- losers_match_ids[[lb_round]][lb_pos]
                    matches[[id]]$loser_next_slot <- if (pos %% 2 == 1) 1L else 2L
                } else {
                    lb_round <- 2L * (round - 1L)
                    matches[[id]]$loser_next_match <- losers_match_ids[[lb_round]][pos]
                    matches[[id]]$loser_next_slot <- 2L
                }
            }
        }
    }

    bracket <- list(
        type = "double_elim",
        participants = participant_names,
        participants_df = seeded,
        matches = matches,
        rounds = winners_rounds,
        winners_rounds = winners_rounds,
        losers_rounds = losers_rounds,
        bracket_size = bracket_size,
        grand_final_reset = grand_final_reset,
        grand_final_id = grand_final_id,
        winners_final_id = winners_final_id,
        grand_final_reset_id = NA_integer_,
        winners_champion = NA_character_,
        best_of = best_of,
        reseed = FALSE,
        reseed_winners = reseed,
        current_round = 1L,
        current_winners_round = 1L
    )
    class(bracket) <- c("double_elim_bracket", "bracket")

    # Process round 1 byes in winners bracket
    bracket <- process_byes(bracket, round = 1L, bracket_type = "winners")

    bracket
}

#' Internal double-elimination bracket constructor
#'
#' @inheritParams double_elim.default
#' @return A double_elim_bracket object.
#' @keywords internal
new_double_elim_bracket <- function(participants, seed = TRUE,
                                    grand_final_reset = TRUE, best_of = NULL,
                                    reseed = FALSE) {
    double_elim.default(
        participants = participants,
        seed = seed,
        grand_final_reset = grand_final_reset,
        best_of = best_of,
        reseed = reseed
    )
}

#' @keywords internal
double_elim.bracketeer_spec <- function(participants, id, seed = TRUE,
                                        grand_final_reset = TRUE, best_of = NULL,
                                        reseed = FALSE,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    add_stage_verb(
        participants,
        id = id,
        stage = double_elim_stage(
            seed = seed,
            grand_final_reset = grand_final_reset,
            best_of = best_of,
            reseed = reseed
        ),
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @keywords internal
double_elim.tournament_spec <- function(participants, id, seed = TRUE,
                                        grand_final_reset = TRUE, best_of = NULL,
                                        reseed = FALSE,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    double_elim.bracketeer_spec(
        participants = participants,
        id = id,
        seed = seed,
        grand_final_reset = grand_final_reset,
        best_of = best_of,
        reseed = reseed,
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @keywords internal
double_elim.tournament <- function(participants, id, seed = TRUE,
                                   grand_final_reset = TRUE, best_of = NULL,
                                   reseed = FALSE,
                                   from = NULL, take = NULL,
                                   seeding = "by_source_rank",
                                   consume = TRUE, allow_overlap = FALSE,
                                   priority = 1L, transition_id = NULL) {
    double_elim.bracketeer_spec(
        participants = participants,
        id = id,
        seed = seed,
        grand_final_reset = grand_final_reset,
        best_of = best_of,
        reseed = reseed,
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @rdname advance_winner
#' @export
advance_winner.double_elim_bracket <- function(bracket, match_id) {
    match <- bracket$matches[[match_id]]

    # Advance winner
    if (!is.na(match$next_match)) {
        next_id <- match$next_match
        next_slot <- match$next_slot
        if (next_slot == 1L) {
            bracket$matches[[next_id]]$participant1 <- match$winner
        } else {
            bracket$matches[[next_id]]$participant2 <- match$winner
        }
    }

    # Advance loser to losers bracket
    if (!is.na(match$loser_next_match) && !is.na(match$loser)) {
        next_id <- match$loser_next_match
        next_slot <- match$loser_next_slot
        if (next_slot == 1L) {
            bracket$matches[[next_id]]$participant1 <- match$loser
        } else {
            bracket$matches[[next_id]]$participant2 <- match$loser
        }
    }

    # Track winners bracket champion
    if (!is.na(bracket$winners_final_id) && match_id == bracket$winners_final_id) {
        bracket$winners_champion <- match$winner
    }

    # Handle grand final reset
    if (match$bracket_type == "grand_final" && isTRUE(bracket$grand_final_reset)) {
        winners_champion <- bracket$winners_champion
        if (is.na(winners_champion)) {
            winners_champion <- bracket$matches[[bracket$winners_final_id]]$winner
        }
        if (!is.na(winners_champion) && match$winner != winners_champion) {
            if (is.na(bracket$grand_final_reset_id)) {
                reset_id <- length(bracket$matches) + 1L
                bracket$matches[[reset_id]] <- create_match(
                    id = reset_id,
                    round = match$round + 1L,
                    position = 1L,
                    participant1 = match$participant1,
                    participant2 = match$participant2,
                    next_match = NA_integer_,
                    next_slot = NA_integer_,
                    bracket_type = "grand_final_reset"
                )
                bracket$grand_final_reset_id <- reset_id
            }
        }
    }

    bracket
}

#' @rdname get_standings
#' @export
get_standings.double_elim_bracket <- function(bracket) {
    participants <- bracket$participants
    n <- length(participants)

    wins <- setNames(rep(0L, n), participants)
    losses <- setNames(rep(0L, n), participants)

    for (m in bracket$matches) {
        if (m$status != "complete") next
        if (is.na(m$winner) || is.na(m$loser)) next
        wins[m$winner] <- wins[m$winner] + 1L
        losses[m$loser] <- losses[m$loser] + 1L
    }

    df <- data.frame(
        participant = participants,
        wins = wins[participants],
        losses = losses[participants],
        stringsAsFactors = FALSE
    )

    df <- df[order(df$losses, -df$wins, df$participant), ]
    df$rank <- seq_len(nrow(df))
    df <- df[, c("rank", "participant", "wins", "losses")]
    rownames(df) <- NULL
    df
}

#' @rdname get_winner
#' @export
get_winner.double_elim_bracket <- function(bracket) {
    if (is.na(bracket$grand_final_id)) return(NA_character_)

    # If no separate grand final (2-team bracket), use winners final
    gf <- bracket$matches[[bracket$grand_final_id]]
    if (gf$bracket_type == "winners") {
        if (gf$status == "complete") return(gf$winner)
        return(NA_character_)
    }

    if (!isTRUE(bracket$grand_final_reset)) {
        if (gf$status == "complete") return(gf$winner)
        return(NA_character_)
    }

    if (!is.na(bracket$grand_final_reset_id)) {
        reset <- bracket$matches[[bracket$grand_final_reset_id]]
        if (reset$status == "complete") return(reset$winner)
        return(NA_character_)
    }

    if (gf$status != "complete") return(NA_character_)

    winners_champion <- bracket$winners_champion
    if (is.na(winners_champion)) {
        winners_champion <- bracket$matches[[bracket$winners_final_id]]$winner
    }
    if (!is.na(winners_champion) && gf$winner == winners_champion) {
        return(gf$winner)
    }

    NA_character_
}

#' @rdname is_complete
#' @export
is_complete.double_elim_bracket <- function(bracket) {
    if (is.na(bracket$grand_final_id)) return(FALSE)

    gf <- bracket$matches[[bracket$grand_final_id]]
    if (gf$bracket_type == "winners") {
        return(gf$status == "complete")
    }

    if (!isTRUE(bracket$grand_final_reset)) {
        return(gf$status == "complete")
    }

    if (gf$status != "complete") return(FALSE)

    winners_champion <- bracket$winners_champion
    if (is.na(winners_champion)) {
        winners_champion <- bracket$matches[[bracket$winners_final_id]]$winner
    }

    if (!is.na(winners_champion) && gf$winner == winners_champion) {
        return(TRUE)
    }

    if (!is.na(bracket$grand_final_reset_id)) {
        reset <- bracket$matches[[bracket$grand_final_reset_id]]
        return(reset$status == "complete")
    }

    FALSE
}

#' @rdname advance
#' @export
advance.double_elim_bracket <- function(bracket, stage_id = NULL) {
    if (!isTRUE(bracket$reseed_winners)) return(bracket)

    current_round <- bracket$current_winners_round
    if (current_round >= bracket$winners_rounds) return(bracket)

    winners_round_matches <- Filter(function(m) {
        m$bracket_type == "winners" && m$round == current_round
    }, bracket$matches)

    if (length(winners_round_matches) == 0) {
        stop("No winners bracket matches found for round ", current_round)
    }
    if (!all(vapply(winners_round_matches, function(m) m$status == "complete", logical(1)))) {
        stop("Winners bracket round ", current_round, " is not complete")
    }

    next_round <- current_round + 1L
    next_idx <- which(vapply(bracket$matches, function(m) {
        m$bracket_type == "winners" && m$round == next_round
    }, logical(1)))

    if (length(next_idx) == 0) return(bracket)

    # Ensure next round matches are not already started
    for (idx in next_idx) {
        m <- bracket$matches[[idx]]
        if (m$status == "complete" || !is.na(m$winner) ||
            !is.na(m$score1) || !is.na(m$score2)) {
            stop("Cannot reseed: winners round ", next_round, " already started")
        }
    }

    winners <- vapply(winners_round_matches, `[[`, character(1), "winner")
    winners <- winners[!is.na(winners)]

    seeds <- bracket$participants_df$seed_rank
    names(seeds) <- bracket$participants_df$name
    winners <- winners[order(seeds[winners], winners)]

    # Reset and assign next round participants by seed order
    for (idx in next_idx) {
        bracket$matches[[idx]]$participant1 <- NA_character_
        bracket$matches[[idx]]$participant2 <- NA_character_
        bracket$matches[[idx]]$score1 <- NA_real_
        bracket$matches[[idx]]$score2 <- NA_real_
        bracket$matches[[idx]]$winner <- NA_character_
        bracket$matches[[idx]]$loser <- NA_character_
        bracket$matches[[idx]]$status <- "pending"
    }

    w_idx <- 1L
    for (idx in next_idx) {
        if (w_idx <= length(winners)) {
            bracket$matches[[idx]]$participant1 <- winners[w_idx]
            w_idx <- w_idx + 1L
        }
        if (w_idx <= length(winners)) {
            bracket$matches[[idx]]$participant2 <- winners[w_idx]
            w_idx <- w_idx + 1L
        }
    }

    bracket$current_winners_round <- next_round
    bracket
}
