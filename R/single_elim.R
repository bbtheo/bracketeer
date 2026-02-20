#' Create a single elimination bracket
#'
#' Single elimination (knockout) tournament where losing a match eliminates
#' the participant from the tournament.
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' column.
#' @param ... Additional arguments passed to bracket constructors or
#'   tournament stage-verb dispatch methods.
#'
#' @return A single_elim_bracket object
#' @examples
#' # Simple knockout bracket
#' trn <- tournament(paste("Team", LETTERS[1:8])) |>
#'   single_elim("bracket")
#'
#' # Chain after group stage
#' trn <- tournament(c("A", "B", "C", "D")) |>
#'   round_robin("groups") |>
#'   single_elim("finals", take = top_n(2))
#' @export
single_elim <- function(participants, ...) {
    if (inherits(participants, "bracketeer_spec")) {
        return(single_elim.bracketeer_spec(participants, ...))
    }
    stop(
        "`single_elim()` expected a `bracketeer_spec` input (from `spec()` or `tournament()`) ",
        "but got class `", paste(class(participants), collapse = "/"), "`. ",
        "For standalone bracket construction, use `new_single_elim_bracket()`."
    )
}

#' @keywords internal
single_elim.default <- function(participants, seed = TRUE, third_place = FALSE,
                                best_of = NULL, reseed = FALSE) {
    info <- normalize_participants(participants)
    seeded <- apply_seed_method(info$data, seed)
    participant_names <- seeded$name
    best_of <- validate_best_of(best_of)

    n <- length(participant_names)
    if (n < 2) {
        stop("Need at least 2 participants")
    }

    # Seed participants into bracket positions
    seeding <- seed_participants(participant_names, seed = seed)
    bracket_size <- seeding$bracket_size
    n_rounds <- calc_rounds_single_elim(bracket_size)

    # Generate matches
    matches <- list()
    match_id <- 1L

    # Calculate matches per round
    matches_per_round <- bracket_size / 2

    for (round in seq_len(n_rounds)) {
        n_matches_this_round <- as.integer(matches_per_round / (2^(round - 1)))

        for (pos in seq_len(n_matches_this_round)) {
            # Determine next match
            if (round < n_rounds) {
                # Next-round IDs start after all matches in the current round.
                next_match <- as.integer(
                    bracket_size * (1 - 1 / (2^round)) + ceiling(pos / 2)
                )
                next_slot <- if (pos %% 2 == 1) 1L else 2L
            } else {
                next_match <- NA_integer_
                next_slot <- NA_integer_
            }

            # For round 1, assign participants from seeding
            if (round == 1) {
                idx1 <- (pos - 1) * 2 + 1
                idx2 <- (pos - 1) * 2 + 2
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
                next_slot = next_slot
            )
            match_id <- match_id + 1L
        }
    }

    # Add third place match if requested
    third_place_match_id <- NA_integer_
    if (third_place && n_rounds >= 2) {
        # Find semifinal matches (last 2 matches before final)
        final_id <- length(matches)
        semi1_id <- final_id - 2
        semi2_id <- final_id - 1

        # Update semifinals to send losers to third place match
        matches[[semi1_id]]$loser_next_match <- match_id
        matches[[semi1_id]]$loser_next_slot <- 1L
        matches[[semi2_id]]$loser_next_match <- match_id
        matches[[semi2_id]]$loser_next_slot <- 2L

        # Create third place match
        matches[[match_id]] <- create_match(
            id = match_id,
            round = n_rounds,  # Same round as final
            position = 2,
            participant1 = NA_character_,
            participant2 = NA_character_,
            next_match = NA_integer_,
            next_slot = NA_integer_
        )
        third_place_match_id <- match_id
    }

    # Create bracket object
    bracket <- list(
        type = "single_elim",
        participants = participant_names,
        participants_df = seeded,
        matches = matches,
        rounds = n_rounds,
        bracket_size = bracket_size,
        third_place = third_place,
        third_place_match_id = third_place_match_id,
        best_of = best_of,
        reseed = reseed,
        current_round = 1L
    )
    class(bracket) <- c("single_elim_bracket", "bracket")

    # Process round 1 byes (do not advance if reseeding)
    bracket <- process_byes(bracket, round = 1L, bracket_type = "main")

    bracket
}

#' Internal single-elimination bracket constructor
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column and optional `seed` column.
#' @param seed Seeding policy forwarded to internal seeding helpers.
#' @param third_place Whether to include a third-place match.
#' @param best_of Optional odd-integer series length specification.
#' @param reseed Whether to reseed participants between rounds.
#' @return A single_elim_bracket object.
#' @keywords internal
new_single_elim_bracket <- function(participants, seed = TRUE, third_place = FALSE,
                                    best_of = NULL, reseed = FALSE) {
    single_elim.default(
        participants = participants,
        seed = seed,
        third_place = third_place,
        best_of = best_of,
        reseed = reseed
    )
}

#' @keywords internal
single_elim.bracketeer_spec <- function(participants, id, seed = TRUE, third_place = FALSE,
                                        best_of = NULL, reseed = FALSE,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    add_stage_verb(
        participants,
        id = id,
        stage = single_elim_stage(
            seed = seed,
            third_place = third_place,
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
single_elim.tournament_spec <- function(participants, id, seed = TRUE, third_place = FALSE,
                                        best_of = NULL, reseed = FALSE,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    single_elim.bracketeer_spec(
        participants = participants,
        id = id,
        seed = seed,
        third_place = third_place,
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
single_elim.tournament <- function(participants, id, seed = TRUE, third_place = FALSE,
                                   best_of = NULL, reseed = FALSE,
                                   from = NULL, take = NULL,
                                   seeding = "by_source_rank",
                                   consume = TRUE, allow_overlap = FALSE,
                                   priority = 1L, transition_id = NULL) {
    single_elim.bracketeer_spec(
        participants = participants,
        id = id,
        seed = seed,
        third_place = third_place,
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

#' @rdname get_standings
#' @export
get_standings.single_elim_bracket <- function(bracket) {
    participants <- bracket$participants
    n <- length(participants)

    # Calculate elimination round for each participant
    elim_round <- rep(NA_integer_, n)
    names(elim_round) <- participants

    for (match in bracket$matches) {
        if (match$status == "complete" && !is.na(match$loser)) {
            elim_round[match$loser] <- match$round
        }
    }

    # Winner gets max round + 1
    winner <- get_winner(bracket)
    if (!is.na(winner)) {
        elim_round[winner] <- bracket$rounds + 1L
    }

    # Create standings
    df <- data.frame(
        participant = participants,
        eliminated_round = elim_round,
        stringsAsFactors = FALSE
    )

    # Sort: winner first, then by elimination round (later = better)
    df <- df[order(-df$eliminated_round, na.last = TRUE), ]
    df$rank <- seq_len(nrow(df))
    df <- df[, c("rank", "participant", "eliminated_round")]
    rownames(df) <- NULL

    df
}

#' @rdname get_winner
#' @export
get_winner.single_elim_bracket <- function(bracket) {
    final_match <- NULL
    for (m in bracket$matches) {
        if (m$round == bracket$rounds &&
            m$position == 1L &&
            m$bracket_type == "main") {
            final_match <- m
            break
        }
    }
    if (is.null(final_match)) return(NA_character_)
    if (final_match$status == "complete") {
        return(final_match$winner)
    }
    NA_character_
}

#' @rdname advance_winner
#' @export
advance_winner.single_elim_bracket <- function(bracket, match_id) {
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

    # Advance loser to third place match if applicable
    if (bracket$third_place && !is.na(match$loser)) {
        if (!is.null(match$loser_next_match) && !is.na(match$loser_next_match)) {
            loser_next <- match$loser_next_match
            loser_slot <- match$loser_next_slot
            if (loser_slot == 1L) {
                bracket$matches[[loser_next]]$participant1 <- match$loser
            } else {
                bracket$matches[[loser_next]]$participant2 <- match$loser
            }
        }
    }

    bracket
}

#' @rdname advance
#' @export
advance.single_elim_bracket <- function(x, stage = NULL, ...) {
    bracket <- x
    if (!isTRUE(bracket$reseed)) {
        if (!is.null(bracket$current_round) && bracket$current_round < bracket$rounds) {
            bracket$current_round <- bracket$current_round + 1L
        }
        return(bracket)
    }

    current_round <- bracket$current_round
    if (current_round >= bracket$rounds) return(bracket)

    round_matches <- Filter(function(m) m$round == current_round, bracket$matches)
    if (length(round_matches) == 0) {
        stop("No matches found for round ", current_round)
    }
    if (!all(vapply(round_matches, function(m) m$status == "complete", logical(1)))) {
        stop("Current round is not complete")
    }

    winners <- vapply(round_matches, `[[`, character(1), "winner")
    winners <- winners[!is.na(winners)]

    seeds <- bracket$participants_df$seed_rank
    names(seeds) <- bracket$participants_df$name
    winners <- winners[order(seeds[winners], winners)]

    next_round <- current_round + 1L
    next_idx <- which(vapply(bracket$matches, function(m) m$round == next_round, logical(1)))

    # Reset next round matches
    for (idx in next_idx) {
        bracket$matches[[idx]]$participant1 <- NA_character_
        bracket$matches[[idx]]$participant2 <- NA_character_
        bracket$matches[[idx]]$score1 <- NA_real_
        bracket$matches[[idx]]$score2 <- NA_real_
        bracket$matches[[idx]]$winner <- NA_character_
        bracket$matches[[idx]]$loser <- NA_character_
        bracket$matches[[idx]]$status <- "pending"
    }

    # Assign winners in order to next round matches
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

    bracket$current_round <- next_round
    bracket
}
