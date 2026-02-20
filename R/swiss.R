#' Create a Swiss-system tournament
#'
#' Swiss system pairs participants by similar records each round.
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' column.
#' @param ... Additional arguments passed to bracket constructors or
#'   tournament stage-verb dispatch methods.
#'
#' @return A swiss_bracket object
#' @examples
#' # Swiss system followed by top-cut playoffs
#' teams <- paste("Team", LETTERS[1:16])
#' trn <- tournament(teams) |>
#'   swiss("open", rounds = 5) |>
#'   single_elim("playoffs", take = top_n(8))
#' @export
swiss <- function(participants, ...) {
    if (inherits(participants, "bracketeer_spec")) {
        return(swiss.bracketeer_spec(participants, ...))
    }
    stop(
        "`swiss()` expected a `bracketeer_spec` input (from `spec()` or `tournament()`) ",
        "but got class `", paste(class(participants), collapse = "/"), "`. ",
        "For standalone bracket construction, use `new_swiss_bracket()`."
    )
}

#' @keywords internal
swiss.default <- function(participants, rounds = NULL, seed = TRUE,
                          allow_ties = TRUE, bye_points = 1, best_of = NULL,
                          tiebreakers = NULL) {
    info <- normalize_participants(participants)
    seeded <- apply_seed_method(info$data, seed)
    participant_names <- seeded$name
    best_of <- validate_best_of(best_of)
    tiebreakers <- normalize_tiebreakers(tiebreakers, format = "swiss")

    n <- length(participant_names)
    if (n < 2) stop("Need at least 2 participants")

    if (is.null(rounds)) {
        rounds <- ceiling(log2(n))
    }
    if (rounds < 1) stop("Rounds must be >= 1")

    bracket <- list(
        type = "swiss",
        participants = participant_names,
        participants_df = seeded,
        matches = list(),
        rounds = rounds,
        allow_ties = allow_ties,
        bye_points = bye_points,
        best_of = best_of,
        tiebreakers = tiebreakers,
        reseed = FALSE,
        current_round = 1L
    )
    class(bracket) <- c("swiss_bracket", "bracket")

    bracket <- swiss_add_round(bracket, round = 1L)
    bracket
}

#' Internal swiss bracket constructor
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column and optional `seed` column.
#' @param rounds Optional positive integer number of Swiss rounds.
#' @param seed Seeding policy for initial ordering.
#' @param allow_ties Whether drawn results are allowed.
#' @param bye_points Points awarded for a bye.
#' @param best_of Optional odd-integer series length specification.
#' @param tiebreakers Optional ordered tiebreaker vector.
#' @return A swiss_bracket object.
#' @keywords internal
new_swiss_bracket <- function(participants, rounds = NULL, seed = TRUE,
                              allow_ties = TRUE, bye_points = 1, best_of = NULL,
                              tiebreakers = NULL) {
    swiss.default(
        participants = participants,
        rounds = rounds,
        seed = seed,
        allow_ties = allow_ties,
        bye_points = bye_points,
        best_of = best_of,
        tiebreakers = tiebreakers
    )
}

#' @keywords internal
swiss.bracketeer_spec <- function(participants, id, rounds = NULL, seed = TRUE,
                                  allow_ties = TRUE, bye_points = 1, best_of = NULL,
                                  tiebreakers = NULL,
                                  from = NULL, take = NULL,
                                  seeding = "by_source_rank",
                                  consume = TRUE, allow_overlap = FALSE,
                                  priority = 1L, transition_id = NULL) {
    add_stage_verb(
        participants,
        id = id,
        stage = swiss_stage(
            rounds = rounds,
            seed = seed,
            allow_ties = allow_ties,
            bye_points = bye_points,
            best_of = best_of,
            tiebreakers = tiebreakers
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
swiss.tournament_spec <- function(participants, id, rounds = NULL, seed = TRUE,
                                  allow_ties = TRUE, bye_points = 1, best_of = NULL,
                                  tiebreakers = NULL,
                                  from = NULL, take = NULL,
                                  seeding = "by_source_rank",
                                  consume = TRUE, allow_overlap = FALSE,
                                  priority = 1L, transition_id = NULL) {
    swiss.bracketeer_spec(
        participants = participants,
        id = id,
        rounds = rounds,
        seed = seed,
        allow_ties = allow_ties,
        bye_points = bye_points,
        best_of = best_of,
        tiebreakers = tiebreakers,
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
swiss.tournament <- function(participants, id, rounds = NULL, seed = TRUE,
                             allow_ties = TRUE, bye_points = 1, best_of = NULL,
                             tiebreakers = NULL,
                             from = NULL, take = NULL,
                             seeding = "by_source_rank",
                             consume = TRUE, allow_overlap = FALSE,
                             priority = 1L, transition_id = NULL) {
    swiss.bracketeer_spec(
        participants = participants,
        id = id,
        rounds = rounds,
        seed = seed,
        allow_ties = allow_ties,
        bye_points = bye_points,
        best_of = best_of,
        tiebreakers = tiebreakers,
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @rdname set_result
#' @export
set_result.swiss_bracket <- function(bracket, match_id, score1, score2,
                                     stage_id = NULL, overwrite = FALSE,
                                     auto_advance = FALSE) {
    if (match_id < 1 || match_id > length(bracket$matches)) {
        stop("Invalid match_id: ", match_id)
    }

    match <- bracket$matches[[match_id]]

    if (match$status == "complete") {
        stop("Match ", match_id, " is already complete")
    }
    if (is.na(match$participant1) || is.na(match$participant2)) {
        stop("Match ", match_id, " does not have both participants yet")
    }
    result <- normalize_match_scores(
        score1 = score1,
        score2 = score2,
        best_of = bracket$best_of,
        allow_ties = bracket$allow_ties,
        participant1 = match$participant1,
        participant2 = match$participant2
    )

    bracket$matches[[match_id]]$score1 <- result$score1
    bracket$matches[[match_id]]$score2 <- result$score2
    bracket$matches[[match_id]]$games <- result$games
    bracket$matches[[match_id]]$winner <- result$winner
    bracket$matches[[match_id]]$loser <- result$loser
    bracket$matches[[match_id]]$status <- "complete"

    bracket
}

#' @rdname advance
#' @export
advance.swiss_bracket <- function(x, stage = NULL, ...) {
    bracket <- x
    current_round <- bracket$current_round
    if (current_round >= bracket$rounds) return(bracket)

    round_matches <- Filter(function(m) m$round == current_round, bracket$matches)
    if (length(round_matches) == 0) {
        stop("No matches found for round ", current_round)
    }
    if (!all(vapply(round_matches, function(m) m$status == "complete", logical(1)))) {
        stop("Current round is not complete")
    }

    bracket <- swiss_add_round(bracket, round = current_round + 1L)
    bracket$current_round <- current_round + 1L
    bracket
}

#' @rdname get_standings
#' @export
get_standings.swiss_bracket <- function(bracket) {
    participants <- bracket$participants
    n <- length(participants)

    wins <- setNames(rep(0L, n), participants)
    losses <- setNames(rep(0L, n), participants)
    draws <- setNames(rep(0L, n), participants)
    points <- setNames(rep(0, n), participants)
    score_for <- setNames(rep(0, n), participants)
    score_against <- setNames(rep(0, n), participants)

    for (m in bracket$matches) {
        if (m$status != "complete") next
        if (is.na(m$participant1)) next

        p1 <- m$participant1
        p2 <- m$participant2
        s1 <- m$score1
        s2 <- m$score2

        score_for[p1] <- score_for[p1] + s1
        score_against[p1] <- score_against[p1] + s2

        if (!is.na(p2)) {
            score_for[p2] <- score_for[p2] + s2
            score_against[p2] <- score_against[p2] + s1
        }

        if (is.na(p2)) {
            wins[p1] <- wins[p1] + 1L
            points[p1] <- points[p1] + bracket$bye_points
            next
        }

        if (s1 > s2) {
            wins[p1] <- wins[p1] + 1L
            losses[p2] <- losses[p2] + 1L
            points[p1] <- points[p1] + 1
        } else if (s2 > s1) {
            wins[p2] <- wins[p2] + 1L
            losses[p1] <- losses[p1] + 1L
            points[p2] <- points[p2] + 1
        } else {
            draws[p1] <- draws[p1] + 1L
            draws[p2] <- draws[p2] + 1L
            points[p1] <- points[p1] + 0.5
            points[p2] <- points[p2] + 0.5
        }
    }

    # Buchholz: sum of opponents' points
    buchholz <- setNames(rep(0, n), participants)
    for (m in bracket$matches) {
        if (m$status != "complete") next
        if (is.na(m$participant1) || is.na(m$participant2)) next
        p1 <- m$participant1
        p2 <- m$participant2
        buchholz[p1] <- buchholz[p1] + points[p2]
        buchholz[p2] <- buchholz[p2] + points[p1]
    }

    df <- data.frame(
        participant = participants,
        wins = wins[participants],
        draws = draws[participants],
        losses = losses[participants],
        points = points[participants],
        buchholz = buchholz[participants],
        sos = buchholz[participants],
        score_for = score_for[participants],
        score_against = score_against[participants],
        score_diff = score_for[participants] - score_against[participants],
        stringsAsFactors = FALSE
    )

    df <- apply_tiebreakers(
        df,
        matches = bracket$matches,
        tiebreakers = bracket$tiebreakers,
        allow_ties = bracket$allow_ties
    )

    if (!"head_to_head" %in% names(df)) {
        df$head_to_head <- 0
    }

    df <- df[, c("rank", "participant", "wins", "draws", "losses",
        "points", "buchholz", "sos", "score_diff", "head_to_head")]
    df
}

#' @rdname get_winner
#' @export
get_winner.swiss_bracket <- function(bracket) {
    if (bracket$current_round < bracket$rounds) return(NA_character_)
    if (!is_complete(bracket)) return(NA_character_)
    standings <- get_standings(bracket)
    standings$participant[1]
}

#' @rdname is_complete
#' @export
is_complete.swiss_bracket <- function(bracket) {
    if (bracket$current_round < bracket$rounds) return(FALSE)
    all(vapply(bracket$matches, function(m) m$status == "complete", logical(1)))
}

# Internal helpers ----

swiss_add_round <- function(bracket, round) {
    standings <- NULL
    if (length(bracket$matches) > 0) {
        standings <- get_standings(bracket)
    }

    pairings <- swiss_pairings(
        participants = bracket$participants,
        standings = standings,
        matches = bracket$matches,
        participants_df = bracket$participants_df
    )

    match_id <- length(bracket$matches) + 1L
    position <- 1L
    for (pair in pairings$matches) {
        bracket$matches[[match_id]] <- create_match(
            id = match_id,
            round = round,
            position = position,
            participant1 = pair[1],
            participant2 = pair[2],
            next_match = NA_integer_,
            next_slot = NA_integer_,
            bracket_type = "swiss"
        )
        match_id <- match_id + 1L
        position <- position + 1L
    }

    if (!is.na(pairings$bye)) {
        bracket$matches[[match_id]] <- create_match(
            id = match_id,
            round = round,
            position = position,
            participant1 = pairings$bye,
            participant2 = NA_character_,
            next_match = NA_integer_,
            next_slot = NA_integer_,
            bracket_type = "swiss"
        )
        bracket$matches[[match_id]]$score1 <- bracket$bye_points
        bracket$matches[[match_id]]$score2 <- 0
        bracket$matches[[match_id]]$winner <- pairings$bye
        bracket$matches[[match_id]]$status <- "complete"
    }

    bracket
}

swiss_pairings <- function(participants, standings, matches, participants_df) {
    n <- length(participants)

    # Build played matrix
    played <- matrix(FALSE, nrow = n, ncol = n,
        dimnames = list(participants, participants))
    if (length(matches) > 0) {
        for (m in matches) {
            if (m$status != "complete") next
            if (is.na(m$participant1) || is.na(m$participant2)) next
            played[m$participant1, m$participant2] <- TRUE
            played[m$participant2, m$participant1] <- TRUE
        }
    }

    # Bye counts
    bye_counts <- setNames(rep(0L, n), participants)
    if (length(matches) > 0) {
        for (m in matches) {
            if (m$status != "complete") next
            if (!is.na(m$participant1) && is.na(m$participant2)) {
                bye_counts[m$participant1] <- bye_counts[m$participant1] + 1L
            }
        }
    }

    if (is.null(standings)) {
        order_names <- participants_df$name
    } else {
        order_names <- standings$participant
    }

    # Select bye recipient if needed
    bye <- NA_character_
    if (n %% 2 == 1) {
        candidates <- rev(order_names)
        min_byes <- min(bye_counts[candidates])
        candidates <- candidates[bye_counts[candidates] == min_byes]
        bye <- candidates[1]
        order_names <- order_names[order_names != bye]
    }

    available <- order_names
    pairs <- list()
    while (length(available) > 1) {
        p1 <- available[1]
        opp_idx <- which(!played[p1, available[-1]])
        if (length(opp_idx) == 0) {
            p2 <- available[2]
        } else {
            p2 <- available[1 + opp_idx[1]]
        }
        pairs[[length(pairs) + 1L]] <- c(p1, p2)
        available <- available[!(available %in% c(p1, p2))]
    }

    list(matches = pairs, bye = bye)
}
