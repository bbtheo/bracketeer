#' Create a round robin tournament
#'
#' Round robin tournament where each participant plays every other participant.
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' column.
#' @param ... Additional arguments passed to bracket constructors or
#'   tournament stage-verb dispatch methods.
#'
#' @return A round_robin_bracket object
#' @export
round_robin <- function(participants, ...) {
    if (inherits(participants, "bracketeer_spec")) {
        return(round_robin.bracketeer_spec(participants, ...))
    }
    stop(
        "`round_robin()` expected a `bracketeer_spec` input (from `spec()` or `tournament()`) ",
        "but got class `", paste(class(participants), collapse = "/"), "`. ",
        "For standalone bracket construction, use `new_round_robin_bracket()`."
    )
}

#' @keywords internal
round_robin.default <- function(participants, home_away = FALSE, n_rounds = NULL,
                                best_of = NULL, tiebreakers = NULL,
                                groups = NULL) {
    info <- normalize_participants(participants)
    participants_df <- apply_seed_method(info$data, seed_method = "none")
    best_of <- validate_best_of(best_of)
    tiebreakers <- normalize_tiebreakers(tiebreakers, format = "round_robin")
    n_rounds_specified <- !is.null(n_rounds)
    if (is.null(n_rounds)) {
        n_rounds <- if (isTRUE(home_away)) 2L else 1L
    } else {
        if (length(n_rounds) != 1L || !is.numeric(n_rounds) || !is.finite(n_rounds) ||
            n_rounds < 1 || n_rounds != as.integer(n_rounds)) {
            stop("n_rounds must be a positive integer")
        }
        n_rounds <- as.integer(n_rounds)
    }
    if (n_rounds_specified && isTRUE(home_away) && n_rounds %% 2L == 1L) {
        warning("n_rounds is odd; pairings may not split evenly", call. = FALSE)
    }

    if (is.null(groups)) {
        names <- participants_df$name
        n <- length(names)
        if (n < 2) stop("Need at least 2 participants")

        matches <- build_round_robin_matches(
            participants = names,
            n_rounds = n_rounds,
            home_away = home_away,
            start_match_id = 1L,
            group_id = NULL
        )
        rounds <- attr(matches, "rounds")
    } else {
        if (!is.numeric(groups) || length(groups) != 1L || is.na(groups) ||
            groups < 1 || groups %% 1 != 0) {
            stop("groups must be a positive integer")
        }
        groups <- as.integer(groups)
        participants_df <- assign_groups(participants_df, groups = groups)

        group_ids <- unique(as.character(participants_df$group))
        matches <- list()
        next_match_id <- 1L
        max_rounds <- 0L

        for (group_id in group_ids) {
            group_participants <- participants_df$name[participants_df$group == group_id]
            if (length(group_participants) < 2L) {
                stop(
                    "Group `", group_id,
                    "` has fewer than 2 participants. Reduce `groups` or add participants."
                )
            }

            group_matches <- build_round_robin_matches(
                participants = group_participants,
                n_rounds = n_rounds,
                home_away = home_away,
                start_match_id = next_match_id,
                group_id = group_id
            )
            max_rounds <- max(max_rounds, attr(group_matches, "rounds"))
            next_match_id <- next_match_id + length(group_matches)
            matches <- c(matches, group_matches)
        }

        rounds <- max_rounds
    }

    bracket <- list(
        type = "round_robin",
        participants = participants_df$name,
        participants_df = participants_df,
        matches = matches,
        rounds = rounds,
        home_away = home_away,
        n_rounds = n_rounds,
        best_of = best_of,
        tiebreakers = tiebreakers,
        groups = groups,
        reseed = FALSE,
        current_round = 1L
    )
    class(bracket) <- c("round_robin_bracket", "bracket")

    bracket
}

#' Internal round-robin bracket constructor
#'
#' @inheritParams round_robin.default
#' @return A round_robin_bracket object.
#' @keywords internal
new_round_robin_bracket <- function(participants, home_away = FALSE, n_rounds = NULL,
                                    best_of = NULL, tiebreakers = NULL,
                                    groups = NULL) {
    round_robin.default(
        participants = participants,
        home_away = home_away,
        n_rounds = n_rounds,
        best_of = best_of,
        tiebreakers = tiebreakers,
        groups = groups
    )
}

#' @keywords internal
round_robin.bracketeer_spec <- function(participants, id, home_away = FALSE, n_rounds = NULL,
                                        best_of = NULL, tiebreakers = NULL, groups = NULL,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    add_stage_verb(
        participants,
        id = id,
        stage = round_robin_stage(
            home_away = home_away,
            n_rounds = n_rounds,
            best_of = best_of,
            tiebreakers = tiebreakers,
            groups = groups
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
round_robin.tournament_spec <- function(participants, id, home_away = FALSE, n_rounds = NULL,
                                        best_of = NULL, tiebreakers = NULL, groups = NULL,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    round_robin.bracketeer_spec(
        participants = participants,
        id = id,
        home_away = home_away,
        n_rounds = n_rounds,
        best_of = best_of,
        tiebreakers = tiebreakers,
        groups = groups,
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
round_robin.tournament <- function(participants, id, home_away = FALSE, n_rounds = NULL,
                                   best_of = NULL, tiebreakers = NULL, groups = NULL,
                                   from = NULL, take = NULL,
                                   seeding = "by_source_rank",
                                   consume = TRUE, allow_overlap = FALSE,
                                   priority = 1L, transition_id = NULL) {
    round_robin.bracketeer_spec(
        participants = participants,
        id = id,
        home_away = home_away,
        n_rounds = n_rounds,
        best_of = best_of,
        tiebreakers = tiebreakers,
        groups = groups,
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
set_result.round_robin_bracket <- function(bracket, match_id, score1, score2,
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
        allow_ties = TRUE,
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

#' @rdname get_standings
#' @export
get_standings.round_robin_bracket <- function(bracket) {
    if (!is.null(bracket$participants_df$group)) {
        group_ids <- unique(as.character(bracket$participants_df$group))
        grouped <- lapply(group_ids, function(group_id) {
            participants <- as.character(
                bracket$participants_df$name[bracket$participants_df$group == group_id]
            )
            matches <- Filter(function(m) {
                identical(as.character(m$group), as.character(group_id))
            }, bracket$matches)
            standings <- compute_round_robin_standings(
                participants = participants,
                matches = matches,
                tiebreakers = bracket$tiebreakers
            )
            standings$group <- as.character(group_id)
            standings$group_rank <- as.integer(standings$rank)
            standings
        })

        combined <- do.call(rbind, grouped)
        rownames(combined) <- NULL
        combined$rank <- seq_len(nrow(combined))
        return(combined[, c(
            "rank", "group", "group_rank", "participant", "wins", "draws", "losses",
            "points", "score_diff", "sos", "head_to_head"
        )])
    }

    compute_round_robin_standings(
        participants = bracket$participants,
        matches = bracket$matches,
        tiebreakers = bracket$tiebreakers
    )
}

compute_round_robin_standings <- function(participants, matches, tiebreakers) {
    participants <- as.character(participants)
    n <- length(participants)

    wins <- setNames(rep(0L, n), participants)
    losses <- setNames(rep(0L, n), participants)
    draws <- setNames(rep(0L, n), participants)
    points <- setNames(rep(0, n), participants)
    score_for <- setNames(rep(0, n), participants)
    score_against <- setNames(rep(0, n), participants)

    for (m in matches) {
        if (m$status != "complete") next
        if (is.na(m$participant1) || is.na(m$participant2)) next

        p1 <- m$participant1
        p2 <- m$participant2
        s1 <- m$score1
        s2 <- m$score2

        score_for[p1] <- score_for[p1] + s1
        score_against[p1] <- score_against[p1] + s2
        score_for[p2] <- score_for[p2] + s2
        score_against[p2] <- score_against[p2] + s1

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

    sos <- setNames(rep(0, n), participants)
    for (m in matches) {
        if (m$status != "complete") next
        if (is.na(m$participant1) || is.na(m$participant2)) next
        p1 <- m$participant1
        p2 <- m$participant2
        sos[p1] <- sos[p1] + points[p2]
        sos[p2] <- sos[p2] + points[p1]
    }

    df <- data.frame(
        participant = participants,
        wins = wins[participants],
        draws = draws[participants],
        losses = losses[participants],
        points = points[participants],
        score_for = score_for[participants],
        score_against = score_against[participants],
        score_diff = score_for[participants] - score_against[participants],
        sos = sos[participants],
        stringsAsFactors = FALSE
    )

    df <- apply_tiebreakers(
        df,
        matches = matches,
        tiebreakers = tiebreakers,
        allow_ties = TRUE
    )

    if (!"head_to_head" %in% names(df)) {
        df$head_to_head <- 0
    }

    df <- df[, c("rank", "participant", "wins", "draws", "losses",
        "points", "score_diff", "sos", "head_to_head")]
    df
}

build_round_robin_matches <- function(participants, n_rounds, home_away,
                                      start_match_id = 1L, group_id = NULL) {
    names <- as.character(participants)
    n <- length(names)
    if (n < 2L) {
        stop("Need at least 2 participants")
    }

    if (n %% 2L == 1L) {
        names <- c(names, NA_character_)
    }

    n_total <- length(names)
    cycle_rounds <- n_total - 1L
    matches <- list()
    match_id <- as.integer(start_match_id)

    rotate <- function(x) {
        if (length(x) <= 2) return(x)
        c(x[1], x[length(x)], x[2:(length(x) - 1)])
    }

    for (cycle in seq_len(n_rounds)) {
        teams <- names
        for (r in seq_len(cycle_rounds)) {
            round_id <- (cycle - 1L) * cycle_rounds + r
            for (i in seq_len(n_total / 2)) {
                p1 <- teams[i]
                p2 <- teams[n_total - i + 1L]
                if (!is.na(p1) && !is.na(p2)) {
                    if (home_away && cycle %% 2L == 0L) {
                        tmp <- p1
                        p1 <- p2
                        p2 <- tmp
                    } else if (!home_away && sample.int(2L, size = 1L) == 2L) {
                        tmp <- p1
                        p1 <- p2
                        p2 <- tmp
                    }
                    match <- create_match(
                        id = match_id,
                        round = round_id,
                        position = i,
                        participant1 = p1,
                        participant2 = p2,
                        next_match = NA_integer_,
                        next_slot = NA_integer_,
                        bracket_type = "round_robin"
                    )
                    if (!is.null(group_id)) {
                        match$group <- as.character(group_id)
                    }
                    matches[[length(matches) + 1L]] <- match
                    match_id <- match_id + 1L
                }
            }
            teams <- rotate(teams)
        }
    }

    attr(matches, "rounds") <- cycle_rounds * n_rounds
    matches
}

#' @rdname get_winner
#' @export
get_winner.round_robin_bracket <- function(bracket) {
    if (!is_complete(bracket)) return(NA_character_)
    standings <- get_standings(bracket)
    standings$participant[1]
}
