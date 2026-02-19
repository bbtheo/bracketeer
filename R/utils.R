#' Utility functions for bracketeer
#'
#' @name utils
#' @keywords internal
NULL

#' Calculate the next power of 2
#'
#' @param n A positive integer
#' @return The smallest power of 2 >= n
#' @keywords internal
next_power_of_2 <- function(n) {
    if (n <= 0) stop("n must be positive")
    2^ceiling(log2(n))
}

#' Check if n is a power of 2
#'
#' @param n A positive integer
#' @return Logical
#' @keywords internal
is_power_of_2 <- function(n) {
    n > 0 && bitwAnd(n, n - 1L) == 0L
}

#' Calculate number of byes needed
#'
#' @param n Number of participants
#' @return Number of byes needed to reach next power of 2
#' @keywords internal
calc_byes <- function(n) {
    next_power_of_2(n) - n
}

#' Calculate number of rounds for single elimination
#'
#' @param n Number of participants
#' @return Number of rounds
#' @keywords internal
calc_rounds_single_elim <- function(n) {
    ceiling(log2(n))
}

#' Generate standard seeding order for bracket
#'
#' Creates seeding positions so that seed 1 vs seed N is in the final
#' if both win all matches. Uses standard bracket seeding algorithm.
#'
#' @param n Number of slots (must be power of 2)
#' @return Vector of seed positions
#' @keywords internal
generate_seed_order <- function(n) {
    if (!is_power_of_2(n)) {
        stop("n must be a power of 2")
    }
    if (n == 1) return(1L)
    if (n == 2) return(c(1L, 2L))

    # Recursive bracket seeding
    # For n slots, seeds are arranged so 1 plays n, 2 plays n-1, etc.
    # in the first round (if all are present)
    half <- n / 2
    upper <- generate_seed_order(half)
    lower <- n + 1L - upper
    # Interleave: take from upper and lower alternately
    result <- integer(n)
    result[seq(1, n, by = 2)] <- upper
    result[seq(2, n, by = 2)] <- lower
    result
}

#' Normalize participants input
#'
#' @param participants Character vector or data.frame with a 'name' column.
#'   Optional 'seed' and 'rating' columns are preserved.
#' @return List with `names` and `data` (data.frame).
#' @keywords internal
normalize_participants <- function(participants) {
    if (is.data.frame(participants)) {
        if (!"name" %in% names(participants)) {
            stop("data.frame must have a 'name' column")
        }
        df <- participants
        df$name <- as.character(df$name)
    } else {
        df <- data.frame(
            name = as.character(participants),
            stringsAsFactors = FALSE
        )
    }

    if (any(is.na(df$name)) || any(df$name == "")) {
        stop("Participant names cannot be NA or empty")
    }
    if (anyDuplicated(df$name)) {
        stop("Participant names must be unique")
    }

    if (!"seed" %in% names(df)) {
        df$seed <- seq_len(nrow(df))
    }
    if (!is.numeric(df$seed)) {
        df$seed <- as.numeric(df$seed)
    }

    list(
        names = df$name,
        data = df
    )
}

#' Normalize seed method
#'
#' @param seed Logical or character
#' @return Character seed method
#' @keywords internal
normalize_seed_method <- function(seed) {
    if (isTRUE(seed)) return("standard")
    if (isFALSE(seed)) return("none")
    match.arg(seed, c("standard", "random", "none", "snake"))
}

#' Apply seed ordering to participants
#'
#' @param participants_df data.frame from normalize_participants()
#' @param seed_method Character seed method
#' @return data.frame reordered with a `seed_rank` column
#' @keywords internal
apply_seed_method <- function(participants_df, seed_method = "standard") {
    method <- normalize_seed_method(seed_method)
    df <- participants_df

    if (method == "random") {
        df <- df[sample.int(nrow(df)), , drop = FALSE]
    } else if (method != "none") {
        df <- df[order(df$seed, df$name), , drop = FALSE]
    }

    if (method == "snake") {
        df <- df[snake_order(nrow(df)), , drop = FALSE]
    }

    df$seed_rank <- seq_len(nrow(df))
    rownames(df) <- NULL
    df
}

#' Assign participants to bracket positions with seeding
#'
#' @param participants Character vector of participant names
#' @param seed Logical, whether to apply seeding
#' @return List with slot assignments and bye information
#' @keywords internal
seed_participants <- function(participants, seed = TRUE) {
    method <- normalize_seed_method(seed)
    if (method %in% c("random", "snake")) {
        method <- "standard"
    }
    n <- length(participants)
    bracket_size <- next_power_of_2(n)
    n_byes <- bracket_size - n

    if (method == "none") {
        seed_order <- seq_len(bracket_size)
    } else {
        seed_order <- generate_seed_order(bracket_size)
    }

    # Create slots - participants fill first n positions, rest are byes
    slots <- rep(NA_character_, bracket_size)
    slots[seed_order <= n] <- participants[seed_order[seed_order <= n]]

    list(
        slots = slots,
        bracket_size = bracket_size,
        n_byes = n_byes,
        seed_order = seed_order
    )
}

#' Create a match object
#'
#' @param id Match ID
#' @param round Round number
#' @param position Position within round
#' @param participant1 First participant (or NA for TBD)
#' @param participant2 Second participant (or NA for TBD)
#' @param next_match ID of match winner advances to (or NA for final)
#' @param next_slot Which slot (1 or 2) in next match
#' @param bracket_type For double elim: "winners", "losers", or "grand_final"
#' @return A bracket_match object
#' @keywords internal
create_match <- function(id, round, position, participant1 = NA_character_,
                         participant2 = NA_character_, next_match = NA_integer_,
                         next_slot = NA_integer_, bracket_type = "main",
                         loser_next_match = NA_integer_,
                         loser_next_slot = NA_integer_) {
    match <- list(
        id = as.integer(id),
        round = as.integer(round),
        position = as.integer(position),
        participant1 = participant1,
        participant2 = participant2,
        score1 = NA_real_,
        score2 = NA_real_,
        games = NULL,
        winner = NA_character_,
        loser = NA_character_,
        next_match = as.integer(next_match),
        next_slot = as.integer(next_slot),
        loser_next_match = as.integer(loser_next_match),
        loser_next_slot = as.integer(loser_next_slot),
        bracket_type = bracket_type,
        status = "pending"
    )
    class(match) <- "bracket_match"
    match
}

#' Check if a match is a bye (one participant is NA)
#'
#' @param match A bracket_match object
#' @return Logical
#' @keywords internal
is_bye_match <- function(match) {
    xor(is.na(match$participant1), is.na(match$participant2))
}

#' Process bye matches automatically (round 1 only)
#'
#' @param bracket A bracket object
#' @param round Round number to process byes for
#' @param bracket_type Optional bracket type filter
#' @return Updated bracket object with byes resolved
#' @keywords internal
process_byes <- function(bracket, round = 1L, bracket_type = NULL) {
    changed <- TRUE
    while (changed) {
        changed <- FALSE
        for (i in seq_along(bracket$matches)) {
            match <- bracket$matches[[i]]
            if (match$round != round) next
            if (!is.null(bracket_type) && match$bracket_type != bracket_type) next
            if (!is_bye_match(match) || match$status != "pending") next

            # Determine winner (the non-NA participant)
            if (is.na(match$participant1) && !is.na(match$participant2)) {
                bracket$matches[[i]]$winner <- match$participant2
                bracket$matches[[i]]$status <- "complete"
            } else if (!is.na(match$participant1) && is.na(match$participant2)) {
                bracket$matches[[i]]$winner <- match$participant1
                bracket$matches[[i]]$status <- "complete"
            } else {
                next
            }

            # Advance winner only when reseeding is disabled
            if (!isTRUE(bracket$reseed) && !is.na(bracket$matches[[i]]$winner)) {
                bracket <- advance_winner(bracket, match$id)
            }
            changed <- TRUE
        }
    }

    bracket
}

#' Generate a simple snake order
#'
#' @param n Number of items
#' @param block_size Block size to reverse in alternating fashion
#' @return Integer vector of indices
#' @keywords internal
snake_order <- function(n, block_size = 2L) {
    if (n <= 1) return(seq_len(n))
    idx <- seq_len(n)
    blocks <- split(idx, ceiling(idx / block_size))
    for (i in seq_along(blocks)) {
        if (i %% 2 == 0) {
            blocks[[i]] <- rev(blocks[[i]])
        }
    }
    unlist(blocks, use.names = FALSE)
}

#' Assign participants to groups using snake distribution
#'
#' @param participants_df data.frame with participants
#' @param groups Number of groups
#' @return data.frame with added `group` column
#' @keywords internal
assign_groups <- function(participants_df, groups) {
    if (length(groups) != 1L || groups < 1) {
        stop("groups must be a positive integer")
    }
    n <- nrow(participants_df)
    if (groups > n) {
        stop("groups cannot exceed number of participants")
    }

    labels <- if (groups <= 26) LETTERS[seq_len(groups)] else as.character(seq_len(groups))
    group <- character(n)
    for (i in seq_len(n)) {
        block <- ceiling(i / groups)
        pos <- i - (block - 1L) * groups
        g <- if (block %% 2 == 1L) pos else (groups - pos + 1L)
        group[i] <- labels[g]
    }

    participants_df$group <- group
    participants_df
}

#' Normalize match scores (supports series)
#'
#' @param score1 Numeric score or vector of game scores for participant1
#' @param score2 Numeric score or vector of game scores for participant2
#' @param best_of Optional best-of value
#' @param allow_ties Logical, whether ties are allowed
#' @return List with score1, score2, winner, loser, games
#' @keywords internal
normalize_match_scores <- function(score1, score2, best_of = NULL, allow_ties = FALSE,
                                   participant1 = NA_character_,
                                   participant2 = NA_character_) {
    if (length(score1) != length(score2)) {
        stop("score1 and score2 must have the same length")
    }
    if (!is.numeric(score1) || !is.numeric(score2) ||
        any(!is.finite(score1)) || any(!is.finite(score2))) {
        stop("Scores must be finite numeric values")
    }

    games <- NULL
    if (length(score1) > 1L) {
        games <- list(score1 = as.numeric(score1), score2 = as.numeric(score2))
        if (!is.null(best_of) && !is.na(best_of)) {
            wins1 <- sum(score1 > score2)
            wins2 <- sum(score2 > score1)
            ties <- length(score1) - wins1 - wins2
            if (!allow_ties && ties > 0) {
                stop("Tied games are not allowed in this format")
            }
            if (length(score1) > best_of) {
                stop("Number of games cannot exceed best_of")
            }
            wins_needed <- floor(best_of / 2) + 1
            if (!allow_ties && max(wins1, wins2) != wins_needed) {
                stop("Best-of-", best_of, " requires a winner with ", wins_needed, " wins")
            }
            score1 <- wins1
            score2 <- wins2
        } else {
            score1 <- sum(score1)
            score2 <- sum(score2)
        }
    }

    if (!is.null(best_of) && !is.na(best_of) && length(score1) == 1L) {
        wins_needed <- floor(best_of / 2) + 1
        if (!allow_ties && score1 == score2) {
            stop("Scores cannot be tied in this format")
        }
        if (!allow_ties) {
            if (max(score1, score2) != wins_needed) {
                stop("Best-of-", best_of, " requires a winner with ", wins_needed, " wins")
            }
            if (min(score1, score2) >= wins_needed) {
                stop("Best-of-", best_of, " scores cannot both reach ", wins_needed)
            }
        } else {
            if (max(score1, score2) > wins_needed) {
                stop("Best-of-", best_of, " scores cannot exceed ", wins_needed)
            }
        }
    }

    winner <- NA_character_
    loser <- NA_character_
    if (score1 > score2) {
        winner <- participant1
        loser <- participant2
    } else if (score2 > score1) {
        winner <- participant2
        loser <- participant1
    } else if (!allow_ties) {
        stop("Scores cannot be tied in this format")
    }

    list(
        score1 = as.numeric(score1),
        score2 = as.numeric(score2),
        winner = winner,
        loser = loser,
        games = games
    )
}

#' Validate best-of parameter
#'
#' @param best_of Integer or NULL
#' @return Normalized integer or NULL
#' @keywords internal
validate_best_of <- function(best_of) {
    if (is.null(best_of)) return(NULL)
    if (length(best_of) != 1L || !is.numeric(best_of) || !is.finite(best_of)) {
        stop("best_of must be a finite numeric value")
    }
    best_of <- as.integer(best_of)
    if (best_of < 1L || best_of %% 2L == 0L) {
        stop("best_of must be an odd integer >= 1")
    }
    best_of
}

#' Build an ordered tiebreaker chain
#'
#' @param ... Tiebreaker names. Accepts one or more strings or character
#'   vectors.
#'
#' @return Character vector of unique tiebreakers in input order.
#' @keywords internal
tiebreaker_chain <- function(...) {
    values <- unlist(list(...), use.names = FALSE)
    if (length(values) == 0L) {
        stop("`tiebreaker_chain()` requires at least one tiebreaker")
    }

    values <- as.character(values)
    if (any(is.na(values)) || any(values == "")) {
        stop("`tiebreaker_chain()` values must be non-empty strings")
    }

    unique(values)
}

#' Default tiebreakers per format
#'
#' @param format Tournament format
#' @return Character vector of tiebreakers
#' @keywords internal
default_tiebreakers <- function(format) {
    switch(format,
        round_robin = c("points", "head_to_head", "score_diff", "sos", "wins", "alphabetical"),
        swiss = c("points", "buchholz", "sos", "head_to_head", "score_diff", "wins", "alphabetical"),
        c("points", "alphabetical")
    )
}

#' Normalize tiebreaker configuration
#'
#' @param tiebreakers Character vector or NULL
#' @param format Tournament format
#' @return Normalized character vector
#' @keywords internal
normalize_tiebreakers <- function(tiebreakers, format) {
    allowed <- c(
        "points", "wins", "draws", "losses", "score_for", "score_against",
        "score_diff", "sos", "buchholz", "head_to_head", "alphabetical"
    )
    if (is.null(tiebreakers)) {
        tiebreakers <- default_tiebreakers(format)
    }
    tiebreakers <- unique(as.character(tiebreakers))
    unknown <- setdiff(tiebreakers, allowed)
    if (length(unknown) > 0) {
        stop("Unknown tiebreakers: ", paste(unknown, collapse = ", "))
    }
    if (!"alphabetical" %in% tiebreakers) {
        tiebreakers <- c(tiebreakers, "alphabetical")
    }
    tiebreakers
}

#' Determine tiebreaker direction
#'
#' @param tiebreaker Name
#' @return "asc" or "desc"
#' @keywords internal
tiebreaker_direction <- function(tiebreaker) {
    if (tiebreaker %in% c("losses", "score_against")) return("asc")
    if (tiebreaker %in% c("alphabetical")) return("asc")
    "desc"
}

#' Compute head-to-head points within a subset
#'
#' @param matches List of matches
#' @param participants Character vector of participants
#' @param allow_ties Logical
#' @return Named numeric vector of points
#' @keywords internal
head_to_head_points <- function(matches, participants, allow_ties = TRUE) {
    points <- setNames(rep(0, length(participants)), participants)
    for (m in matches) {
        if (m$status != "complete") next
        if (is.na(m$participant1) || is.na(m$participant2)) next
        p1 <- m$participant1
        p2 <- m$participant2
        if (!(p1 %in% participants && p2 %in% participants)) next
        s1 <- m$score1
        s2 <- m$score2
        if (s1 > s2) {
            points[p1] <- points[p1] + 1
        } else if (s2 > s1) {
            points[p2] <- points[p2] + 1
        } else if (allow_ties) {
            points[p1] <- points[p1] + 0.5
            points[p2] <- points[p2] + 0.5
        }
    }
    points
}

#' Apply tiebreaker ordering
#'
#' @param df Standings data frame with participant column
#' @param matches List of matches
#' @param tiebreakers Character vector of tiebreakers
#' @param allow_ties Logical
#' @return Ordered data frame with rank column
#' @keywords internal
apply_tiebreakers <- function(df, matches, tiebreakers, allow_ties = TRUE) {
    if (!"alphabetical" %in% tiebreakers) {
        tiebreakers <- c(tiebreakers, "alphabetical")
    }

    # Compute head-to-head within tie groups defined by earlier keys
    if ("head_to_head" %in% tiebreakers) {
        df$head_to_head <- 0
        keys <- character(0)
        for (tb in tiebreakers) {
            if (tb == "head_to_head") {
                if (length(keys) == 0) {
                    groups <- list(df$participant)
                } else {
                    key_df <- df[, keys, drop = FALSE]
                    groups <- split(df$participant, interaction(key_df, drop = TRUE))
                }
                h2h <- setNames(rep(0, nrow(df)), df$participant)
                for (g in groups) {
                    if (length(g) <= 1) next
                    pts <- head_to_head_points(matches, g, allow_ties = allow_ties)
                    h2h[g] <- pts[g]
                }
                df$head_to_head <- h2h[df$participant]
            }
            keys <- c(keys, tb)
        }
    }

    order_args <- list()
    for (tb in tiebreakers) {
        if (tb == "alphabetical") {
            order_args[[length(order_args) + 1L]] <- df$participant
            next
        }
        v <- df[[tb]]
        if (is.null(v)) {
            stop("Missing tiebreaker column: ", tb)
        }
        if (tiebreaker_direction(tb) == "desc") {
            v <- -v
        }
        order_args[[length(order_args) + 1L]] <- v
    }

    ord <- do.call(order, c(order_args, list(na.last = TRUE)))
    df <- df[ord, , drop = FALSE]
    df$rank <- seq_len(nrow(df))
    rownames(df) <- NULL
    df
}
