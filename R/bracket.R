#' Create a tournament bracket
#'
#' Generic constructor for tournament brackets. Dispatches to specific
#' tournament type constructors.
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' or 'rating' columns.
#' @param type Tournament type: "single_elim", "double_elim", "round_robin",
#'   "swiss", "group_stage_knockout", or "two_leg_knockout"
#' @param ... Additional arguments passed to type-specific constructor
#'
#' @return A bracket object
#' @keywords internal
bracket <- function(participants, type = "single_elim", ...) {
    if (inherits(participants, "stage_spec")) {
        stop(
            "`bracket()` no longer accepts stage specs directly. ",
            "Use `stage$builder(participants)` or the tournament runtime APIs."
        )
    }

    type <- match.arg(type, c(
        "single_elim",
        "double_elim",
        "round_robin",
        "swiss",
        "group_stage_knockout",
        "two_leg_knockout"
    ))

    switch(type,
        single_elim = single_elim.default(participants, ...),
        double_elim = double_elim.default(participants, ...),
        round_robin = round_robin.default(participants, ...),
        swiss = swiss.default(participants, ...),
        group_stage_knockout = group_stage_knockout.default(participants, ...),
        two_leg_knockout = two_leg_knockout.default(participants, ...)
    )
}

#' Internal generic bracket constructor
#'
#' @inheritParams bracket
#' @return A bracket object.
#' @keywords internal
new_bracket <- function(participants, type = "single_elim", ...) {
    bracket(participants = participants, type = type, ...)
}

#' Set match result by scores
#'
#' Record the score for a match. The winner is determined by the higher score.
#'
#' @param bracket A bracket object
#' @param match_id The ID of the match to update
#' @param score1 Score for participant 1, or a numeric vector of game scores.
#' @param score2 Score for participant 2, or a numeric vector of game scores.
#'
#' @param stage_id Optional stage identifier used by multi-stage tournament
#'   runtimes.
#' @param overwrite Logical; when `TRUE`, requests explicit result overwrite
#'   handling where supported.
#' @param auto_advance Logical; when `TRUE` and supported by the bracket type,
#'   automatically advances completed stages.
#'
#' @return Updated bracket object
#' @keywords internal
set_result <- function(bracket, match_id, score1, score2, stage_id = NULL,
                       overwrite = FALSE, auto_advance = FALSE) {
    UseMethod("set_result")
}

#' @rdname set_result
set_result.bracket <- function(bracket, match_id, score1, score2,
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
    if (!is.null(bracket$legs) && !is.na(bracket$legs)) {
        if (length(score1) != bracket$legs || length(score2) != bracket$legs) {
            stop("This format requires ", bracket$legs, " legs of scores")
        }
    }
    allow_ties <- isTRUE(bracket$allow_ties)
    if (isTRUE(bracket$away_goals)) {
        allow_ties <- TRUE
    }
    result <- normalize_match_scores(
        score1 = score1,
        score2 = score2,
        best_of = bracket$best_of,
        allow_ties = allow_ties,
        participant1 = match$participant1,
        participant2 = match$participant2
    )

    if (isTRUE(bracket$away_goals) && is.na(result$winner)) {
        if (is.null(bracket$legs) || is.na(bracket$legs) || bracket$legs != 2L) {
            stop("Away goals are only supported for two-leg matches")
        }
        away1 <- score1[2]
        away2 <- score2[1]
        if (away1 > away2) {
            result$winner <- match$participant1
            result$loser <- match$participant2
        } else if (away2 > away1) {
            result$winner <- match$participant2
            result$loser <- match$participant1
        } else {
            stop("Aggregate tied and away goals equal; resolve tie manually")
        }
    }

    bracket$matches[[match_id]]$score1 <- result$score1
    bracket$matches[[match_id]]$score2 <- result$score2
    bracket$matches[[match_id]]$games <- result$games
    bracket$matches[[match_id]]$winner <- result$winner
    bracket$matches[[match_id]]$loser <- result$loser
    bracket$matches[[match_id]]$status <- "complete"

    # Advance winner to next match
    if (!isTRUE(bracket$reseed)) {
        bracket <- advance_winner(bracket, match_id)
    }

    bracket
}

#' Set match winner directly
#'
#' Record the winner of a match without specifying scores.
#'
#' @param bracket A bracket object
#' @param match_id The ID of the match to update
#' @param winner Name of the winning participant
#'
#' @return Updated bracket object
#' @keywords internal
set_winner <- function(bracket, match_id, winner) {
    UseMethod("set_winner")
}

#' @rdname set_winner
set_winner.bracket <- function(bracket, match_id, winner) {
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
    if (!winner %in% c(match$participant1, match$participant2)) {
        stop("Winner must be one of the match participants")
    }

    bracket$matches[[match_id]]$score1 <- NA_real_
    bracket$matches[[match_id]]$score2 <- NA_real_
    bracket$matches[[match_id]]$games <- NULL
    bracket$matches[[match_id]]$winner <- winner
    bracket$matches[[match_id]]$loser <- if (winner == match$participant1) {
        match$participant2
    } else {
        match$participant1
    }
    bracket$matches[[match_id]]$status <- "complete"

    # Advance winner to next match
    if (!isTRUE(bracket$reseed)) {
        bracket <- advance_winner(bracket, match_id)
    }

    bracket
}

#' Advance winner to next match
#'
#' @param bracket A bracket object
#' @param match_id The ID of the completed match
#' @return Updated bracket object
#' @keywords internal
advance_winner <- function(bracket, match_id) {
    UseMethod("advance_winner")
}

#' @rdname advance_winner
#' @export
advance_winner.bracket <- function(bracket, match_id) {
    match <- bracket$matches[[match_id]]

    if (!is.na(match$next_match)) {
        next_id <- match$next_match
        next_slot <- match$next_slot

        if (next_slot == 1L) {
            bracket$matches[[next_id]]$participant1 <- match$winner
        } else {
            bracket$matches[[next_id]]$participant2 <- match$winner
        }
    }

    bracket
}

#' Get matches from a bracket
#'
#' @param bracket A bracket object
#' @param round Optional round number to filter by
#' @param status Filter by status: "pending", "complete", or "all"
#'
#' @return Data frame of matches
#' @keywords internal
get_matches <- function(bracket, round = NULL, status = "all") {
    UseMethod("get_matches")
}

#' @rdname get_matches
get_matches.bracket <- function(bracket, round = NULL, status = "all") {
    status <- match.arg(status, c("pending", "complete", "all"))

    matches <- bracket$matches

    # Convert to data frame
    df <- data.frame(
        id = vapply(matches, `[[`, integer(1), "id"),
        round = vapply(matches, `[[`, integer(1), "round"),
        position = vapply(matches, `[[`, integer(1), "position"),
        participant1 = vapply(matches, `[[`, character(1), "participant1"),
        participant2 = vapply(matches, `[[`, character(1), "participant2"),
        score1 = vapply(matches, function(m) {
            if (is.na(m$score1)) NA_real_ else m$score1
        }, numeric(1)),
        score2 = vapply(matches, function(m) {
            if (is.na(m$score2)) NA_real_ else m$score2
        }, numeric(1)),
        winner = vapply(matches, `[[`, character(1), "winner"),
        bracket_type = vapply(matches, `[[`, character(1), "bracket_type"),
        status = vapply(matches, `[[`, character(1), "status"),
        stringsAsFactors = FALSE
    )

    # Filter by round
    if (!is.null(round)) {
        df <- df[df$round == round, ]
    }

    # Filter by status
    if (status != "all") {
        df <- df[df$status == status, ]
    }

    rownames(df) <- NULL
    df
}

#' Get tournament standings
#'
#' @param bracket A bracket object
#'
#' @details
#' Standings are ordered using bracket-specific tiebreakers where applicable
#' (e.g., round robin and Swiss).
#'
#' @return Data frame with standings
#' @keywords internal
get_standings <- function(bracket) {
    UseMethod("get_standings")
}

#' Get tournament winner
#'
#' @param bracket A bracket object
#'
#' @return Name of winner, or NA if tournament not complete
#' @keywords internal
get_winner <- function(bracket) {
    UseMethod("get_winner")
}

#' @rdname get_winner
get_winner.bracket <- function(bracket) {
    final_match <- bracket$matches[[length(bracket$matches)]]
    if (final_match$status == "complete") {
        return(final_match$winner)
    }
    NA_character_
}

#' Check if tournament is complete
#'
#' @param bracket A bracket object
#'
#' @return Logical
#' @keywords internal
is_complete <- function(bracket) {
    UseMethod("is_complete")
}

#' @rdname is_complete
is_complete.bracket <- function(bracket) {
    all(vapply(bracket$matches, function(m) m$status == "complete", logical(1)))
}

#' Advance tournament to next round
#'
#' Check if current round is complete and update bracket state.
#'
#' @param x A bracket object.
#' @param stage Optional stage identifier for tournament methods.
#' @param ... Additional method-specific arguments.
#'
#' @return Updated bracket object
#' @export
advance <- function(x, stage = NULL, ...) {
    UseMethod("advance")
}

#' @rdname advance
#' @export
advance.bracket <- function(x, stage = NULL, ...) {
    # Default implementation - just return bracket unchanged
    # Specific types may override

    x
}

#' Teardown tournament state
#'
#' For tournament runtimes, this un-materializes a stage and its downstream
#' dependents so upstream results can be corrected and replayed.
#'
#' @param x A bracket or tournament object.
#' @param stage Stage identifier to teardown (tournament method).
#' @param ... Additional method-specific arguments.
#'
#' @return Updated object.
#' @export
teardown <- function(x, stage = NULL, ...) {
    UseMethod("teardown")
}

#' @rdname teardown
#' @export
teardown.bracket <- function(x, stage = NULL, ...) {
    x
}
