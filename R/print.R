#' Print bracketeer objects
#'
#' @name print
#' @param x A bracket or match object.
#' @param ... Additional arguments (unused).
#' @return The object, invisibly.
NULL

#' @rdname print
#' @export
print.bracket_match <- function(x, ...) {
    p1 <- if (is.na(x$participant1)) "TBD" else x$participant1
    p2 <- if (is.na(x$participant2)) "TBD" else x$participant2

    if (x$status == "complete") {
        if (!is.na(x$score1) && !is.na(x$score2)) {
            cat(sprintf("Match %d: %s (%d) vs %s (%d) - Winner: %s\n",
                x$id, p1, x$score1, p2, x$score2, x$winner))
        } else {
            cat(sprintf("Match %d: %s vs %s - Winner: %s\n",
                x$id, p1, p2, x$winner))
        }
    } else {
        cat(sprintf("Match %d: %s vs %s [%s]\n",
            x$id, p1, p2, x$status))
    }
    invisible(x)
}

#' @rdname print
#' @export
print.bracket <- function(x, ...) {
    cat(sprintf("%s Tournament\n", format_type(x$type)))
    cat(sprintf("Participants: %d\n", length(x$participants)))
    cat(sprintf("Rounds: %d\n", x$rounds))
    cat("---\n")

    # Group matches by round
    for (r in seq_len(x$rounds)) {
        round_matches <- Filter(function(m) m$round == r, x$matches)
        if (length(round_matches) == 0) next

        cat(sprintf("\nRound %d:\n", r))
        for (match in round_matches) {
            cat("  ")
            print(match)
        }
    }

    if (is_complete(x)) {
        cat(sprintf("\nWinner: %s\n", get_winner(x)))
    } else {
        pending <- sum(vapply(x$matches, function(m) m$status == "pending", logical(1)))
        complete <- sum(vapply(x$matches, function(m) m$status == "complete", logical(1)))
        cat(sprintf("\nProgress: %d/%d matches complete\n", complete, complete + pending))
    }

    invisible(x)
}

#' @rdname print
#' @export
print.double_elim_bracket <- function(x, ...) {
    cat(sprintf("%s Tournament\n", format_type(x$type)))
    cat(sprintf("Participants: %d\n", length(x$participants)))
    cat("---\n")

    # Winners bracket
    cat("\nWinners Bracket:\n")
    for (r in seq_len(x$winners_rounds)) {
        round_matches <- Filter(function(m) {
            m$round == r && m$bracket_type == "winners"
        }, x$matches)
        if (length(round_matches) == 0) next
        cat(sprintf("  Round %d:\n", r))
        for (match in round_matches) {
            cat("    ")
            print(match)
        }
    }

    # Losers bracket
    if (x$losers_rounds > 0) {
        cat("\nLosers Bracket:\n")
        for (r in seq_len(x$losers_rounds)) {
            round_matches <- Filter(function(m) {
                m$round == r && m$bracket_type == "losers"
            }, x$matches)
            if (length(round_matches) == 0) next
            cat(sprintf("  Round %d:\n", r))
            for (match in round_matches) {
                cat("    ")
                print(match)
            }
        }
    }

    # Grand final(s)
    gf <- x$matches[[x$grand_final_id]]
    cat("\nGrand Final:\n")
    cat("  ")
    print(gf)
    if (!is.na(x$grand_final_reset_id)) {
        reset <- x$matches[[x$grand_final_reset_id]]
        cat("  Reset:\n")
        cat("    ")
        print(reset)
    }

    if (is_complete(x)) {
        cat(sprintf("\nWinner: %s\n", get_winner(x)))
    } else {
        pending <- sum(vapply(x$matches, function(m) m$status == "pending", logical(1)))
        complete <- sum(vapply(x$matches, function(m) m$status == "complete", logical(1)))
        cat(sprintf("\nProgress: %d/%d matches complete\n", complete, complete + pending))
    }

    invisible(x)
}

#' @rdname print
#' @export
print.tournament <- function(x, ...) {
    status <- stage_status(x)
    n_stages <- nrow(status)

    cat(sprintf("Tournament [%d stage%s]\n", n_stages, if (n_stages == 1L) "" else "s"))
    if (n_stages == 0L) {
        return(invisible(x))
    }

    for (i in seq_len(n_stages)) {
        cat(
            sprintf(
                "  %-12s %-12s %d/%d matches\n",
                status$stage[[i]],
                status$status[[i]],
                status$complete[[i]],
                status$total[[i]]
            )
        )
    }

    invisible(x)
}

#' Summarize bracketeer objects
#'
#' @name summary
#' @param object A bracket object.
#' @param ... Additional arguments (unused).
#' @return The object, invisibly.
NULL

#' @rdname summary
#' @export
summary.bracket <- function(object, ...) {
    complete <- sum(vapply(object$matches, function(m) m$status == "complete", logical(1)))
    total <- length(object$matches)

    cat(sprintf("%s Tournament Summary\n", format_type(object$type)))
    cat(sprintf("Participants: %d\n", length(object$participants)))
    cat(sprintf("Total matches: %d\n", total))
    cat(sprintf("Completed: %d (%.0f%%)\n", complete, 100 * complete / total))
    cat(sprintf("Remaining: %d\n", total - complete))

    if (is_complete(object)) {
        cat(sprintf("Winner: %s\n", get_winner(object)))
    }

    invisible(object)
}

#' Format tournament type for display
#' @keywords internal
format_type <- function(type) {
    switch(type,
        single_elim = "Single Elimination",
        double_elim = "Double Elimination",
        round_robin = "Round Robin",
        swiss = "Swiss System",
        group_stage_knockout = "Group Stage + Knockout",
        two_leg_knockout = "Two-Leg Knockout",
        type
    )
}
