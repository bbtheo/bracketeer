#' Create a bracketeer tournament specification
#'
#' @return A `bracketeer_spec` object.
#' @export
spec <- function() {
    x <- tournament_spec()
    class(x) <- unique(c("bracketeer_spec", class(x)))
    x
}

#' Build a live tournament runtime from a specification
#'
#' @param x A `bracketeer_spec` or `tournament_spec` object.
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column.
#'
#' @return A tournament runtime object.
#' @export
build <- function(x, participants) {
    if (!inherits(x, "bracketeer_spec") && !inherits(x, "tournament_spec")) {
        stop(
            "`build()` expected `bracketeer_spec` or `tournament_spec`; ",
            "actual class `", paste(class(x), collapse = "/"), "`."
        )
    }

    participant_info <- normalize_participants(participants)
    validate(x, n = nrow(participant_info$data))
    trn <- build_tournament(x, participants)
    class(trn) <- unique(c("tournament", "bracketeer_spec", class(trn)))
    trn
}

#' Create an empty live tournament pipeline
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column.
#' @param auto_advance Logical scalar. Stored as the runtime default for future
#'   result-entry helpers.
#'
#' @return A tournament runtime object with no stages materialized yet.
#' @export
tournament <- function(participants, auto_advance = TRUE) {
    if (!is.logical(auto_advance) || length(auto_advance) != 1L || is.na(auto_advance)) {
        stop(
            "`auto_advance` expected logical scalar TRUE/FALSE; ",
            "actual class `", paste(class(auto_advance), collapse = "/"),
            "` with length ", length(auto_advance), "."
        )
    }

    trn <- build(spec(), participants)
    trn$auto_advance <- auto_advance
    trn
}

#' Validate a tournament spec preflight
#'
#' @param x A `bracketeer_spec` or `tournament_spec` object.
#' @param n Participant count for feasibility checks.
#'
#' @return A preflight validation summary.
#' @export
validate <- function(x, n) {
    if (!inherits(x, "bracketeer_spec") && !inherits(x, "tournament_spec")) {
        stop(
            "`validate()` expected `bracketeer_spec` or `tournament_spec`; ",
            "actual class `", paste(class(x), collapse = "/"), "`."
        )
    }

    validate_tournament(x, n_participants = n)
}
