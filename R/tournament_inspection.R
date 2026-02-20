#' Inspect tournament matches
#'
#' @param x A `tournament` object.
#' @param stage Optional stage identifier.
#' @param status One of `"pending"`, `"complete"`, or `"all"`.
#'
#' @return Data frame of matches.
#' @examples
#' trn <- tournament(c("A", "B", "C", "D")) |>
#'   round_robin("groups")
#'
#' # Get pending matches
#' matches(trn, "groups")
#'
#' # Get all matches across stages
#' matches(trn, status = "all")
#' @export
matches <- function(x, stage = NULL, status = "pending") {
    UseMethod("matches")
}

#' @rdname matches
#' @export
matches.tournament <- function(x, stage = NULL, status = "pending") {
    status <- match.arg(status, c("pending", "complete", "all"))

    if (is.null(stage)) {
        out <- export_matches(x)
        if (status != "all" && "status" %in% names(out)) {
            out <- out[out$status == status, , drop = FALSE]
            rownames(out) <- NULL
        }
        return(out)
    }

    assert_runtime_scalar_string(stage, arg = "stage")
    if (!stage %in% names(x$stage_state)) {
        stop("Unknown stage_id `", stage, "`")
    }

    state <- x$stage_state[[stage]]
    if (!isTRUE(state$materialized)) {
        return(data.frame(
            stage_id = character(0),
            match_id = character(0),
            compound_match_id = character(0),
            stringsAsFactors = FALSE
        ))
    }

    out <- get_matches(state$bracket, status = status)
    if (!is.data.frame(out) || nrow(out) == 0L) {
        return(data.frame(
            stage_id = character(0),
            match_id = character(0),
            compound_match_id = character(0),
            stringsAsFactors = FALSE
        ))
    }
    out$stage_id <- stage
    out$match_id <- as.character(out$id)
    out$compound_match_id <- paste0(stage, "::", out$match_id)

    cols <- c("stage_id", "match_id", "compound_match_id", setdiff(names(out), c("stage_id", "match_id", "compound_match_id")))
    out <- out[, cols, drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Inspect tournament standings
#'
#' @param x A `tournament` object.
#' @param stage Optional stage identifier.
#'
#' @return Data frame of standings.
#' @examples
#' trn <- tournament(c("A", "B", "C", "D")) |>
#'   round_robin("groups")
#'
#' # Enter some results
#' m <- matches(trn, "groups")
#' trn <- result(trn, "groups", m$match_id[1], score = c(2, 1))
#'
#' # View current standings
#' standings(trn, "groups")
#' @export
standings <- function(x, stage = NULL) {
    UseMethod("standings")
}

#' @rdname standings
#' @export
standings.tournament <- function(x, stage = NULL) {
    if (is.null(stage)) {
        return(export_standings(x))
    }

    assert_runtime_scalar_string(stage, arg = "stage")
    if (!stage %in% names(x$stage_state)) {
        stop("Unknown stage_id `", stage, "`")
    }

    state <- x$stage_state[[stage]]
    if (!isTRUE(state$materialized)) {
        return(data.frame(
            stage_id = character(0),
            rank = integer(0),
            participant = character(0),
            stringsAsFactors = FALSE
        ))
    }

    out <- get_standings(state$bracket)
    if (!"participant" %in% names(out)) {
        out$participant <- as.character(seq_len(nrow(out)))
    }
    if (!"rank" %in% names(out)) {
        out$rank <- seq_len(nrow(out))
    }
    out$stage_id <- stage

    cols <- c("stage_id", "rank", "participant", setdiff(names(out), c("stage_id", "rank", "participant")))
    out <- out[, cols, drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Inspect tournament stage status
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame with one row per stage.
#' @export
stage_status <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`stage_status()` requires a tournament object")
    }

    stage_ids <- ordered_stage_ids_for_export(tournament)
    if (length(stage_ids) == 0L) {
        return(data.frame(
            stage = character(0),
            status = character(0),
            complete = integer(0),
            total = integer(0),
            materialized = logical(0),
            stringsAsFactors = FALSE
        ))
    }

    rows <- lapply(stage_ids, function(stage_id) {
        state <- tournament$stage_state[[stage_id]]
        total <- 0L
        complete <- 0L

        if (isTRUE(state$materialized)) {
            ms <- tryCatch(get_matches(state$bracket, status = "all"), error = function(e) data.frame())
            if (is.data.frame(ms) && nrow(ms) > 0L) {
                total <- as.integer(nrow(ms))
                if ("status" %in% names(ms)) {
                    complete <- as.integer(sum(ms$status == "complete"))
                }
            }
        }

        data.frame(
            stage = stage_id,
            status = as.character(state$status),
            complete = complete,
            total = total,
            materialized = isTRUE(state$materialized),
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

#' Get tournament winner
#'
#' @param tournament A `tournament` object.
#'
#' @return Winner name or `NA_character_`.
#' @examples
#' teams <- c("A", "B", "C", "D")
#' trn <- tournament(teams) |>
#'   round_robin("groups") |>
#'   single_elim("finals", take = top_n(2))
#'
#' # ... enter all results ...
#'
#' # Get the champion
#' winner(trn)
#' @export
winner <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`winner()` requires a tournament object")
    }

    get_winner(tournament)
}

#' Get tournament rankings
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame of rankings.
#' @export
rankings <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`rankings()` requires a tournament object")
    }

    out <- tournament$rankings
    if (is.null(out) || !is.data.frame(out)) {
        out <- compute_tournament_rankings(tournament)
    }
    if (is.null(out) || !is.data.frame(out)) {
        return(data.frame(
            rank = integer(0),
            participant = character(0),
            stringsAsFactors = FALSE
        ))
    }

    out
}

#' Get transition routing log
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame audit trail.
#' @export
routing_log <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`routing_log()` requires a tournament object")
    }

    export_tournament_log(tournament)
}
