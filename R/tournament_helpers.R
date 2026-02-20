#' Fluent tournament result entry helper
#'
#' Convenience wrapper around `set_result()` for tournament workflows.
#'
#' @param tournament A `tournament` object.
#' @param stage Stage identifier containing the match.
#' @param match Match identifier inside `stage`.
#' @param score Numeric vector score payload. For a single match, pass
#'   `c(score1, score2)`.
#' @param overwrite Logical; forwards to `set_result(..., overwrite = ...)`.
#' @param auto_advance Optional logical override. If `NULL`, defaults to the
#'   tournament's `auto_advance` setting when present.
#'
#' @return Updated `tournament` object.
#' @examples
#' teams <- c("A", "B", "C", "D")
#' trn <- tournament(teams) |>
#'   round_robin("groups")
#'
#' # Enter a single result
#' trn <- result(trn, "groups", match = 1, score = c(2, 1))
#' @export
result <- function(tournament, stage, match, score,
                   overwrite = FALSE, auto_advance = NULL) {
    if (!inherits(tournament, "tournament")) {
        stop("`result()` requires a tournament object")
    }
    if (!is.character(stage) || length(stage) != 1L || is.na(stage) || stage == "") {
        stop("`stage` must be a non-empty string")
    }
    if (length(match) != 1L || is.na(match)) {
        stop("`match` must be a non-NA scalar")
    }
    if (!is.numeric(score) || anyNA(score) || length(score) < 2L) {
        stop("`score` must be a numeric vector of length >= 2")
    }
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
        stop("`overwrite` must be TRUE or FALSE")
    }
    if (is.null(auto_advance)) {
        auto_advance <- tournament$auto_advance
        if (is.null(auto_advance)) {
            auto_advance <- FALSE
        }
    }
    if (!is.logical(auto_advance) || length(auto_advance) != 1L || is.na(auto_advance)) {
        stop("`auto_advance` must be TRUE or FALSE")
    }
    normalized <- normalize_result_score(score)

    set_result(
        tournament,
        stage_id = stage,
        match_id = match,
        score1 = normalized$score1,
        score2 = normalized$score2,
        overwrite = overwrite,
        auto_advance = auto_advance
    )
}

normalize_result_score <- function(score) {
    if (!is.numeric(score) || anyNA(score) || length(score) < 2L) {
        stop("`score` must be a numeric vector of length >= 2")
    }

    if (length(score) == 2L) {
        return(list(score1 = score[[1]], score2 = score[[2]]))
    }

    score1 <- as.numeric(score[seq.int(1L, length(score), by = 2L)])
    score2 <- as.numeric(score[seq.int(2L, length(score), by = 2L)])
    if (length(score1) > length(score2)) {
        score2 <- c(score2, 0)
    }

    list(score1 = score1, score2 = score2)
}

#' Fluent tournament batch result entry helper
#'
#' Convenience wrapper for entering multiple match results for one stage.
#'
#' @param tournament A `tournament` object.
#' @param stage Stage identifier containing the matches.
#' @param df Data frame with required columns: `match`, `score1`, `score2`.
#' @param overwrite Logical; forwards to `result(..., overwrite = ...)`.
#' @param auto_advance Optional logical override for the final row. If `NULL`,
#'   defaults to the tournament's `auto_advance` setting when present.
#'
#' @return Updated `tournament` object.
#' @examples
#' teams <- c("A", "B", "C", "D")
#' trn <- tournament(teams) |>
#'   round_robin("groups")
#'
#' m <- matches(trn, "groups")
#' trn <- results(trn, "groups", data.frame(
#'   match  = m$match_id,
#'   score1 = c(2, 1, 3),
#'   score2 = c(1, 2, 0)
#' ))
#' @export
results <- function(tournament, stage, df, overwrite = FALSE, auto_advance = NULL) {
    if (!inherits(tournament, "tournament")) {
        stop("`results()` requires a tournament object")
    }
    if (!is.character(stage) || length(stage) != 1L || is.na(stage) || stage == "") {
        stop("`stage` must be a non-empty string")
    }
    if (!is.data.frame(df)) {
        stop("`df` must be a data.frame with columns `match`, `score1`, `score2`")
    }
    required_cols <- c("match", "score1", "score2")
    if (!all(required_cols %in% names(df))) {
        stop("`df` must include columns: `match`, `score1`, `score2`")
    }
    if (anyNA(df$match)) {
        stop("`df$match` must not contain NA")
    }
    if (!is.numeric(df$score1) || !is.numeric(df$score2)) {
        stop("`df$score1` and `df$score2` must be numeric")
    }
    if (any(!is.finite(df$score1)) || any(!is.finite(df$score2))) {
        stop("`df$score1` and `df$score2` must be finite and non-missing")
    }
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
        stop("`overwrite` must be TRUE or FALSE")
    }
    if (is.null(auto_advance)) {
        auto_advance <- tournament$auto_advance
        if (is.null(auto_advance)) {
            auto_advance <- FALSE
        }
    }
    if (!is.logical(auto_advance) || length(auto_advance) != 1L || is.na(auto_advance)) {
        stop("`auto_advance` must be TRUE or FALSE")
    }

    if (nrow(df) == 0L) {
        return(tournament)
    }

    for (i in seq_len(nrow(df))) {
        final_row <- i == nrow(df)
        tournament <- result(
            tournament = tournament,
            stage = stage,
            match = df$match[[i]],
            score = c(df$score1[[i]], df$score2[[i]]),
            overwrite = overwrite,
            auto_advance = if (final_row) auto_advance else FALSE
        )
    }

    tournament
}
