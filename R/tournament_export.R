#' Export tournament routing log entries
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame with one row per routing log entry.
export_tournament_log <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`export_tournament_log()` requires a tournament object")
    }

    entries <- get_routing_log(tournament)
    if (length(entries) == 0L) {
        return(data.frame(
            source_stage_id = character(0),
            transition_id = character(0),
            rule_applied = character(0),
            selected = character(0),
            selected_count = integer(0),
            pool_before = integer(0),
            pool_after = integer(0),
            timestamp = as.POSIXct(character(0)),
            stringsAsFactors = FALSE
        ))
    }

    timestamps <- as.POSIXct(
        vapply(entries, function(entry) {
            as.numeric(as.POSIXct(entry$timestamp))
        }, numeric(1)),
        origin = "1970-01-01"
    )

    data.frame(
        source_stage_id = vapply(entries, `[[`, character(1), "source_stage_id"),
        transition_id = vapply(entries, `[[`, character(1), "transition_id"),
        rule_applied = vapply(entries, `[[`, character(1), "rule_applied"),
        selected = vapply(entries, function(entry) {
            selected <- entry$selected
            if (is.null(selected) || length(selected) == 0L) {
                return("")
            }
            paste(as.character(selected), collapse = ", ")
        }, character(1)),
        selected_count = vapply(entries, function(entry) {
            selected <- entry$selected
            if (is.null(selected)) {
                return(0L)
            }
            as.integer(length(selected))
        }, integer(1)),
        pool_before = vapply(entries, `[[`, integer(1), "pool_before"),
        pool_after = vapply(entries, `[[`, integer(1), "pool_after"),
        timestamp = timestamps,
        stringsAsFactors = FALSE
    )
}

#' Export tournament matches across materialized stages
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame with stage-tagged matches and compound match IDs.
export_matches <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`export_matches()` requires a tournament object")
    }

    stage_rows <- lapply(ordered_stage_ids_for_export(tournament), function(stage_id) {
        state <- tournament$stage_state[[stage_id]]
        if (!isTRUE(state$materialized)) {
            return(NULL)
        }

        matches <- tryCatch(
            get_matches(state$bracket, status = "all"),
            error = function(err) NULL
        )
        if (is.null(matches) || !is.data.frame(matches) || nrow(matches) == 0L) {
            return(NULL)
        }

        if ("id" %in% names(matches)) {
            match_id <- as.character(matches$id)
        } else {
            match_id <- as.character(seq_len(nrow(matches)))
        }

        matches$stage_id <- stage_id
        matches$match_id <- match_id
        matches$compound_match_id <- paste0(stage_id, "::", match_id)

        ordered_cols <- c(
            "stage_id",
            "match_id",
            "compound_match_id",
            setdiff(names(matches), c("stage_id", "match_id", "compound_match_id"))
        )
        matches[, ordered_cols, drop = FALSE]
    })

    exported <- bind_data_frame_rows(stage_rows)
    if (is.null(exported)) {
        return(data.frame(
            stage_id = character(0),
            match_id = character(0),
            compound_match_id = character(0),
            stringsAsFactors = FALSE
        ))
    }

    exported
}

#' Export tournament standings across materialized stages
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame with stage-tagged standings.
export_standings <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`export_standings()` requires a tournament object")
    }

    stage_rows <- lapply(ordered_stage_ids_for_export(tournament), function(stage_id) {
        state <- tournament$stage_state[[stage_id]]
        if (!isTRUE(state$materialized)) {
            return(NULL)
        }

        standings <- tryCatch(
            get_standings(state$bracket),
            error = function(err) NULL
        )
        if (is.null(standings) || !is.data.frame(standings) || nrow(standings) == 0L) {
            return(NULL)
        }

        if (!"participant" %in% names(standings)) {
            standings$participant <- as.character(seq_len(nrow(standings)))
        }
        if (!"rank" %in% names(standings)) {
            standings$rank <- seq_len(nrow(standings))
        }

        standings$stage_id <- stage_id
        standings$participant <- as.character(standings$participant)
        standings$rank <- as.integer(standings$rank)

        ordered_cols <- c(
            "stage_id",
            "rank",
            "participant",
            setdiff(names(standings), c("stage_id", "rank", "participant"))
        )
        standings[, ordered_cols, drop = FALSE]
    })

    exported <- bind_data_frame_rows(stage_rows)
    if (is.null(exported)) {
        return(data.frame(
            stage_id = character(0),
            rank = integer(0),
            participant = character(0),
            stringsAsFactors = FALSE
        ))
    }

    exported
}

ordered_stage_ids_for_export <- function(tournament) {
    stage_ids <- names(tournament$stage_state)
    if (is.null(stage_ids) || length(stage_ids) == 0L) {
        return(character(0))
    }

    topological <- tournament$spec$metadata$topological_order
    if (is.null(topological) || length(topological) == 0L) {
        return(stage_ids)
    }

    ordered <- topological[topological %in% stage_ids]
    extras <- setdiff(stage_ids, ordered)
    c(ordered, extras)
}

bind_data_frame_rows <- function(rows) {
    rows <- Filter(function(x) {
        is.data.frame(x) && nrow(x) > 0L
    }, rows)

    if (length(rows) == 0L) {
        return(NULL)
    }

    all_cols <- unique(unlist(lapply(rows, names), use.names = FALSE))

    normalized_rows <- lapply(rows, function(df) {
        missing_cols <- setdiff(all_cols, names(df))
        if (length(missing_cols) > 0L) {
            for (col_name in missing_cols) {
                df[[col_name]] <- NA
            }
        }
        df <- df[, all_cols, drop = FALSE]
        rownames(df) <- NULL
        df
    })

    out <- do.call(rbind, normalized_rows)
    rownames(out) <- NULL
    out
}
