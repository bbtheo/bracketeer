#' Build a tournament runtime from a tournament specification
#'
#' @param spec A `tournament_spec` object.
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column.
#'
#' @return A `tournament` runtime object.
build_tournament <- function(spec, participants) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`build_tournament()` requires a tournament_spec object")
    }

    spec <- validate_tournament_spec(spec)
    participant_info <- normalize_participants(participants)

    stage_ids <- names(spec$stages)
    if (is.null(stage_ids)) {
        stage_ids <- character(0)
    }

    stage_state <- setNames(vector("list", length(stage_ids)), stage_ids)
    for (stage_id in stage_ids) {
        stage_state[[stage_id]] <- list(
            bracket = NULL,
            status = "blocked",
            participants = character(0),
            materialized = FALSE,
            advanced = FALSE,
            pending_by_transition = list(),
            resolved_inbound_transition_ids = character(0)
        )
    }

    source_stage_ids <- spec$metadata$source_stage_ids
    if (is.null(source_stage_ids)) {
        source_stage_ids <- character(0)
    }

    for (stage_id in source_stage_ids) {
        stage_bracket <- materialize_stage(
            stage = spec$stages[[stage_id]],
            participants = participant_info$data,
            stage_id = stage_id
        )

        stage_state[[stage_id]] <- list(
            bracket = stage_bracket,
            status = if (is_stage_complete(stage_bracket)) "complete" else "in_progress",
            participants = extract_stage_participants(stage_bracket),
            materialized = TRUE,
            advanced = FALSE,
            pending_by_transition = list(),
            resolved_inbound_transition_ids = character(0)
        )
    }

    tournament <- structure(
        list(
            spec = spec,
            participants = participant_info$data,
            stage_state = stage_state,
            active_stage_ids = character(0),
            completed = FALSE,
            rankings = NULL,
            routing_log = list()
        ),
        class = "tournament"
    )

    refresh_tournament_state(tournament)
}

#' Get stage IDs currently ready to advance
#'
#' @param tournament A `tournament` object.
#'
#' @return Character vector of stage IDs in deterministic order.
get_ready_stages <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`get_ready_stages()` requires a tournament object")
    }

    ready_to_advance_stage_ids(tournament)
}

#' Get transition routing log entries
#'
#' @param tournament A `tournament` object.
#'
#' @return List of routing log entries in append order.
get_routing_log <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`get_routing_log()` requires a tournament object")
    }

    if (is.null(tournament$routing_log) || !is.list(tournament$routing_log)) {
        return(list())
    }

    tournament$routing_log
}

#' Check whether a stage is complete
#'
#' @param x A stage bracket object or `tournament`.
#' @param ... Additional method-specific arguments.
#'
#' @return Logical scalar.
is_stage_complete <- function(x, ...) {
    if (inherits(x, "bracket")) {
        return(is_complete(x))
    }
    if (!inherits(x, "tournament")) {
        stop("`is_stage_complete()` requires a stage bracket or tournament object")
    }

    stage_id <- list(...)[["stage_id"]]
    assert_runtime_scalar_string(stage_id, arg = "stage_id")

    if (!stage_id %in% names(x$stage_state)) {
        stop("Unknown stage_id `", stage_id, "`")
    }

    state <- x$stage_state[[stage_id]]
    if (!isTRUE(state$materialized)) {
        return(FALSE)
    }

    is_stage_complete(state$bracket)
}

#' @rdname get_winner
#' @export
get_winner.tournament <- function(bracket) {
    if (!isTRUE(bracket$completed)) {
        return(NA_character_)
    }

    rankings <- bracket$rankings
    if (!is.data.frame(rankings) || nrow(rankings) == 0L || !"participant" %in% names(rankings)) {
        rankings <- compute_tournament_rankings(bracket)
    }

    if (!is.data.frame(rankings) || nrow(rankings) == 0L || !"participant" %in% names(rankings)) {
        return(NA_character_)
    }

    as.character(rankings$participant[[1]])
}

#' @rdname set_result
#' @export
set_result.tournament <- function(bracket, match_id, score1, score2,
                                  stage_id = NULL, overwrite = FALSE,
                                  auto_advance = TRUE) {
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
        stop("`overwrite` must be TRUE or FALSE")
    }
    if (!is.logical(auto_advance) || length(auto_advance) != 1L || is.na(auto_advance)) {
        stop("`auto_advance` must be TRUE or FALSE")
    }

    tournament <- bracket
    match_ref <- resolve_tournament_match_reference(
        tournament = tournament,
        match_id = match_id,
        stage_id = stage_id
    )

    target_stage_id <- match_ref$stage_id
    target_match_id <- match_ref$match_id
    state <- tournament$stage_state[[target_stage_id]]

    if (!isTRUE(state$materialized)) {
        stop("Stage `", target_stage_id, "` is not materialized")
    }

    if (isTRUE(overwrite)) {
        blocking_stage_id <- blocking_downstream_stage_id(
            tournament = tournament,
            stage_id = target_stage_id
        )
        if (!is.null(blocking_stage_id)) {
            stop("Cannot overwrite result for stage_id `", target_stage_id,
                "` because blocking downstream stage_id `", blocking_stage_id,
                "` is already materialized or has resolved transition outputs")
        }
        state <- overwrite_stage_match_result(
            stage = tournament$spec$stages[[target_stage_id]],
            state = state,
            stage_id = target_stage_id,
            match_id = target_match_id,
            score1 = score1,
            score2 = score2
        )
    } else {
        state$bracket <- set_result(
            state$bracket,
            match_id = target_match_id,
            score1 = score1,
            score2 = score2
        )
    }

    state$participants <- extract_stage_participants(state$bracket)
    tournament$stage_state[[target_stage_id]] <- state

    tournament <- refresh_tournament_state(tournament)

    if (!isTRUE(auto_advance)) {
        return(tournament)
    }

    target_state <- tournament$stage_state[[target_stage_id]]
    if (!identical(target_state$status, "complete")) {
        return(tournament)
    }
    if (isTRUE(target_state$advanced)) {
        return(tournament)
    }
    if (length(outgoing_transitions_for_stage(tournament$spec, target_stage_id)) == 0L) {
        return(tournament)
    }

    advance(tournament, stage_id = target_stage_id)
}

#' @rdname advance
#' @export
advance.tournament <- function(bracket, stage_id = NULL) {
    tournament <- bracket

    if (is.null(stage_id)) {
        ready <- ready_to_advance_stage_ids(tournament)
        if (length(ready) == 0L) {
            stop("No completed stage is ready to advance")
        }
        if (length(ready) > 1L) {
            stop(
                "Multiple completed stages are ready to advance: ",
                paste(ready, collapse = ", "),
                ". Provide `stage_id`."
            )
        }
        stage_id <- ready[[1]]
    } else {
        assert_runtime_scalar_string(stage_id, arg = "stage_id")
    }

    if (!stage_id %in% names(tournament$stage_state)) {
        stop("Unknown stage_id `", stage_id, "`")
    }

    source_state <- tournament$stage_state[[stage_id]]
    if (!isTRUE(source_state$materialized)) {
        stop("Stage `", stage_id, "` is not materialized")
    }
    if (!identical(source_state$status, "complete")) {
        stop("Cannot advance stage_id `", stage_id, "` because it is not complete")
    }
    if (isTRUE(source_state$advanced)) {
        return(tournament)
    }

    outgoing <- outgoing_transitions_for_stage(tournament$spec, stage_id = stage_id)
    if (length(outgoing) == 0L) {
        tournament$stage_state[[stage_id]]$advanced <- TRUE
        return(refresh_tournament_state(tournament))
    }

    source_pool <- source_state$participants
    if (is.null(source_pool)) {
        source_pool <- character(0)
    } else {
        source_pool <- as.character(source_pool)
    }
    selected_by_participant <- list()
    transition_overlap_policy <- list()

    for (edge in outgoing) {
        destination_stage_id <- edge$to_stage_id
        destination_state <- tournament$stage_state[[destination_stage_id]]
        if (isTRUE(destination_state$materialized)) {
            next
        }

        pool_before <- as.integer(length(source_pool))
        selected_participants <- resolve_transition_participants(
            tournament = tournament,
            source_stage_id = stage_id,
            transition = edge,
            source_pool = source_pool
        )

        if (isTRUE(edge$consume)) {
            selected_participants <- filter_selection_by_pool(
                selected_participants = selected_participants,
                source_pool = source_pool,
                transition_id = edge$transition_id,
                source_stage_id = stage_id
            )
            source_pool <- source_pool[!source_pool %in% selected_participants$name]
        }
        detect_transition_overlap_conflicts(
            selected_participants = selected_participants,
            transition = edge,
            selected_by_participant = selected_by_participant,
            transition_overlap_policy = transition_overlap_policy,
            source_stage_id = stage_id
        )
        selected_by_participant <- register_transition_selection(
            selected_by_participant = selected_by_participant,
            transition = edge,
            selected_participants = selected_participants
        )
        transition_overlap_policy[[edge$transition_id]] <- isTRUE(edge$allow_overlap)

        tournament <- register_inbound_transition_output(
            tournament = tournament,
            destination_stage_id = destination_stage_id,
            transition_id = edge$transition_id,
            selected_participants = selected_participants
        )
        tournament <- append_routing_log_entry(
            tournament = tournament,
            source_stage_id = stage_id,
            transition = edge,
            selected_participants = selected_participants,
            pool_before = pool_before,
            pool_after = as.integer(length(source_pool))
        )

        if (!is_stage_ready_for_materialization(
            tournament = tournament,
            stage_id = destination_stage_id
        )) {
            next
        }

        destination_participants <- collect_inbound_participants_for_stage(
            tournament = tournament,
            stage_id = destination_stage_id
        )
        selected_count <- nrow(destination_participants)
        if (selected_count < 2L) {
            stop(
                "Stage_id `", destination_stage_id,
                "` resolved all inbound transitions but selected ",
                selected_count, " participant(s); requires at least 2"
            )
        }

        destination_bracket <- tryCatch(
            materialize_stage(
                stage = tournament$spec$stages[[destination_stage_id]],
                participants = destination_participants,
                stage_id = destination_stage_id
            ),
            error = function(err) {
                stop(
                    "Failed to materialize stage_id `", destination_stage_id,
                    "` from transition_id(s) `",
                    paste(inbound_transition_ids_for_stage(
                        spec = tournament$spec,
                        stage_id = destination_stage_id
                    ), collapse = ", "),
                    "`: ", conditionMessage(err)
                )
            }
        )

        destination_state <- tournament$stage_state[[destination_stage_id]]

        tournament$stage_state[[destination_stage_id]] <- list(
            bracket = destination_bracket,
            status = if (is_stage_complete(destination_bracket)) "complete" else "in_progress",
            participants = extract_stage_participants(destination_bracket),
            materialized = TRUE,
            advanced = FALSE,
            pending_by_transition = destination_state$pending_by_transition,
            resolved_inbound_transition_ids = destination_state$resolved_inbound_transition_ids
        )
    }
    tournament$stage_state[[stage_id]]$advanced <- TRUE

    refresh_tournament_state(tournament)
}

#' @rdname teardown
#' @export
teardown.tournament <- function(bracket, stage_id = NULL) {
    tournament <- bracket
    assert_runtime_scalar_string(stage_id, arg = "stage_id")

    if (!stage_id %in% names(tournament$stage_state)) {
        stop("Unknown stage_id `", stage_id, "`")
    }

    teardown_stage_ids <- c(stage_id, downstream_stage_ids(tournament$spec, stage_id))
    teardown_stage_ids <- unique(teardown_stage_ids)

    for (id in teardown_stage_ids) {
        state <- tournament$stage_state[[id]]
        state$bracket <- NULL
        state$status <- "blocked"
        state$participants <- character(0)
        state$materialized <- FALSE
        state$advanced <- FALSE
        state$pending_by_transition <- list()
        state$resolved_inbound_transition_ids <- character(0)
        tournament$stage_state[[id]] <- state
    }

    reset_advanced <- upstream_sources_for_teardown(
        spec = tournament$spec,
        teardown_stage_ids = teardown_stage_ids
    )
    for (id in reset_advanced) {
        if (id %in% teardown_stage_ids) {
            next
        }
        state <- tournament$stage_state[[id]]
        if (isTRUE(state$materialized)) {
            state$advanced <- FALSE
            tournament$stage_state[[id]] <- state
        }
    }

    refresh_tournament_state(tournament)
}

#' Compute tournament rankings
#'
#' @param tournament A `tournament` object.
#'
#' @return Data frame with `rank` and `participant`, or `NULL` when unavailable.
compute_tournament_rankings <- function(tournament) {
    if (!inherits(tournament, "tournament")) {
        stop("`compute_tournament_rankings()` requires a tournament object")
    }

    stage_ids <- names(tournament$spec$stages)
    if (is.null(stage_ids) || length(stage_ids) == 0L) {
        return(NULL)
    }

    sink_ids <- sink_stage_ids(tournament$spec)
    if (length(sink_ids) == 0L) {
        return(NULL)
    }

    sink_ids <- sink_ids[vapply(sink_ids, function(stage_id) {
        state <- tournament$stage_state[[stage_id]]
        isTRUE(state$materialized) && identical(state$status, "complete")
    }, logical(1))]

    if (length(sink_ids) == 0L) {
        return(NULL)
    }

    topological <- tournament$spec$metadata$topological_order
    if (is.null(topological) || length(topological) == 0L) {
        topological <- stage_ids
    }
    sink_ids <- topological[topological %in% sink_ids]
    if (length(sink_ids) == 0L) {
        return(NULL)
    }

    final_stage <- sink_ids[[1]]
    standings <- tryCatch(
        get_standings(tournament$stage_state[[final_stage]]$bracket),
        error = function(e) NULL
    )
    if (is.null(standings) || !is.data.frame(standings) || nrow(standings) == 0L) {
        return(NULL)
    }

    if (!"participant" %in% names(standings)) {
        return(NULL)
    }
    if (!"rank" %in% names(standings)) {
        standings$rank <- seq_len(nrow(standings))
    }

    track_placements <- tournament$spec$outcome$track_placements
    if (is.null(track_placements) || length(track_placements) != 1L || is.na(track_placements)) {
        track_placements <- 1L
    }
    track_placements <- as.integer(track_placements)
    if (track_placements < 1L) {
        track_placements <- 1L
    }

    n_take <- min(track_placements, nrow(standings))
    rankings <- standings[seq_len(n_take), c("rank", "participant"), drop = FALSE]
    rankings$rank <- as.integer(rankings$rank)
    rankings$participant <- as.character(rankings$participant)
    rownames(rankings) <- NULL
    rankings
}

materialize_stage <- function(stage, participants, stage_id) {
    if (inherits(stage, "stage_spec") &&
        is.list(stage) &&
        is.function(stage$builder)) {
        return(stage$builder(participants))
    }

    stop(
        "Unsupported stage definition for stage_id `", stage_id,
        "`; expected a `stage_spec` with `builder(participants)`"
    )
}

refresh_tournament_state <- function(tournament) {
    stage_ids <- names(tournament$stage_state)
    if (is.null(stage_ids)) {
        stage_ids <- character(0)
    }

    for (stage_id in stage_ids) {
        state <- tournament$stage_state[[stage_id]]
        if (isTRUE(state$materialized)) {
            state$status <- if (is_stage_complete(state$bracket)) "complete" else "in_progress"
            if (is.null(state$participants)) {
                state$participants <- extract_stage_participants(state$bracket)
            }
        } else {
            state$status <- "blocked"
        }
        if (is.null(state$advanced)) {
            state$advanced <- FALSE
        }
        if (is.null(state$pending_by_transition) || !is.list(state$pending_by_transition)) {
            state$pending_by_transition <- list()
        }
        if (is.null(state$resolved_inbound_transition_ids)) {
            state$resolved_inbound_transition_ids <- character(0)
        }
        tournament$stage_state[[stage_id]] <- state
    }

    topological <- tournament$spec$metadata$topological_order
    if (is.null(topological) || length(topological) == 0L) {
        topological <- stage_ids
    }
    in_progress <- stage_ids[vapply(tournament$stage_state, function(state) {
        identical(state$status, "in_progress")
    }, logical(1))]
    tournament$active_stage_ids <- topological[topological %in% in_progress]

    all_complete <- length(stage_ids) > 0L && all(vapply(tournament$stage_state, function(state) {
        isTRUE(state$materialized) && identical(state$status, "complete")
    }, logical(1)))
    tournament$completed <- all_complete
    tournament$rankings <- if (all_complete) compute_tournament_rankings(tournament) else NULL

    tournament
}

extract_stage_participants <- function(stage_bracket) {
    if (!is.null(stage_bracket$participants)) {
        return(as.character(stage_bracket$participants))
    }
    if (!is.null(stage_bracket$participants_df) &&
        is.data.frame(stage_bracket$participants_df) &&
        "name" %in% names(stage_bracket$participants_df)) {
        return(as.character(stage_bracket$participants_df$name))
    }

    standings <- tryCatch(get_standings(stage_bracket), error = function(e) NULL)
    if (is.null(standings) || !is.data.frame(standings) || !"participant" %in% names(standings)) {
        return(character(0))
    }
    as.character(standings$participant)
}

resolve_tournament_match_reference <- function(tournament, match_id, stage_id = NULL) {
    if (length(match_id) != 1L || is.na(match_id)) {
        stop("`match_id` must be a non-NA scalar")
    }

    parsed <- parse_compound_match_id(match_id)
    if (!is.null(parsed)) {
        if (!is.null(stage_id) && !identical(stage_id, parsed$stage_id)) {
            stop(
                "Conflicting stage identifiers: stage_id `", stage_id,
                "` and compound match_id stage `", parsed$stage_id, "`"
            )
        }
        stage_id <- parsed$stage_id
        match_id <- parsed$match_id
    }

    if (is.null(stage_id)) {
        candidates <- stages_with_match_id(tournament, match_id)
        if (length(candidates) == 0L) {
            stop(
                "Unknown match_id `", match_id,
                "`. Provide `stage_id` or use `stage_id::match_id`."
            )
        }
        if (length(candidates) > 1L) {
            stop(
                "Ambiguous match_id `", match_id,
                "` found in stage_id(s): ", paste(candidates, collapse = ", "),
                ". Provide `stage_id` or use `stage_id::match_id`."
            )
        }
        stage_id <- candidates[[1]]
    }

    assert_runtime_scalar_string(stage_id, arg = "stage_id")
    if (!stage_id %in% names(tournament$stage_state)) {
        stop("Unknown stage_id `", stage_id, "`")
    }

    stage_state <- tournament$stage_state[[stage_id]]
    if (!isTRUE(stage_state$materialized)) {
        stop("Stage `", stage_id, "` is not materialized")
    }

    stage_match_table <- stage_match_table(stage_state$bracket)
    if (nrow(stage_match_table) == 0L) {
        stop("Stage `", stage_id, "` has no matches")
    }

    typed_match_id <- coerce_match_id_for_stage(match_id, stage_match_table$id)
    if (!as.character(typed_match_id) %in% as.character(stage_match_table$id)) {
        stop(
            "Unknown match_id `", match_id,
            "` for stage_id `", stage_id, "`"
        )
    }

    list(stage_id = stage_id, match_id = typed_match_id)
}

parse_compound_match_id <- function(match_id) {
    if (!is.character(match_id) || length(match_id) != 1L) {
        return(NULL)
    }
    if (!grepl("::", match_id, fixed = TRUE)) {
        return(NULL)
    }

    parts <- strsplit(match_id, "::", fixed = TRUE)[[1]]
    if (length(parts) != 2L || any(parts == "")) {
        stop("Compound match_id must be in the format `stage_id::match_id`")
    }

    list(stage_id = parts[[1]], match_id = parts[[2]])
}

stage_match_table <- function(stage_bracket) {
    matches <- tryCatch(
        get_matches(stage_bracket, status = "all"),
        error = function(e) data.frame()
    )
    if (!is.data.frame(matches) || !"id" %in% names(matches)) {
        return(data.frame())
    }
    matches
}

coerce_match_id_for_stage <- function(match_id, stage_match_ids) {
    if (length(stage_match_ids) == 0L) {
        return(match_id)
    }

    ids_are_numeric <- is.numeric(stage_match_ids) || is.integer(stage_match_ids)
    if (!ids_are_numeric) {
        return(as.character(match_id))
    }

    parsed <- suppressWarnings(as.integer(as.character(match_id)))
    if (is.na(parsed)) {
        return(match_id)
    }
    parsed
}

stages_with_match_id <- function(tournament, match_id) {
    stage_ids <- names(tournament$stage_state)
    if (is.null(stage_ids) || length(stage_ids) == 0L) {
        return(character(0))
    }

    found <- stage_ids[vapply(stage_ids, function(stage_id) {
        state <- tournament$stage_state[[stage_id]]
        if (!isTRUE(state$materialized)) {
            return(FALSE)
        }
        table <- stage_match_table(state$bracket)
        if (nrow(table) == 0L) {
            return(FALSE)
        }
        as.character(match_id) %in% as.character(table$id)
    }, logical(1))]

    topological <- tournament$spec$metadata$topological_order
    if (!is.null(topological) && length(topological) > 0L) {
        found <- topological[topological %in% found]
    }
    found
}

ready_to_advance_stage_ids <- function(tournament) {
    stage_ids <- names(tournament$stage_state)
    if (is.null(stage_ids) || length(stage_ids) == 0L) {
        return(character(0))
    }

    ready <- stage_ids[vapply(stage_ids, function(stage_id) {
        state <- tournament$stage_state[[stage_id]]
        identical(state$status, "complete") &&
            !isTRUE(state$advanced) &&
            length(outgoing_transitions_for_stage(tournament$spec, stage_id)) > 0L
    }, logical(1))]

    topological <- tournament$spec$metadata$topological_order
    if (!is.null(topological) && length(topological) > 0L) {
        ready <- topological[topological %in% ready]
    }
    ready
}

outgoing_transitions_for_stage <- function(spec, stage_id) {
    transition_ids <- names(spec$edges)
    if (is.null(transition_ids) || length(transition_ids) == 0L) {
        return(list())
    }

    outgoing_ids <- transition_ids[vapply(transition_ids, function(transition_id) {
        identical(spec$edges[[transition_id]]$from_stage_id, stage_id)
    }, logical(1))]
    if (length(outgoing_ids) == 0L) {
        return(list())
    }

    priorities <- vapply(outgoing_ids, function(transition_id) {
        as.integer(spec$edges[[transition_id]]$priority)
    }, integer(1))
    outgoing_ids <- outgoing_ids[order(priorities, seq_along(outgoing_ids))]

    unname(spec$edges[outgoing_ids])
}

inbound_transitions_for_stage <- function(spec, stage_id) {
    transition_ids <- names(spec$edges)
    if (is.null(transition_ids) || length(transition_ids) == 0L) {
        return(list())
    }

    inbound_ids <- transition_ids[vapply(transition_ids, function(transition_id) {
        identical(spec$edges[[transition_id]]$to_stage_id, stage_id)
    }, logical(1))]
    if (length(inbound_ids) == 0L) {
        return(list())
    }

    priorities <- vapply(inbound_ids, function(transition_id) {
        as.integer(spec$edges[[transition_id]]$priority)
    }, integer(1))
    inbound_ids <- inbound_ids[order(priorities, seq_along(inbound_ids))]

    unname(spec$edges[inbound_ids])
}

inbound_transition_ids_for_stage <- function(spec, stage_id) {
    inbound <- inbound_transitions_for_stage(spec = spec, stage_id = stage_id)
    if (length(inbound) == 0L) {
        return(character(0))
    }
    vapply(inbound, `[[`, character(1), "transition_id")
}

register_inbound_transition_output <- function(tournament, destination_stage_id,
                                               transition_id, selected_participants) {
    state <- tournament$stage_state[[destination_stage_id]]
    if (is.null(state$pending_by_transition) || !is.list(state$pending_by_transition)) {
        state$pending_by_transition <- list()
    }
    if (is.null(state$resolved_inbound_transition_ids)) {
        state$resolved_inbound_transition_ids <- character(0)
    }

    state$pending_by_transition[[transition_id]] <- selected_participants
    if (!transition_id %in% state$resolved_inbound_transition_ids) {
        state$resolved_inbound_transition_ids <- c(
            state$resolved_inbound_transition_ids,
            transition_id
        )
    }

    tournament$stage_state[[destination_stage_id]] <- state
    tournament
}

is_stage_ready_for_materialization <- function(tournament, stage_id) {
    inbound_ids <- inbound_transition_ids_for_stage(
        spec = tournament$spec,
        stage_id = stage_id
    )
    if (length(inbound_ids) == 0L) {
        return(FALSE)
    }

    state <- tournament$stage_state[[stage_id]]
    resolved <- state$resolved_inbound_transition_ids
    if (is.null(resolved)) {
        resolved <- character(0)
    }

    all(inbound_ids %in% resolved)
}

collect_inbound_participants_for_stage <- function(tournament, stage_id) {
    state <- tournament$stage_state[[stage_id]]
    inbound_ids <- inbound_transition_ids_for_stage(
        spec = tournament$spec,
        stage_id = stage_id
    )
    if (length(inbound_ids) == 0L) {
        return(data.frame(name = character(0), stringsAsFactors = FALSE))
    }

    pending <- state$pending_by_transition
    if (is.null(pending) || !is.list(pending)) {
        pending <- list()
    }

    selected_list <- lapply(inbound_ids, function(transition_id) {
        selection <- pending[[transition_id]]
        if (is.null(selection)) {
            return(data.frame(name = character(0), stringsAsFactors = FALSE))
        }
        if (!is.data.frame(selection) || !"name" %in% names(selection)) {
            stop(
                "Invalid pending selection for stage_id `", stage_id,
                "` from transition_id `", transition_id,
                "`. Expected a data.frame with a `name` column."
            )
        }
        selection
    })
    non_empty <- Filter(function(selection) nrow(selection) > 0L, selected_list)
    if (length(non_empty) == 0L) {
        return(data.frame(name = character(0), stringsAsFactors = FALSE))
    }

    combined <- do.call(rbind, non_empty)
    rownames(combined) <- NULL

    duplicate_names <- unique(as.character(combined$name)[duplicated(as.character(combined$name))])
    if (length(duplicate_names) > 0L) {
        stop(
            "Stage_id `", stage_id,
            "` cannot materialize because inbound transitions selected overlapping participant(s): ",
            paste(duplicate_names, collapse = ", ")
        )
    }

    normalize_participants(combined)$data
}

resolve_transition_participants <- function(tournament, source_stage_id, transition,
                                            source_pool = NULL) {
    source_state <- tournament$stage_state[[source_stage_id]]
    source_bracket <- source_state$bracket
    standings <- tryCatch(get_standings(source_bracket), error = function(e) NULL)

    selection <- NULL
    if (inherits(transition$take, "bracketeer_selector")) {
        selection <- evaluate_selector(
            selector = transition$take,
            source_pool = source_pool,
            standings = standings,
            source_bracket = source_bracket,
            participants = tournament$participants
        )
    } else if (is.function(transition$rule)) {
        selection <- invoke_transition_rule(
            rule = transition$rule,
            source_bracket = source_bracket,
            standings = standings,
            participants = tournament$participants,
            source_pool = source_pool
        )
    } else {
        selection <- default_transition_selection(
            source_bracket = source_bracket,
            standings = standings,
            participants = tournament$participants
        )
    }

    selected_participants <- normalize_transition_selection(
        selection = selection,
        standings = standings,
        participant_table = tournament$participants
    )

    apply_transition_seeding_policy(
        selected_participants = selected_participants,
        transition = transition,
        standings = standings,
        source_bracket = source_bracket
    )
}

default_transition_selection <- function(source_bracket, standings, participants) {
    if (!is.null(standings) &&
        is.data.frame(standings) &&
        "participant" %in% names(standings)) {
        return(as.character(standings$participant))
    }

    if (!is.null(source_bracket$participants)) {
        return(as.character(source_bracket$participants))
    }

    as.character(participants$name)
}

invoke_transition_rule <- function(rule, source_bracket, standings,
                                   participants, source_pool = NULL) {
    formal_names <- names(formals(rule))
    args <- list(
        source_bracket = source_bracket,
        standings = standings,
        participants = participants,
        source_pool = source_pool
    )

    if (is.null(formal_names) || length(formal_names) == 0L) {
        return(rule())
    }

    if ("..." %in% formal_names) {
        return(do.call(rule, args))
    }

    matched_args <- args[names(args) %in% formal_names]
    if (length(matched_args) == 0L) {
        return(rule())
    }

    do.call(rule, matched_args)
}

normalize_transition_selection <- function(selection, standings, participant_table) {
    if (is.data.frame(selection)) {
        if (!"name" %in% names(selection)) {
            stop("Transition selection data.frame must include a `name` column")
        }
        return(normalize_participants(selection)$data)
    }

    if (is.numeric(selection)) {
        if (is.null(standings) || !is.data.frame(standings) ||
            !"participant" %in% names(standings)) {
            stop("Numeric transition selection requires standings with a `participant` column")
        }
        idx <- as.integer(selection)
        if (any(is.na(idx)) || any(idx < 1L) || any(idx > nrow(standings))) {
            stop("Numeric transition selection contains out-of-range positions")
        }
        selection <- standings$participant[idx]
    }

    if (!is.character(selection)) {
        stop("Unsupported transition selection type: ", typeof(selection))
    }

    selected_names <- as.character(selection)
    source_idx <- match(selected_names, participant_table$name)
    if (all(!is.na(source_idx))) {
        selected_df <- participant_table[source_idx, , drop = FALSE]
    } else {
        selected_df <- data.frame(name = selected_names, stringsAsFactors = FALSE)
    }

    normalize_participants(selected_df)$data
}

apply_transition_seeding_policy <- function(selected_participants, transition,
                                            standings, source_bracket) {
    if (!is.data.frame(selected_participants) || nrow(selected_participants) <= 1L) {
        return(selected_participants)
    }

    seeding <- transition$seeding
    if (is.null(seeding)) {
        seeding <- "by_source_rank"
    }
    seeding <- match.arg(
        as.character(seeding),
        c("as_is", "by_source_rank", "cross_group", "snake", "random")
    )

    if (seeding == "as_is") {
        rownames(selected_participants) <- NULL
        return(selected_participants)
    }

    if (seeding == "random") {
        randomized <- selected_participants[sample.int(nrow(selected_participants)), , drop = FALSE]
        rownames(randomized) <- NULL
        return(randomized)
    }

    ranked <- order_selected_by_source_rank(
        selected_participants = selected_participants,
        standings = standings,
        source_bracket = source_bracket
    )

    if (seeding == "snake") {
        ranked <- ranked[snake_order(nrow(ranked)), , drop = FALSE]
    }

    rownames(ranked) <- NULL
    ranked
}

order_selected_by_source_rank <- function(selected_participants, standings, source_bracket) {
    selected_names <- as.character(selected_participants$name)
    rank_lookup <- source_rank_lookup(source_bracket = source_bracket, standings = standings)
    selected_ranks <- as.integer(rank_lookup[selected_names])

    if (length(selected_ranks) > 0L) {
        missing_idx <- which(is.na(selected_ranks))
        if (length(missing_idx) > 0L) {
            fallback_start <- if (all(is.na(selected_ranks))) {
                1L
            } else {
                max(selected_ranks, na.rm = TRUE) + 1L
            }
            selected_ranks[missing_idx] <- seq.int(
                fallback_start,
                length.out = length(missing_idx)
            )
        }
    }

    ordered <- selected_participants[order(selected_ranks, selected_names), , drop = FALSE]
    rownames(ordered) <- NULL
    ordered
}

source_rank_lookup <- function(source_bracket, standings) {
    if (!is.null(standings) &&
        is.data.frame(standings) &&
        "participant" %in% names(standings)) {
        if ("rank" %in% names(standings)) {
            ranks <- as.integer(standings$rank)
        } else {
            ranks <- seq_len(nrow(standings))
        }
        return(setNames(ranks, as.character(standings$participant)))
    }

    if (!is.null(source_bracket$participants_df) &&
        is.data.frame(source_bracket$participants_df) &&
        "name" %in% names(source_bracket$participants_df)) {
        if ("seed_rank" %in% names(source_bracket$participants_df)) {
            ranks <- as.integer(source_bracket$participants_df$seed_rank)
        } else if ("seed" %in% names(source_bracket$participants_df)) {
            ranks <- as.integer(rank(source_bracket$participants_df$seed, ties.method = "first"))
        } else {
            ranks <- seq_len(nrow(source_bracket$participants_df))
        }
        return(setNames(ranks, as.character(source_bracket$participants_df$name)))
    }

    if (!is.null(source_bracket$participants)) {
        participants <- as.character(source_bracket$participants)
        return(setNames(seq_along(participants), participants))
    }

    integer(0)
}

sink_stage_ids <- function(spec) {
    stage_ids <- names(spec$stages)
    if (is.null(stage_ids) || length(stage_ids) == 0L) {
        return(character(0))
    }

    out_degree <- setNames(integer(length(stage_ids)), stage_ids)
    transition_ids <- names(spec$edges)
    if (!is.null(transition_ids) && length(transition_ids) > 0L) {
        for (transition_id in transition_ids) {
            edge <- spec$edges[[transition_id]]
            out_degree[[edge$from_stage_id]] <- out_degree[[edge$from_stage_id]] + 1L
        }
    }

    stage_ids[out_degree[stage_ids] == 0L]
}

downstream_stage_ids <- function(spec, stage_id) {
    visited <- character(0)
    frontier <- stage_id

    repeat {
        next_ids <- unlist(lapply(frontier, function(id) {
            outgoing <- outgoing_transitions_for_stage(spec = spec, stage_id = id)
            if (length(outgoing) == 0L) {
                return(character(0))
            }
            vapply(outgoing, `[[`, character(1), "to_stage_id")
        }), use.names = FALSE)
        next_ids <- setdiff(unique(next_ids), c(visited, stage_id))
        if (length(next_ids) == 0L) {
            break
        }
        visited <- c(visited, next_ids)
        frontier <- next_ids
    }

    visited
}

upstream_sources_for_teardown <- function(spec, teardown_stage_ids) {
    transition_ids <- names(spec$edges)
    if (is.null(transition_ids) || length(transition_ids) == 0L) {
        return(character(0))
    }

    unique(vapply(Filter(function(edge) {
        edge$to_stage_id %in% teardown_stage_ids
    }, spec$edges), `[[`, character(1), "from_stage_id"))
}

has_materialized_downstream_stage <- function(tournament, stage_id) {
    transitions <- outgoing_transitions_for_stage(tournament$spec, stage_id = stage_id)
    if (length(transitions) == 0L) {
        return(FALSE)
    }

    any(vapply(transitions, function(transition) {
        destination <- transition$to_stage_id
        isTRUE(tournament$stage_state[[destination]]$materialized)
    }, logical(1)))
}

has_resolved_downstream_transition_outputs <- function(tournament, stage_id) {
    transitions <- outgoing_transitions_for_stage(tournament$spec, stage_id = stage_id)
    if (length(transitions) == 0L) {
        return(FALSE)
    }

    any(vapply(transitions, function(transition) {
        destination <- transition$to_stage_id
        destination_state <- tournament$stage_state[[destination]]
        resolved <- destination_state$resolved_inbound_transition_ids
        if (is.null(resolved)) {
            return(FALSE)
        }
        transition$transition_id %in% resolved
    }, logical(1)))
}

blocking_downstream_stage_id <- function(tournament, stage_id) {
    transitions <- outgoing_transitions_for_stage(tournament$spec, stage_id = stage_id)
    if (length(transitions) == 0L) {
        return(NULL)
    }

    for (transition in transitions) {
        destination <- transition$to_stage_id
        destination_state <- tournament$stage_state[[destination]]

        if (isTRUE(destination_state$materialized)) {
            return(destination)
        }

        resolved <- destination_state$resolved_inbound_transition_ids
        if (!is.null(resolved) && transition$transition_id %in% resolved) {
            return(destination)
        }
    }

    NULL
}

filter_selection_by_pool <- function(selected_participants, source_pool,
                                     transition_id, source_stage_id) {
    selected_names <- as.character(selected_participants$name)
    unavailable <- setdiff(selected_names, source_pool)
    if (length(unavailable) > 0L) {
        stop(
            "Transition `", transition_id, "` selected participant(s) not available from stage_id `",
            source_stage_id, "`: ", paste(unavailable, collapse = ", ")
        )
    }

    selected_participants
}

detect_transition_overlap_conflicts <- function(selected_participants, transition,
                                                selected_by_participant,
                                                transition_overlap_policy,
                                                source_stage_id) {
    selected_names <- as.character(selected_participants$name)
    if (length(selected_names) == 0L) {
        return(invisible(NULL))
    }

    conflicting_names <- character(0)
    conflicting_transition_ids <- character(0)

    for (name in selected_names) {
        prior_transition_ids <- selected_by_participant[[name]]
        if (is.null(prior_transition_ids) || length(prior_transition_ids) == 0L) {
            next
        }

        for (prior_transition_id in prior_transition_ids) {
            prior_allows_overlap <- isTRUE(transition_overlap_policy[[prior_transition_id]])
            current_allows_overlap <- isTRUE(transition$allow_overlap)
            if (prior_allows_overlap && current_allows_overlap) {
                next
            }

            conflicting_names <- c(conflicting_names, name)
            conflicting_transition_ids <- c(conflicting_transition_ids, prior_transition_id)
        }
    }

    if (length(conflicting_names) == 0L) {
        return(invisible(NULL))
    }

    stop(
        "Transition overlap conflict for stage_id `", source_stage_id,
        "`: transition_id `", transition$transition_id, "` selected participant(s) ",
        paste(unique(conflicting_names), collapse = ", "),
        " already selected by transition_id(s) `",
        paste(unique(conflicting_transition_ids), collapse = ", "),
        "`. Set `allow_overlap = TRUE` on all overlapping transitions to allow this."
    )
}

register_transition_selection <- function(selected_by_participant, transition,
                                          selected_participants) {
    selected_names <- as.character(selected_participants$name)
    if (length(selected_names) == 0L) {
        return(selected_by_participant)
    }

    for (name in selected_names) {
        prior <- selected_by_participant[[name]]
        if (is.null(prior) || length(prior) == 0L) {
            selected_by_participant[[name]] <- transition$transition_id
        } else {
            selected_by_participant[[name]] <- c(prior, transition$transition_id)
        }
    }

    selected_by_participant
}

overwrite_stage_match_result <- function(stage, state, stage_id, match_id, score1, score2) {
    match_index <- find_stage_match_index(state$bracket, match_id)
    if (is.na(match_index)) {
        stop("Unknown match_id `", match_id, "` for stage_id `", stage_id, "`")
    }

    target_match <- state$bracket$matches[[match_index]]
    if (!identical(target_match$status, "complete")) {
        state$bracket <- set_result(
            state$bracket,
            match_id = match_id,
            score1 = score1,
            score2 = score2
        )
        return(state)
    }

    entrants <- stage_entrants_for_overwrite(state, stage_id)
    rebuilt_bracket <- materialize_stage(
        stage = stage,
        participants = entrants,
        stage_id = stage_id
    )

    completed_results <- collect_completed_stage_results(state$bracket)
    target_seen <- FALSE

    for (entry in completed_results) {
        replay_match_id <- entry$match_id
        replay_score1 <- entry$score1
        replay_score2 <- entry$score2

        if (identical(as.character(replay_match_id), as.character(match_id))) {
            replay_score1 <- score1
            replay_score2 <- score2
            target_seen <- TRUE
        } else if (!is_replayable_stage_result(entry)) {
            next
        }

        rebuilt_bracket <- tryCatch(
            set_result(
                rebuilt_bracket,
                match_id = replay_match_id,
                score1 = replay_score1,
                score2 = replay_score2
            ),
            error = function(err) {
                stop(
                    "Cannot overwrite result for stage_id `", stage_id,
                    "` at match_id `", match_id,
                    "` because existing stage results would become inconsistent: ",
                    conditionMessage(err)
                )
            }
        )
    }

    if (!isTRUE(target_seen)) {
        rebuilt_bracket <- tryCatch(
            set_result(
                rebuilt_bracket,
                match_id = match_id,
                score1 = score1,
                score2 = score2
            ),
            error = function(err) {
                stop(
                    "Cannot overwrite result for stage_id `", stage_id,
                    "` at match_id `", match_id, "`: ",
                    conditionMessage(err)
                )
            }
        )
    }

    state$bracket <- rebuilt_bracket
    state
}

find_stage_match_index <- function(stage_bracket, match_id) {
    if (length(stage_bracket$matches) == 0L) {
        return(NA_integer_)
    }

    match_ids <- vapply(stage_bracket$matches, function(match) {
        as.character(match$id)
    }, character(1))

    idx <- which(match_ids == as.character(match_id))
    if (length(idx) != 1L) {
        return(NA_integer_)
    }
    as.integer(idx[[1]])
}

stage_entrants_for_overwrite <- function(state, stage_id) {
    if (!is.null(state$bracket$participants_df) &&
        is.data.frame(state$bracket$participants_df) &&
        "name" %in% names(state$bracket$participants_df)) {
        return(state$bracket$participants_df)
    }

    if (!is.null(state$bracket$participants)) {
        return(data.frame(
            name = as.character(state$bracket$participants),
            stringsAsFactors = FALSE
        ))
    }

    if (!is.null(state$participants) && length(state$participants) > 0L) {
        return(data.frame(
            name = as.character(state$participants),
            stringsAsFactors = FALSE
        ))
    }

    stop("Cannot determine stage entrants for stage_id `", stage_id, "`")
}

collect_completed_stage_results <- function(stage_bracket) {
    if (length(stage_bracket$matches) == 0L) {
        return(list())
    }

    completed <- Filter(function(match) {
        identical(match$status, "complete")
    }, stage_bracket$matches)

    if (length(completed) == 0L) {
        return(list())
    }

    ordering <- order(
        vapply(completed, `[[`, integer(1), "round"),
        vapply(completed, `[[`, integer(1), "position"),
        vapply(completed, `[[`, integer(1), "id")
    )
    completed <- completed[ordering]

    lapply(completed, function(match) {
        list(
            match_id = match$id,
            score1 = match$score1,
            score2 = match$score2
        )
    })
}

append_routing_log_entry <- function(tournament, source_stage_id, transition,
                                     selected_participants, pool_before, pool_after) {
    selected_names <- character(0)
    if (is.data.frame(selected_participants) && "name" %in% names(selected_participants)) {
        selected_names <- as.character(selected_participants$name)
    }

    entry <- list(
        source_stage_id = source_stage_id,
        transition_id = transition$transition_id,
        rule_applied = describe_transition_rule(
            rule = transition$rule,
            take = transition$take
        ),
        selected = selected_names,
        pool_before = as.integer(pool_before),
        pool_after = as.integer(pool_after),
        timestamp = Sys.time()
    )

    if (is.null(tournament$routing_log) || !is.list(tournament$routing_log)) {
        tournament$routing_log <- list()
    }
    tournament$routing_log[[length(tournament$routing_log) + 1L]] <- entry
    tournament
}

describe_transition_rule <- function(rule = NULL, take = NULL) {
    if (inherits(take, "bracketeer_selector")) {
        return(describe_selector(take))
    }

    if (is.null(rule)) {
        return("default_transition_selection")
    }

    if (is.function(rule)) {
        text <- paste(deparse(rule), collapse = " ")
        text <- gsub("[[:space:]]+", " ", text)
        return(trimws(text))
    }

    as.character(rule)[[1]]
}

is_replayable_stage_result <- function(entry) {
    is_replayable_score(entry$score1) && is_replayable_score(entry$score2)
}

is_replayable_score <- function(score) {
    is.numeric(score) && length(score) > 0L && all(!is.na(score))
}

assert_runtime_scalar_string <- function(value, arg) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || value == "") {
        stop("`", arg, "` must be a non-empty string")
    }
}
