#' Dry-run preflight validation for tournament flow feasibility
#'
#' Validates a `tournament_spec` against a participant count without running a
#' live tournament. This preflight catches infeasible routing paths and stage
#' size mismatches early.
#'
#' @param spec A `tournament_spec` object.
#' @param n_participants Positive integer participant count.
#'
#' @return A `tournament_validation` summary list.
validate_tournament <- function(spec, n_participants) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`validate_tournament()` requires a tournament_spec object")
    }

    spec <- validate_tournament_spec(spec)
    n_participants <- normalize_preflight_participant_count(n_participants)

    participant_table <- make_preflight_participant_table(n_participants)

    stage_ids <- names(spec$stages)
    if (is.null(stage_ids)) {
        stage_ids <- character(0)
    }
    if (length(stage_ids) == 0L) {
        return(structure(
            list(
                ok = TRUE,
                n_participants = n_participants,
                stage_counts = setNames(integer(0), character(0))
            ),
            class = "tournament_validation"
        ))
    }

    topological_order <- spec$metadata$topological_order
    if (is.null(topological_order) || length(topological_order) == 0L) {
        topological_order <- stage_ids
    }

    source_stage_ids <- spec$metadata$source_stage_ids
    if (is.null(source_stage_ids)) {
        source_stage_ids <- character(0)
    }

    stage_participants <- setNames(vector("list", length(stage_ids)), stage_ids)
    pending_by_stage <- setNames(vector("list", length(stage_ids)), stage_ids)
    for (stage_id in stage_ids) {
        pending_by_stage[[stage_id]] <- list()
    }

    for (stage_id in source_stage_ids) {
        participant_names <- as.character(participant_table$name)
        stage_participants[[stage_id]] <- participant_names
        validate_preflight_stage_inputs(
            stage = spec$stages[[stage_id]],
            stage_id = stage_id,
            participant_names = participant_names,
            inbound_transition_ids = character(0)
        )
    }

    for (stage_id in topological_order) {
        if (!stage_id %in% source_stage_ids) {
            inbound_transition_ids <- inbound_transition_ids_for_stage(spec = spec, stage_id = stage_id)
            participant_names <- collect_preflight_inbound_participants(
                pending_by_stage = pending_by_stage,
                stage_id = stage_id,
                inbound_transition_ids = inbound_transition_ids
            )
            stage_participants[[stage_id]] <- participant_names
            validate_preflight_stage_inputs(
                stage = spec$stages[[stage_id]],
                stage_id = stage_id,
                participant_names = participant_names,
                inbound_transition_ids = inbound_transition_ids
            )
        }

        source_pool <- stage_participants[[stage_id]]
        if (is.null(source_pool)) {
            source_pool <- character(0)
        }

        outgoing <- outgoing_transitions_for_stage(spec = spec, stage_id = stage_id)
        if (length(outgoing) == 0L) {
            next
        }

        for (transition in outgoing) {
            selected_names <- resolve_preflight_transition_selection(
                transition = transition,
                source_stage_id = stage_id,
                source_pool = source_pool,
                participant_table = participant_table,
                source_stage = spec$stages[[stage_id]]
            )

            if (isTRUE(transition$consume)) {
                unavailable <- setdiff(selected_names, source_pool)
                if (length(unavailable) > 0L) {
                    stop(
                        "Transition `", transition$transition_id,
                        "` from stage_id `", stage_id,
                        "` selected participant(s) not available in the dry-run source pool: ",
                        paste(unavailable, collapse = ", "),
                        ". Hint: review transition ordering (`priority`) and `consume` semantics."
                    )
                }
                source_pool <- source_pool[!source_pool %in% selected_names]
            }

            destination_stage_id <- transition$to_stage_id
            pending <- pending_by_stage[[destination_stage_id]]
            pending[[transition$transition_id]] <- selected_names
            pending_by_stage[[destination_stage_id]] <- pending
        }
    }

    stage_counts <- vapply(stage_participants, function(participant_names) {
        length(participant_names)
    }, integer(1))

    structure(
        list(
            ok = TRUE,
            n_participants = n_participants,
            stage_counts = stage_counts
        ),
        class = "tournament_validation"
    )
}

normalize_preflight_participant_count <- function(n_participants) {
    if (!is.numeric(n_participants) || length(n_participants) != 1L ||
        is.na(n_participants) || n_participants < 2 || n_participants %% 1 != 0) {
        stop("`n_participants` must be an integer >= 2")
    }

    as.integer(n_participants)
}

make_preflight_participant_table <- function(n_participants) {
    participants <- data.frame(
        name = paste0("P", seq_len(n_participants)),
        seed = seq_len(n_participants),
        stringsAsFactors = FALSE
    )

    normalize_participants(participants)$data
}

collect_preflight_inbound_participants <- function(pending_by_stage, stage_id,
                                                   inbound_transition_ids) {
    if (length(inbound_transition_ids) == 0L) {
        return(character(0))
    }

    pending <- pending_by_stage[[stage_id]]
    if (is.null(pending) || !is.list(pending)) {
        pending <- list()
    }

    selected <- unlist(lapply(inbound_transition_ids, function(transition_id) {
        values <- pending[[transition_id]]
        if (is.null(values)) {
            return(character(0))
        }
        as.character(values)
    }), use.names = FALSE)

    duplicate_names <- unique(selected[duplicated(selected)])
    if (length(duplicate_names) > 0L) {
        stop(
            "stage_id `", stage_id,
            "` cannot materialize in dry-run because inbound transition selections overlap: ",
            paste(duplicate_names, collapse = ", "),
            ". Hint: avoid overlapping qualifier sets feeding the same destination stage."
        )
    }

    selected
}

validate_preflight_stage_inputs <- function(stage, stage_id, participant_names,
                                            inbound_transition_ids) {
    participant_names <- as.character(participant_names)
    selected_count <- length(participant_names)
    min_required <- minimum_stage_participants(stage)

    if (selected_count < min_required) {
        if (length(inbound_transition_ids) > 0L) {
            stop(
                "stage_id `", stage_id,
                "` selected ", selected_count,
                " participant(s) from transition_id(s) `",
                paste(inbound_transition_ids, collapse = ", "),
                "` but requires at least ", min_required, ". ",
                preflight_hint_for_min_participants(stage)
            )
        }

        stop(
            "stage_id `", stage_id,
            "` requires at least ", min_required,
            " participant(s), but dry-run received ", selected_count,
            ". ", preflight_hint_for_min_participants(stage)
        )
    }

    participants_df <- data.frame(
        name = participant_names,
        seed = seq_along(participant_names),
        stringsAsFactors = FALSE
    )

    tryCatch(
        materialize_stage(stage = stage, participants = participants_df, stage_id = stage_id),
        error = function(err) {
            stop(
                "Dry-run failed for stage_id `", stage_id,
                "`: ", conditionMessage(err),
                ". ", preflight_hint_for_stage_error(stage, conditionMessage(err))
            )
        }
    )

    invisible(NULL)
}

minimum_stage_participants <- function(stage) {
    if (inherits(stage, "group_stage_knockout_stage")) {
        groups <- stage$params$groups
        advance_per_group <- stage$params$advance_per_group

        if (is.numeric(groups) && length(groups) == 1L && !is.na(groups) &&
            groups >= 1 && groups %% 1 == 0 &&
            is.numeric(advance_per_group) && length(advance_per_group) == 1L &&
            !is.na(advance_per_group) && advance_per_group >= 1 &&
            advance_per_group %% 1 == 0) {
            return(max(2L, as.integer(groups) * as.integer(advance_per_group)))
        }
    }

    2L
}

preflight_hint_for_min_participants <- function(stage) {
    if (inherits(stage, "group_stage_knockout_stage")) {
        return("Hint: increase `n_participants` or reduce `groups`/`advance_per_group`.")
    }

    "Hint: increase qualifiers feeding this stage or choose a format with lower entrant requirements."
}

preflight_hint_for_stage_error <- function(stage, error_message) {
    if (inherits(stage, "group_stage_knockout_stage") &&
        grepl("groups cannot exceed number of participants", error_message, fixed = TRUE)) {
        return("Hint: reduce `groups` or increase `n_participants`.")
    }
    if (inherits(stage, "group_stage_knockout_stage") &&
        grepl("fewer participants than advance_per_group", error_message, fixed = TRUE)) {
        return("Hint: increase qualifiers or lower `advance_per_group`.")
    }
    if (grepl("Need at least 2 participants", error_message, fixed = TRUE)) {
        return("Hint: ensure qualifying transitions feed at least two entrants.")
    }

    "Hint: review stage parameters and transition qualifier counts."
}

resolve_preflight_transition_selection <- function(transition, source_stage_id,
                                                   source_pool, participant_table,
                                                   source_stage = NULL) {
    source_pool <- as.character(source_pool)
    mock_standings <- data.frame(
        rank = seq_along(source_pool),
        participant = source_pool,
        stringsAsFactors = FALSE
    )
    if (inherits(source_stage, "round_robin_stage") &&
        !is.null(source_stage$params$groups)) {
        seed_idx <- match(source_pool, participant_table$name)
        seed_values <- participant_table$seed[seed_idx]
        seed_values[is.na(seed_values)] <- seq_len(sum(is.na(seed_values)))
        grouped_df <- assign_groups(
            data.frame(
                name = source_pool,
                seed = seed_values,
                stringsAsFactors = FALSE
            ),
            groups = source_stage$params$groups
        )
        mock_standings$group <- grouped_df$group[match(mock_standings$participant, grouped_df$name)]
    }
    mock_bracket <- list(
        participants = source_pool,
        participants_df = data.frame(
            name = source_pool,
            seed = seq_along(source_pool),
            seed_rank = seq_along(source_pool),
            stringsAsFactors = FALSE
        ),
        matches = list()
    )

    validate_selector_feasibility_preflight(
        selector = transition$take,
        transition = transition,
        source_stage_id = source_stage_id,
        standings = mock_standings
    )

    selection <- NULL
    if (inherits(transition$take, "bracketeer_selector")) {
        selection <- tryCatch(
            evaluate_selector(
                selector = transition$take,
                source_pool = source_pool,
                standings = mock_standings,
                source_bracket = mock_bracket,
                participants = participant_table
            ),
            error = function(err) {
                stop(
                    "transition_id `", transition$transition_id,
                    "` from stage_id `", source_stage_id,
                    "` failed dry-run selector evaluation: ", conditionMessage(err),
                    ". Hint: make selector parameters feasible for source-stage outputs."
                )
            }
        )
    } else if (is.function(transition$rule)) {
        selection <- tryCatch(
            invoke_transition_rule(
                rule = transition$rule,
                source_bracket = mock_bracket,
                standings = mock_standings,
                participants = participant_table,
                source_pool = source_pool
            ),
            error = function(err) {
                stop(
                    "transition_id `", transition$transition_id,
                    "` from stage_id `", source_stage_id,
                    "` failed dry-run rule evaluation: ", conditionMessage(err),
                    ". Hint: make the rule robust to preflight inputs or use a deterministic selector."
                )
            }
        )
    } else {
        selection <- default_transition_selection(
            source_bracket = mock_bracket,
            standings = mock_standings,
            participants = participant_table
        )
    }

    selected_df <- tryCatch(
        normalize_transition_selection(
            selection = selection,
            standings = mock_standings,
            participant_table = participant_table
        ),
        error = function(err) {
            stop(
                "transition_id `", transition$transition_id,
                "` from stage_id `", source_stage_id,
                "` produced an invalid selection in dry-run: ",
                conditionMessage(err),
                ". Hint: return participant names, standings positions, or a data frame with `name`."
            )
        }
    )

    selected_names <- as.character(selected_df$name)
    duplicate_names <- unique(selected_names[duplicated(selected_names)])
    if (length(duplicate_names) > 0L) {
        stop(
            "transition_id `", transition$transition_id,
            "` from stage_id `", source_stage_id,
            "` selected duplicate participant(s): ",
            paste(duplicate_names, collapse = ", "),
            ". Hint: return each participant at most once per transition."
        )
    }

    selected_names
}

validate_selector_feasibility_preflight <- function(selector, transition, source_stage_id, standings) {
    if (!inherits(selector, "bracketeer_selector")) {
        return(invisible(NULL))
    }

    kind <- selector$kind
    params <- selector$params
    n_available <- nrow(standings)

    if (kind %in% c("top_n", "bottom_n")) {
        requested <- as.integer(params$n)
        if (!is.na(requested) && requested > n_available) {
            stop(
                "transition_id `", transition$transition_id,
                "` from stage_id `", source_stage_id,
                "` requests ", kind, "(", requested, ") but only ",
                n_available, " participant(s) are available in dry-run."
            )
        }
    }

    if (kind == "slice_range") {
        requested_to <- as.integer(params$to)
        if (!is.na(requested_to) && requested_to > n_available) {
            stop(
                "transition_id `", transition$transition_id,
                "` from stage_id `", source_stage_id,
                "` requests slice_range(..., ", requested_to,
                ") but only ", n_available, " participant(s) are available in dry-run."
            )
        }
    }

    invisible(NULL)
}
