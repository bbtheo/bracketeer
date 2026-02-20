#' Create a tournament specification graph
#'
#' Construct a multi-stage tournament specification object.
#'
#' @return A `tournament_spec` object.
tournament_spec <- function() {
    structure(
        list(
            stages = list(),
            edges = list(),
            outcome = list(track_placements = 1L),
            metadata = list(
                version = 1L,
                stage_order = character(0),
                source_stage_ids = character(0),
                topological_order = character(0)
            )
        ),
        class = "tournament_spec"
    )
}

#' Add a stage to a tournament specification
#'
#' @param spec A `tournament_spec` object.
#' @param stage_id Unique stage identifier.
#' @param stage Stage definition object.
#'
#' @return Updated `tournament_spec`.
add_stage <- function(spec, stage_id, stage) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`add_stage()` requires a tournament_spec object")
    }

    assert_scalar_string(stage_id, arg = "stage_id")
    assert_no_reserved_separator(stage_id, arg = "stage_id")

    if (missing(stage) || is.null(stage)) {
        stop("`stage` is required")
    }
    if (stage_id %in% names(spec$stages)) {
        stop("Stage already exists for stage_id: ", stage_id)
    }
    if (!inherits(stage, "stage_spec") || !is.list(stage) || !is.function(stage$builder)) {
        stop(
            "`stage` for stage_id `", stage_id,
            "` must be a `stage_spec` with a `builder(participants)` function"
        )
    }

    spec$stages[[stage_id]] <- stage
    spec$metadata$stage_order <- c(spec$metadata$stage_order, stage_id)
    validate_tournament_spec(spec)
}

#' Add a transition between stages
#'
#' @param spec A `tournament_spec` object.
#' @param from Source stage ID, or `from_previous()`.
#' @param to Destination stage ID.
#' @param rule Transition rule object (optional for MVP graph wiring).
#' @param take Selector object for transition participant selection.
#' @param seeding Seeding policy label.
#' @param priority Transition resolution priority.
#' @param consume Whether selected participants are consumed.
#' @param allow_overlap Whether overlap is allowed across transitions.
#' @param transition_id Transition ID. If `NULL`, deterministic auto-ID is used.
#'
#' @return Updated `tournament_spec`.
add_transition <- function(spec, from, to, rule = NULL, seeding = "by_source_rank",
                           take = NULL,
                           priority = 1L, consume = TRUE, allow_overlap = FALSE,
                           transition_id = NULL) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`add_transition()` requires a tournament_spec object")
    }

    register_edge(
        spec = spec,
        from = from,
        to = to,
        rule = rule,
        take = take,
        seeding = seeding,
        priority = priority,
        consume = consume,
        allow_overlap = allow_overlap,
        transition_id = transition_id
    )
}

register_edge <- function(spec, from, to, rule = NULL, take = NULL,
                          seeding = "by_source_rank",
                          priority = 1L, consume = TRUE, allow_overlap = FALSE,
                          transition_id = NULL) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`spec` must be a tournament_spec object")
    }

    assert_scalar_string(to, arg = "to")
    assert_no_reserved_separator(to, arg = "to")

    from_stage_id <- resolve_from_stage(spec, from = from, to = to)

    if (!from_stage_id %in% names(spec$stages)) {
        stop("Unknown source stage_id: ", from_stage_id)
    }
    if (!to %in% names(spec$stages)) {
        stop("Unknown destination stage_id: ", to)
    }

    if (!is.numeric(priority) || length(priority) != 1L ||
        is.na(priority) || priority < 1 || priority %% 1 != 0) {
        stop("`priority` must be a positive integer")
    }
    if (!is.logical(consume) || length(consume) != 1L || is.na(consume)) {
        stop("`consume` must be TRUE or FALSE")
    }
    if (!is.logical(allow_overlap) || length(allow_overlap) != 1L || is.na(allow_overlap)) {
        stop("`allow_overlap` must be TRUE or FALSE")
    }
    if (!is.null(seeding)) {
        seeding <- normalize_transition_seeding_policy(seeding, arg = "seeding")
    }

    if (!is.null(take) &&
        !inherits(take, "bracketeer_selector") &&
        !is.function(take)) {
        stop("`take` must be NULL, a function, or a bracketeer_selector")
    }
    take_selector <- NULL
    if (!is.null(take)) {
        take_selector <- if (inherits(take, "bracketeer_selector")) take else filter_by(take)
    } else if (is.null(rule)) {
        take_selector <- all_selector()
    }

    next_transition_id <- resolve_transition_id(
        existing_ids = names(spec$edges),
        from_stage_id = from_stage_id,
        to_stage_id = to,
        transition_id = transition_id
    )

    spec$edges[[next_transition_id]] <- structure(
        list(
            transition_id = next_transition_id,
            from_stage_id = from_stage_id,
            to_stage_id = to,
            rule = rule,
            take = take_selector,
            seeding = seeding,
            priority = as.integer(priority),
            consume = consume,
            allow_overlap = allow_overlap
        ),
        class = "transition_spec"
    )

    validate_tournament_spec(spec)
}

#' Add multiple transitions from one source stage
#'
#' Convenience sugar for branching stage fan-out. Compiles into deterministic
#' `add_transition()` calls.
#'
#' @param spec A `tournament_spec` object.
#' @param from Source stage ID, or `from_previous()`.
#' @param into Named list mapping destination stage IDs to transition rules, or
#'   branch configs with a required `rule` field.
#' @param priority_start Starting priority for branch transitions when a branch
#'   does not explicitly provide `priority`.
#' @param consume Default `consume` value for branches.
#' @param allow_overlap Default `allow_overlap` value for branches.
#' @param seeding Default seeding policy for branches.
#'
#' @return Updated `tournament_spec`.
split_stage <- function(spec, from, into, priority_start = 1L,
                        consume = TRUE, allow_overlap = FALSE,
                        seeding = "by_source_rank") {
    if (!inherits(spec, "tournament_spec")) {
        stop("`split_stage()` requires a tournament_spec object")
    }

    if (!is.numeric(priority_start) || length(priority_start) != 1L ||
        is.na(priority_start) || priority_start < 1 || priority_start %% 1 != 0) {
        stop("`priority_start` must be a positive integer")
    }
    if (!is.logical(consume) || length(consume) != 1L || is.na(consume)) {
        stop("`consume` must be TRUE or FALSE")
    }
    if (!is.logical(allow_overlap) || length(allow_overlap) != 1L || is.na(allow_overlap)) {
        stop("`allow_overlap` must be TRUE or FALSE")
    }
    seeding <- normalize_transition_seeding_policy(seeding, arg = "seeding")

    if (!is.list(into) || length(into) == 0L) {
        stop("`into` must be a non-empty named list")
    }
    destination_ids <- names(into)
    if (is.null(destination_ids) || any(is.na(destination_ids)) || any(destination_ids == "")) {
        stop("`into` must be a named list keyed by destination stage_id")
    }
    if (anyDuplicated(destination_ids)) {
        stop("`into` destination stage_id values must be unique")
    }

    next_priority <- as.integer(priority_start)
    updated_spec <- spec

    for (destination_stage_id in destination_ids) {
        branch <- into[[destination_stage_id]]
        branch_rule <- NULL
        branch_seeding <- seeding
        branch_priority <- next_priority
        branch_consume <- consume
        branch_allow_overlap <- allow_overlap
        branch_transition_id <- NULL

        if (is.function(branch) || is.null(branch)) {
            branch_rule <- branch
        } else if (is.list(branch)) {
            if (!"rule" %in% names(branch)) {
                stop(
                    "Branch config for destination stage_id `", destination_stage_id,
                    "` must include a `rule` field"
                )
            }
            branch_rule <- branch$rule
            if ("seeding" %in% names(branch)) {
                branch_seeding <- branch$seeding
            }
            if ("priority" %in% names(branch)) {
                branch_priority <- branch$priority
            }
            if ("consume" %in% names(branch)) {
                branch_consume <- branch$consume
            }
            if ("allow_overlap" %in% names(branch)) {
                branch_allow_overlap <- branch$allow_overlap
            }
            if ("transition_id" %in% names(branch)) {
                branch_transition_id <- branch$transition_id
            }
        } else {
            stop(
                "Branch value for destination stage_id `", destination_stage_id,
                "` must be a rule function or a list containing `rule`"
            )
        }

        updated_spec <- add_transition(
            updated_spec,
            from = from,
            to = destination_stage_id,
            rule = branch_rule,
            seeding = branch_seeding,
            priority = branch_priority,
            consume = branch_consume,
            allow_overlap = branch_allow_overlap,
            transition_id = branch_transition_id
        )
        next_priority <- next_priority + 1L
    }

    updated_spec
}

#' Validate a tournament specification
#'
#' @param spec A `tournament_spec` object.
#'
#' @return The validated `tournament_spec`.
validate_tournament_spec <- function(spec) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`validate_tournament_spec()` requires a tournament_spec object")
    }

    stage_ids <- names(spec$stages)
    if (is.null(stage_ids)) {
        stage_ids <- character(0)
    }
    if (anyDuplicated(stage_ids)) {
        stop("Duplicate stage_id values found")
    }
    for (stage_id in stage_ids) {
        assert_no_reserved_separator(stage_id, arg = "stage_id")
    }

    transition_ids <- names(spec$edges)
    if (is.null(transition_ids)) {
        transition_ids <- character(0)
    }
    if (anyDuplicated(transition_ids)) {
        stop("Duplicate transition_id values found")
    }

    for (transition_id in transition_ids) {
        edge <- spec$edges[[transition_id]]
        if (!is.list(edge)) {
            stop("Transition `", transition_id, "` must be a list")
        }
        edge <- validate_transition_metadata(edge, transition_id = transition_id)
        spec$edges[[transition_id]] <- edge

        from_stage_id <- edge$from_stage_id
        to_stage_id <- edge$to_stage_id
        if (!from_stage_id %in% stage_ids) {
            stop("Transition `", transition_id, "` references unknown source stage_id: ", from_stage_id)
        }
        if (!to_stage_id %in% stage_ids) {
            stop("Transition `", transition_id, "` references unknown destination stage_id: ", to_stage_id)
        }
    }

    topo <- kahn_topological_sort(stage_ids = stage_ids, edges = spec$edges)
    spec$metadata$source_stage_ids <- topo$sources
    spec$metadata$topological_order <- topo$order
    spec
}

#' Configure tournament outcome depth
#'
#' @param spec A `tournament_spec` object.
#' @param track_placements Number of placements to track.
#'
#' @return Updated `tournament_spec`.
set_outcome <- function(spec, track_placements = 1L) {
    if (!inherits(spec, "tournament_spec")) {
        stop("`set_outcome()` requires a tournament_spec object")
    }

    if (!is.numeric(track_placements) || length(track_placements) != 1L ||
        is.na(track_placements) || track_placements < 1 ||
        track_placements %% 1 != 0) {
        stop("`track_placements` must be a positive integer")
    }

    spec$outcome$track_placements <- as.integer(track_placements)
    spec
}

#' Resolve source stage from most recently defined stage order
#'
#' @return Sentinel object to be resolved by `add_transition()`.
from_previous <- function() {
    structure(list(), class = "from_previous_stage")
}

#' Resolve source stage from the immediately preceding stage
#'
#' Alias for `from_previous()` used by the rewritten stage-verb API.
#'
#' @return Sentinel object to be resolved by transition wiring.
#' @examples
#' teams <- paste("Team", LETTERS[1:8])
#'
#' # Implicit: defaults to previous_stage()
#' trn <- tournament(teams) |>
#'   swiss("open", rounds = 3) |>
#'   single_elim("playoffs", take = top_n(4))
#'
#' # Explicit: useful for branching
#' trn <- tournament(teams) |>
#'   round_robin("groups") |>
#'   single_elim("finals", from = previous_stage(), take = top_n(2))
#' @export
previous_stage <- function() {
    from_previous()
}

#' @export
print.tournament_spec <- function(x, ...) {
    stage_ids <- names(x$stages)
    if (is.null(stage_ids)) {
        stage_ids <- character(0)
    }
    transition_ids <- names(x$edges)
    if (is.null(transition_ids)) {
        transition_ids <- character(0)
    }

    cat("Tournament Spec\n")
    cat("Stages: ", length(stage_ids), "\n", sep = "")
    cat("Transitions: ", length(transition_ids), "\n", sep = "")

    if (length(stage_ids) == 0L) {
        cat("Graph: <empty>\n")
        return(invisible(x))
    }

    cat("Graph:\n")
    if (length(transition_ids) == 0L) {
        for (stage_id in stage_ids) {
            cat("[ ", stage_id, " ]\n", sep = "")
        }
        return(invisible(x))
    }

    for (transition_id in transition_ids) {
        edge <- x$edges[[transition_id]]
        cat("[ ", edge$from_stage_id, " ] ---> [ ", edge$to_stage_id, " ]\n", sep = "")
    }

    invisible(x)
}

assert_scalar_string <- function(value, arg) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || value == "") {
        stop("`", arg, "` must be a non-empty string")
    }
}

assert_no_reserved_separator <- function(value, arg) {
    if (grepl("::", value, fixed = TRUE)) {
        stop("`", arg, "` cannot contain reserved separator `::`")
    }
}

resolve_from_stage <- function(spec, from, to) {
    if (inherits(from, "from_previous_stage")) {
        stage_order <- spec$metadata$stage_order
        if (length(stage_order) < 2L) {
            stop("Cannot resolve from_previous() for destination stage_id `", to, "`")
        }
        to_index <- match(to, stage_order)
        if (is.na(to_index) || to_index <= 1L) {
            stop("Cannot resolve from_previous() for destination stage_id `", to, "`")
        }
        return(stage_order[[to_index - 1L]])
    }

    assert_scalar_string(from, arg = "from")
    assert_no_reserved_separator(from, arg = "from")
    from
}

resolve_transition_id <- function(existing_ids, from_stage_id, to_stage_id, transition_id = NULL) {
    if (is.null(existing_ids)) {
        existing_ids <- character(0)
    }

    if (!is.null(transition_id)) {
        assert_scalar_string(transition_id, arg = "transition_id")
        if (transition_id %in% existing_ids) {
            stop("Transition already exists for transition_id: ", transition_id)
        }
        return(transition_id)
    }

    base_id <- paste0(from_stage_id, "_to_", to_stage_id)
    if (!base_id %in% existing_ids) {
        return(base_id)
    }

    idx <- 2L
    repeat {
        candidate <- paste0(base_id, "_", idx)
        if (!candidate %in% existing_ids) {
            return(candidate)
        }
        idx <- idx + 1L
    }
}

kahn_topological_sort <- function(stage_ids, edges) {
    if (length(stage_ids) == 0L) {
        return(list(order = character(0), sources = character(0)))
    }

    incoming <- setNames(integer(length(stage_ids)), stage_ids)
    adjacency <- setNames(vector("list", length(stage_ids)), stage_ids)
    for (id in stage_ids) {
        adjacency[[id]] <- character(0)
    }

    transition_ids <- names(edges)
    if (is.null(transition_ids)) {
        transition_ids <- character(0)
    }

    for (transition_id in transition_ids) {
        edge <- edges[[transition_id]]
        from <- edge$from_stage_id
        to <- edge$to_stage_id
        adjacency[[from]] <- c(adjacency[[from]], to)
        incoming[[to]] <- incoming[[to]] + 1L
    }

    sources <- stage_ids[incoming[stage_ids] == 0L]
    queue <- sources
    order <- character(0)

    while (length(queue) > 0L) {
        current <- queue[[1]]
        queue <- queue[-1]
        order <- c(order, current)

        for (neighbor in adjacency[[current]]) {
            incoming[[neighbor]] <- incoming[[neighbor]] - 1L
            if (incoming[[neighbor]] == 0L) {
                queue <- c(queue, neighbor)
                queue <- stage_ids[stage_ids %in% queue]
            }
        }
    }

    if (length(order) != length(stage_ids)) {
        cycle_nodes <- stage_ids[!stage_ids %in% order]
        stop("Cycle detected in tournament_spec involving stage_id(s): ",
            paste(cycle_nodes, collapse = ", "))
    }

    list(order = order, sources = sources)
}

validate_transition_metadata <- function(edge, transition_id) {
    required_fields <- c(
        "transition_id", "from_stage_id", "to_stage_id",
        "rule", "priority", "consume"
    )
    missing_fields <- required_fields[!required_fields %in% names(edge)]
    if (length(missing_fields) > 0L) {
        stop(
            "Transition `", transition_id,
            "` is missing required field(s): ",
            paste(missing_fields, collapse = ", ")
        )
    }

    if (!is.character(edge$transition_id) || length(edge$transition_id) != 1L ||
        is.na(edge$transition_id) || edge$transition_id == "") {
        stop("Transition `", transition_id, "` has invalid `transition_id` metadata")
    }
    if (!identical(edge$transition_id, transition_id)) {
        stop(
            "Transition `", transition_id,
            "` has transition_id metadata `", edge$transition_id,
            "`; transition_id metadata must match list key"
        )
    }

    if (!is.character(edge$from_stage_id) || length(edge$from_stage_id) != 1L ||
        is.na(edge$from_stage_id) || edge$from_stage_id == "") {
        stop("Transition `", transition_id, "` has invalid `from_stage_id`")
    }
    assert_no_reserved_separator(edge$from_stage_id, arg = "from_stage_id")

    if (!is.character(edge$to_stage_id) || length(edge$to_stage_id) != 1L ||
        is.na(edge$to_stage_id) || edge$to_stage_id == "") {
        stop("Transition `", transition_id, "` has invalid `to_stage_id`")
    }
    assert_no_reserved_separator(edge$to_stage_id, arg = "to_stage_id")

    if (!is.numeric(edge$priority) || length(edge$priority) != 1L ||
        is.na(edge$priority) || edge$priority < 1 || edge$priority %% 1 != 0) {
        stop("Transition `", transition_id, "` has invalid `priority`; must be a positive integer")
    }
    edge$priority <- as.integer(edge$priority)

    if (!is.logical(edge$consume) || length(edge$consume) != 1L || is.na(edge$consume)) {
        stop("Transition `", transition_id, "` has invalid `consume`; must be TRUE or FALSE")
    }

    if (!"allow_overlap" %in% names(edge)) {
        edge$allow_overlap <- FALSE
    } else if (!is.logical(edge$allow_overlap) || length(edge$allow_overlap) != 1L ||
        is.na(edge$allow_overlap)) {
        stop("Transition `", transition_id, "` has invalid `allow_overlap`; must be TRUE or FALSE")
    }

    if (!"seeding" %in% names(edge)) {
        edge$seeding <- "by_source_rank"
    } else if (!is.null(edge$seeding)) {
        edge$seeding <- tryCatch(
            normalize_transition_seeding_policy(edge$seeding, arg = "seeding"),
            error = function(err) {
                stop(
                    "Transition `", transition_id,
                    "` has invalid `seeding` metadata: ",
                    conditionMessage(err)
                )
            }
        )
    }

    edge
}

normalize_transition_seeding_policy <- function(value, arg = "seeding") {
    assert_scalar_string(value, arg = arg)
    allowed <- c("as_is", "by_source_rank", "cross_group", "snake", "random")
    if (!value %in% allowed) {
        stop("`", arg, "` must be one of: ", paste(allowed, collapse = ", "))
    }
    value
}
