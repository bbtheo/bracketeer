as_stage_graph <- function(x) {
    if (inherits(x, "tournament")) {
        return(x$spec)
    }

    x
}

as_transition_selector <- function(take) {
    if (is.null(take)) {
        return(all_selector())
    }

    if (inherits(take, "bracketeer_selector")) {
        return(take)
    }

    if (is.function(take)) {
        return(filter_by(take))
    }

    stop("`take` must be NULL, a function, or a bracketeer_selector")
}

add_stage_verb <- function(x, id, stage, from = NULL, take = NULL,
                           seeding = "by_source_rank", consume = TRUE,
                           allow_overlap = FALSE, priority = 1L,
                           transition_id = NULL) {
    if (!inherits(x, "bracketeer_spec") && !inherits(x, "tournament_spec") &&
        !inherits(x, "tournament")) {
        stop("Stage verbs require a bracketeer_spec or tournament object")
    }

    if (!is.character(id) || length(id) != 1L || is.na(id) || id == "") {
        stop("`id` must be a non-empty string")
    }

    spec_graph <- as_stage_graph(x)
    prior_stage_order <- spec_graph$metadata$stage_order
    if (is.null(prior_stage_order)) {
        prior_stage_order <- character(0)
    }
    spec_graph <- add_stage(spec_graph, stage_id = id, stage = stage)
    class(spec_graph) <- unique(c("bracketeer_spec", class(spec_graph)))

    if (length(prior_stage_order) > 0L || !is.null(from)) {
        resolved_from <- from
        if (is.null(resolved_from) && length(prior_stage_order) > 0L) {
            resolved_from <- from_previous()
        }

        if (!is.null(resolved_from)) {
            spec_graph <- register_edge(
                spec_graph,
                from = resolved_from,
                to = id,
                take = as_transition_selector(take),
                seeding = seeding,
                priority = priority,
                consume = consume,
                allow_overlap = allow_overlap,
                transition_id = transition_id
            )
            class(spec_graph) <- unique(c("bracketeer_spec", class(spec_graph)))
        }
    }

    if (!inherits(x, "tournament")) {
        return(spec_graph)
    }

    auto_advance <- if (!is.null(x$auto_advance)) x$auto_advance else TRUE
    trn <- build(spec_graph, x$participants)
    trn$auto_advance <- auto_advance
    trn
}
