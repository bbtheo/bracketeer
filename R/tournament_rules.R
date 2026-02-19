#' Construct a selector object for transition `take =` routing
#'
#' @param kind Selector kind label.
#' @param params Selector parameters list.
#' @param evaluator Function implementing selection logic.
#'
#' @return A `bracketeer_selector` object.
new_selector <- function(kind, params = list(), evaluator) {
    if (!is.character(kind) || length(kind) != 1L || is.na(kind) || kind == "") {
        stop("`kind` must be a non-empty string")
    }
    if (!is.list(params)) {
        stop("`params` must be a list")
    }
    if (!is.function(evaluator)) {
        stop("`evaluator` must be a function")
    }

    structure(
        list(
            kind = kind,
            params = params,
            evaluator = evaluator
        ),
        class = "bracketeer_selector"
    )
}

all_selector <- function() {
    new_selector(
        kind = "all",
        evaluator = function(source_pool = NULL, standings = NULL,
                             source_bracket = NULL, participants = NULL, ...) {
            if (!is.null(source_pool)) {
                return(as.character(source_pool))
            }
            if (!is.null(standings) &&
                is.data.frame(standings) &&
                "participant" %in% names(standings)) {
                return(as.character(standings$participant))
            }
            if (!is.null(source_bracket$participants)) {
                return(as.character(source_bracket$participants))
            }
            if (!is.null(participants) &&
                is.data.frame(participants) &&
                "name" %in% names(participants)) {
                return(as.character(participants$name))
            }

            character(0)
        }
    )
}

#' Select top ranked participants from source standings
#'
#' @param n Positive integer count.
#'
#' @return A `bracketeer_selector` object.
#' @export
top_n <- function(n) {
    n <- validate_selector_count(n, arg = "n")

    new_selector(
        kind = "top_n",
        params = list(n = n),
        evaluator = function(source_pool = NULL, standings = NULL, ...) {
            names <- standings_participants_or_pool(
                standings = standings,
                source_pool = source_pool
            )
            names[seq_len(min(length(names), n))]
        }
    )
}

#' Select bottom ranked participants from source standings
#'
#' @param n Positive integer count.
#'
#' @return A `bracketeer_selector` object.
#' @export
bottom_n <- function(n) {
    n <- validate_selector_count(n, arg = "n")

    new_selector(
        kind = "bottom_n",
        params = list(n = n),
        evaluator = function(source_pool = NULL, standings = NULL, ...) {
            names <- standings_participants_or_pool(
                standings = standings,
                source_pool = source_pool
            )
            if (length(names) == 0L) {
                return(character(0))
            }
            start <- max(length(names) - n + 1L, 1L)
            names[seq.int(start, length(names))]
        }
    )
}

#' Select an inclusive standings slice
#'
#' @param from Positive integer starting position.
#' @param to Positive integer ending position (must be `>= from`).
#'
#' @return A `bracketeer_selector` object.
#' @export
slice_range <- function(from, to) {
    from <- validate_selector_count(from, arg = "from")
    to <- validate_selector_count(to, arg = "to")
    if (to < from) {
        stop("`to` must be >= `from`")
    }

    new_selector(
        kind = "slice_range",
        params = list(from = from, to = to),
        evaluator = function(source_pool = NULL, standings = NULL, ...) {
            names <- standings_participants_or_pool(
                standings = standings,
                source_pool = source_pool
            )
            if (length(names) == 0L || from > length(names)) {
                return(character(0))
            }
            names[seq.int(from, min(to, length(names)))]
        }
    )
}

#' Select entrants remaining in the current transition source pool
#'
#' @return A `bracketeer_selector` object.
#' @export
remaining <- function() {
    new_selector(
        kind = "remaining",
        evaluator = qualify_remaining()
    )
}

#' Select losers from a source stage by elimination round
#'
#' @param round One of `"all"`, `"latest"`, or an integer vector of rounds.
#' @param stage Optional stage selector (reserved for future use).
#' @param ordering Ordering mode: `"elimination_round"`, `"source_seed"`, or
#'   `"as_recorded"`.
#'
#' @return A `bracketeer_selector` object.
#' @export
losers <- function(round = "all", stage = NULL, ordering = "elimination_round") {
    new_selector(
        kind = "losers",
        params = list(round = round, stage = stage, ordering = ordering),
        evaluator = qualify_losers(
            round = round,
            stage = stage,
            ordering = ordering
        )
    )
}

#' Select participants using a custom predicate function
#'
#' @param fn A transition predicate function.
#'
#' @return A `bracketeer_selector` object.
#' @export
filter_by <- function(fn) {
    if (!is.function(fn)) {
        stop("`fn` must be a function")
    }

    new_selector(
        kind = "filter_by",
        params = list(),
        evaluator = fn
    )
}

#' Select top ranked participants per group
#'
#' @param n Positive integer count per group.
#'
#' @return A `bracketeer_selector` object.
#' @export
top_per_group <- function(n) {
    n <- validate_selector_count(n, arg = "n")

    new_selector(
        kind = "top_per_group",
        params = list(n = n),
        evaluator = function(standings = NULL, ...) {
            grouped <- require_grouped_standings(standings)
            select_per_group(grouped, fn = function(df) head(df, n))
        }
    )
}

#' Select bottom ranked participants per group
#'
#' @param n Positive integer count per group.
#'
#' @return A `bracketeer_selector` object.
#' @export
bottom_per_group <- function(n) {
    n <- validate_selector_count(n, arg = "n")

    new_selector(
        kind = "bottom_per_group",
        params = list(n = n),
        evaluator = function(standings = NULL, ...) {
            grouped <- require_grouped_standings(standings)
            select_per_group(grouped, fn = function(df) tail(df, n))
        }
    )
}

#' Select an inclusive standings slice per group
#'
#' @param from Positive integer starting position.
#' @param to Positive integer ending position (must be `>= from`).
#'
#' @return A `bracketeer_selector` object.
#' @export
slice_per_group <- function(from, to) {
    from <- validate_selector_count(from, arg = "from")
    to <- validate_selector_count(to, arg = "to")
    if (to < from) {
        stop("`to` must be >= `from`")
    }

    new_selector(
        kind = "slice_per_group",
        params = list(from = from, to = to),
        evaluator = function(standings = NULL, ...) {
            grouped <- require_grouped_standings(standings)
            select_per_group(grouped, fn = function(df) {
                if (nrow(df) == 0L || from > nrow(df)) {
                    return(df[0, , drop = FALSE])
                }
                df[seq.int(from, min(to, nrow(df))), , drop = FALSE]
            })
        }
    )
}

evaluate_selector <- function(selector, source_pool = NULL, standings = NULL,
                              source_bracket = NULL, participants = NULL) {
    if (!inherits(selector, "bracketeer_selector")) {
        stop("`selector` must inherit from bracketeer_selector")
    }

    invoke_selector_fn(
        fn = selector$evaluator,
        source_pool = source_pool,
        standings = standings,
        source_bracket = source_bracket,
        participants = participants
    )
}

describe_selector <- function(selector) {
    if (is.null(selector)) {
        return("default_transition_selection")
    }
    if (!inherits(selector, "bracketeer_selector")) {
        return(as.character(selector)[[1]])
    }

    kind <- selector$kind
    if (is.null(selector$params) || length(selector$params) == 0L) {
        return(kind)
    }

    formatted_params <- vapply(names(selector$params), function(name) {
        value <- selector$params[[name]]
        value_chr <- paste(as.character(value), collapse = ",")
        paste0(name, "=", value_chr)
    }, character(1))

    paste0(kind, "(", paste(formatted_params, collapse = ", "), ")")
}

validate_selector_count <- function(x, arg) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x %% 1 != 0) {
        stop("`", arg, "` must be a positive integer")
    }
    as.integer(x)
}

standings_participants_or_pool <- function(standings, source_pool) {
    if (!is.null(standings) &&
        is.data.frame(standings) &&
        "participant" %in% names(standings)) {
        return(as.character(standings$participant))
    }

    if (!is.null(source_pool)) {
        return(as.character(source_pool))
    }

    character(0)
}

invoke_selector_fn <- function(fn, source_pool = NULL, standings = NULL,
                               source_bracket = NULL, participants = NULL) {
    formal_names <- names(formals(fn))
    args <- list(
        source_pool = source_pool,
        standings = standings,
        source_bracket = source_bracket,
        participants = participants
    )

    if (is.null(formal_names) || length(formal_names) == 0L) {
        return(fn())
    }
    if ("..." %in% formal_names) {
        return(do.call(fn, args))
    }

    matched_args <- args[names(args) %in% formal_names]
    if (length(matched_args) == 0L) {
        return(fn())
    }

    do.call(fn, matched_args)
}

require_grouped_standings <- function(standings) {
    if (is.null(standings) || !is.data.frame(standings) ||
        !"participant" %in% names(standings) ||
        !"group" %in% names(standings)) {
        stop("Selector requires grouped standings with `participant` and `group` columns")
    }

    grouped <- standings[, c("participant", "group"), drop = FALSE]
    grouped$participant <- as.character(grouped$participant)
    grouped$group <- as.character(grouped$group)
    grouped
}

select_per_group <- function(grouped_standings, fn) {
    groups <- unique(grouped_standings$group)
    selected <- unlist(lapply(groups, function(group_id) {
        rows <- grouped_standings[grouped_standings$group == group_id, , drop = FALSE]
        as.character(fn(rows)$participant)
    }), use.names = FALSE)

    selected
}

#' Select all entrants remaining in the transition source pool
#'
#' Returns a transition rule function intended for use with `add_transition()`.
#' During `advance()`, this selects all participants still available from the
#' source stage after higher-priority consuming transitions have resolved.
#'
#' @return A transition rule function.
qualify_remaining <- function() {
    function(source_pool = NULL, standings = NULL, source_bracket = NULL, ...) {
        if (!is.null(source_pool)) {
            return(as.character(source_pool))
        }

        if (!is.null(standings) &&
            is.data.frame(standings) &&
            "participant" %in% names(standings)) {
            return(as.character(standings$participant))
        }

        if (!is.null(source_bracket$participants)) {
            return(as.character(source_bracket$participants))
        }

        character(0)
    }
}

#' Select losers from a source stage by elimination round
#'
#' Returns a transition rule function intended for use with `add_transition()`.
#'
#' @param round One of `"all"`, `"latest"`, or an integer vector of elimination
#'   rounds to include.
#' @param stage Optional stage selector (reserved for future use).
#' @param ordering Ordering mode: `"elimination_round"`, `"source_seed"`, or
#'   `"as_recorded"`.
#'
#' @return A transition rule function.
qualify_losers <- function(round = "all", stage = NULL,
                           ordering = "elimination_round") {
    if (!is.null(stage) &&
        (!is.character(stage) || length(stage) != 1L || is.na(stage) || stage == "")) {
        stop("`stage` must be NULL or a non-empty string")
    }

    if (!is.character(ordering) || length(ordering) != 1L || is.na(ordering)) {
        stop("`ordering` must be one of: elimination_round, source_seed, as_recorded")
    }
    ordering <- match.arg(ordering, c("elimination_round", "source_seed", "as_recorded"))

    round_mode <- normalize_qualify_losers_round(round)

    function(source_pool = NULL, standings = NULL, source_bracket = NULL, ...) {
        if (is.null(source_bracket) || !is.list(source_bracket) || is.null(source_bracket$matches)) {
            stop("`qualify_losers()` requires `source_bracket` with match data")
        }

        eliminations <- compute_stage_eliminations(source_bracket)
        if (nrow(eliminations) == 0L) {
            return(character(0))
        }

        available_rounds <- sort(unique(eliminations$elimination_round))
        target_rounds <- resolve_qualify_losers_rounds(
            round_mode = round_mode,
            available_rounds = available_rounds
        )
        selected <- eliminations[eliminations$elimination_round %in% target_rounds, , drop = FALSE]
        if (nrow(selected) == 0L) {
            return(character(0))
        }

        ordered_names <- order_qualified_losers(selected, ordering = ordering)

        if (!is.null(source_pool)) {
            source_pool <- as.character(source_pool)
            ordered_names <- ordered_names[ordered_names %in% source_pool]
        }

        ordered_names
    }
}

normalize_qualify_losers_round <- function(round) {
    if (is.character(round)) {
        if (length(round) != 1L || is.na(round)) {
            stop("`round` must be \"all\", \"latest\", or an integer vector")
        }
        if (!round %in% c("all", "latest")) {
            stop("`round` must be \"all\", \"latest\", or an integer vector")
        }
        return(list(type = "label", value = round))
    }

    if (!is.numeric(round)) {
        stop("`round` must be \"all\", \"latest\", or an integer vector")
    }

    round <- as.integer(round)
    if (length(round) == 0L || any(is.na(round)) || any(round < 1L)) {
        stop("`round` integer values must be positive")
    }

    list(type = "numeric", value = sort(unique(round)))
}

resolve_qualify_losers_rounds <- function(round_mode, available_rounds) {
    if (length(available_rounds) == 0L) {
        return(integer(0))
    }

    if (identical(round_mode$type, "label")) {
        if (identical(round_mode$value, "all")) {
            return(available_rounds)
        }
        return(max(available_rounds))
    }

    requested <- round_mode$value
    missing_rounds <- requested[!requested %in% available_rounds]
    if (length(missing_rounds) > 0L) {
        stop(
            "Requested loser round(s) not available: ",
            paste(missing_rounds, collapse = ", "),
            ". Use available rounds: ",
            paste(available_rounds, collapse = ", ")
        )
    }

    requested
}

compute_stage_eliminations <- function(source_bracket) {
    completed <- Filter(function(match) {
        identical(match$status, "complete") && !is.na(match$loser)
    }, source_bracket$matches)
    if (length(completed) == 0L) {
        return(data.frame(
            participant = character(0),
            elimination_round = integer(0),
            event_index = integer(0),
            source_seed = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    event_table <- data.frame(
        participant = vapply(completed, `[[`, character(1), "loser"),
        elimination_round = vapply(completed, `[[`, integer(1), "round"),
        event_index = seq_along(completed),
        stringsAsFactors = FALSE
    )

    participants <- unique(event_table$participant)
    elimination_round <- vapply(participants, function(name) {
        max(event_table$elimination_round[event_table$participant == name])
    }, integer(1))
    elimination_event <- vapply(participants, function(name) {
        max(event_table$event_index[event_table$participant == name])
    }, integer(1))

    source_seed <- resolve_source_seed_rank(participants = participants, source_bracket = source_bracket)

    data.frame(
        participant = participants,
        elimination_round = as.integer(elimination_round),
        event_index = as.integer(elimination_event),
        source_seed = as.integer(source_seed),
        stringsAsFactors = FALSE
    )
}

resolve_source_seed_rank <- function(participants, source_bracket) {
    seed_lookup <- setNames(seq_along(participants), participants)

    if (!is.null(source_bracket$participants_df) &&
        is.data.frame(source_bracket$participants_df) &&
        "name" %in% names(source_bracket$participants_df)) {
        if ("seed_rank" %in% names(source_bracket$participants_df)) {
            ranks <- as.integer(source_bracket$participants_df$seed_rank)
        } else if ("seed" %in% names(source_bracket$participants_df)) {
            ranks <- rank(source_bracket$participants_df$seed, ties.method = "first")
            ranks <- as.integer(ranks)
        } else {
            ranks <- seq_len(nrow(source_bracket$participants_df))
        }
        seed_lookup <- setNames(ranks, as.character(source_bracket$participants_df$name))
    } else if (!is.null(source_bracket$participants)) {
        seed_lookup <- setNames(
            seq_along(source_bracket$participants),
            as.character(source_bracket$participants)
        )
    }

    matched <- as.integer(seed_lookup[participants])
    fallback_start <- if (length(seed_lookup) == 0L) 1L else max(seed_lookup, na.rm = TRUE) + 1L
    missing_idx <- which(is.na(matched))
    if (length(missing_idx) > 0L) {
        matched[missing_idx] <- seq.int(fallback_start, length.out = length(missing_idx))
    }

    matched
}

order_qualified_losers <- function(selected, ordering) {
    if (ordering == "source_seed") {
        ord <- order(selected$source_seed, selected$participant)
    } else if (ordering == "as_recorded") {
        ord <- order(selected$event_index, selected$participant)
    } else {
        ord <- order(
            selected$elimination_round,
            selected$source_seed,
            selected$participant
        )
    }

    selected$participant[ord]
}
