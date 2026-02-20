#' Create a group stage followed by knockout bracket
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' column.
#' @param ... Additional arguments passed to bracket constructors or
#'   tournament stage-verb dispatch methods.
#'
#' @return A group_stage_knockout object
#' @export

group_stage_knockout <- function(participants, ...) {
    if (inherits(participants, "bracketeer_spec")) {
        return(group_stage_knockout.bracketeer_spec(participants, ...))
    }
    stop(
        "`group_stage_knockout()` expected a `bracketeer_spec` input (from `spec()` or `tournament()`) ",
        "but got class `", paste(class(participants), collapse = "/"), "`. ",
        "For standalone bracket construction, use `new_group_stage_knockout_bracket()`."
    )
}

#' @keywords internal
group_stage_knockout.default <- function(participants,
                                         groups = 2,
                                         advance_per_group = 2,
                                         seed = TRUE,
                                         group_home_away = FALSE,
                                         group_best_of = NULL,
                                         group_tiebreakers = NULL,
                                         knockout_type = "single_elim",
                                         knockout_seed = TRUE,
                                         third_place = FALSE,
                                         grand_final_reset = TRUE,
                                         knockout_best_of = NULL) {
    knockout_type <- match.arg(knockout_type, c("single_elim", "double_elim"))
    if (length(groups) != 1L || !is.numeric(groups) || !is.finite(groups)) {
        stop("groups must be a finite numeric value")
    }
    groups <- as.integer(groups)
    if (groups < 1L) {
        stop("groups must be >= 1")
    }
    if (length(advance_per_group) != 1L ||
        !is.numeric(advance_per_group) || !is.finite(advance_per_group)) {
        stop("advance_per_group must be a finite numeric value")
    }
    advance_per_group <- as.integer(advance_per_group)
    info <- normalize_participants(participants)
    seeded <- apply_seed_method(info$data, seed)
    grouped <- assign_groups(seeded, groups)
    group_best_of <- validate_best_of(group_best_of)
    knockout_best_of <- validate_best_of(knockout_best_of)

    if (advance_per_group < 1) {
        stop("advance_per_group must be >= 1")
    }

    group_labels <- sort(unique(grouped$group))
    group_brackets <- list()

    for (label in group_labels) {
        names <- grouped$name[grouped$group == label]
        if (length(names) < advance_per_group) {
            stop("Group ", label, " has fewer participants than advance_per_group")
        }
        group_brackets[[label]] <- round_robin.default(
            names,
            home_away = group_home_away,
            best_of = group_best_of,
            tiebreakers = group_tiebreakers
        )
        group_brackets[[label]]$group <- label
    }

    # Build global match map for group phase
    group_match_map <- data.frame(
        id = integer(0),
        group = character(0),
        local_id = integer(0),
        stringsAsFactors = FALSE
    )
    global_id <- 1L
    for (label in group_labels) {
        for (m in group_brackets[[label]]$matches) {
            group_match_map <- rbind(group_match_map, data.frame(
                id = global_id,
                group = label,
                local_id = m$id,
                stringsAsFactors = FALSE
            ))
            global_id <- global_id + 1L
        }
    }

    bracket <- list(
        type = "group_stage_knockout",
        phase = "groups",
        participants = grouped$name,
        participants_df = grouped,
        group_brackets = group_brackets,
        group_match_map = group_match_map,
        groups = groups,
        advance_per_group = advance_per_group,
        group_home_away = group_home_away,
        group_best_of = group_best_of,
        group_tiebreakers = group_tiebreakers,
        knockout_type = knockout_type,
        knockout_seed = knockout_seed,
        third_place = third_place,
        grand_final_reset = grand_final_reset,
        knockout_best_of = knockout_best_of,
        knockout_bracket = NULL,
        reseed = FALSE
    )
    class(bracket) <- c("group_stage_knockout", "bracket")

    bracket
}

#' Internal group-stage-knockout bracket constructor
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column and optional `seed` column.
#' @param groups Number of groups to create.
#' @param advance_per_group Number of participants advancing from each group.
#' @param seed Seeding policy for initial participant allocation.
#' @param group_home_away Whether group matches are home/away double round robin.
#' @param group_best_of Optional odd-integer series length for group matches.
#' @param group_tiebreakers Optional ordered tiebreaker vector for groups.
#' @param knockout_type Knockout format: `"single_elim"` or `"double_elim"`.
#' @param knockout_seed Seeding policy for knockout-stage placement.
#' @param third_place Whether to include a third-place match in single elimination.
#' @param grand_final_reset Whether double-elim knockout can trigger a reset final.
#' @param knockout_best_of Optional odd-integer series length for knockout matches.
#' @return A group_stage_knockout object.
#' @keywords internal
new_group_stage_knockout_bracket <- function(participants,
                                             groups = 2,
                                             advance_per_group = 2,
                                             seed = TRUE,
                                             group_home_away = FALSE,
                                             group_best_of = NULL,
                                             group_tiebreakers = NULL,
                                             knockout_type = "single_elim",
                                             knockout_seed = TRUE,
                                             third_place = FALSE,
                                             grand_final_reset = TRUE,
                                             knockout_best_of = NULL) {
    group_stage_knockout.default(
        participants = participants,
        groups = groups,
        advance_per_group = advance_per_group,
        seed = seed,
        group_home_away = group_home_away,
        group_best_of = group_best_of,
        group_tiebreakers = group_tiebreakers,
        knockout_type = knockout_type,
        knockout_seed = knockout_seed,
        third_place = third_place,
        grand_final_reset = grand_final_reset,
        knockout_best_of = knockout_best_of
    )
}

#' @keywords internal
group_stage_knockout.bracketeer_spec <- function(participants, id,
                                                 groups = 2,
                                                 advance_per_group = 2,
                                                 seed = TRUE,
                                                 group_home_away = FALSE,
                                                 group_best_of = NULL,
                                                 group_tiebreakers = NULL,
                                                 knockout_type = "single_elim",
                                                 knockout_seed = TRUE,
                                                 third_place = FALSE,
                                                 grand_final_reset = TRUE,
                                                 knockout_best_of = NULL,
                                                 from = NULL, take = NULL,
                                                 seeding = "by_source_rank",
                                                 consume = TRUE, allow_overlap = FALSE,
                                                 priority = 1L, transition_id = NULL) {
    add_stage_verb(
        participants,
        id = id,
        stage = group_stage_knockout_stage(
            groups = groups,
            advance_per_group = advance_per_group,
            seed = seed,
            group_home_away = group_home_away,
            group_best_of = group_best_of,
            group_tiebreakers = group_tiebreakers,
            knockout_type = knockout_type,
            knockout_seed = knockout_seed,
            third_place = third_place,
            grand_final_reset = grand_final_reset,
            knockout_best_of = knockout_best_of
        ),
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @keywords internal
group_stage_knockout.tournament_spec <- function(participants, id,
                                                 groups = 2,
                                                 advance_per_group = 2,
                                                 seed = TRUE,
                                                 group_home_away = FALSE,
                                                 group_best_of = NULL,
                                                 group_tiebreakers = NULL,
                                                 knockout_type = "single_elim",
                                                 knockout_seed = TRUE,
                                                 third_place = FALSE,
                                                 grand_final_reset = TRUE,
                                                 knockout_best_of = NULL,
                                                 from = NULL, take = NULL,
                                                 seeding = "by_source_rank",
                                                 consume = TRUE, allow_overlap = FALSE,
                                                 priority = 1L, transition_id = NULL) {
    group_stage_knockout.bracketeer_spec(
        participants = participants,
        id = id,
        groups = groups,
        advance_per_group = advance_per_group,
        seed = seed,
        group_home_away = group_home_away,
        group_best_of = group_best_of,
        group_tiebreakers = group_tiebreakers,
        knockout_type = knockout_type,
        knockout_seed = knockout_seed,
        third_place = third_place,
        grand_final_reset = grand_final_reset,
        knockout_best_of = knockout_best_of,
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @keywords internal
group_stage_knockout.tournament <- function(participants, id,
                                            groups = 2,
                                            advance_per_group = 2,
                                            seed = TRUE,
                                            group_home_away = FALSE,
                                            group_best_of = NULL,
                                            group_tiebreakers = NULL,
                                            knockout_type = "single_elim",
                                            knockout_seed = TRUE,
                                            third_place = FALSE,
                                            grand_final_reset = TRUE,
                                            knockout_best_of = NULL,
                                            from = NULL, take = NULL,
                                            seeding = "by_source_rank",
                                            consume = TRUE, allow_overlap = FALSE,
                                            priority = 1L, transition_id = NULL) {
    group_stage_knockout.bracketeer_spec(
        participants = participants,
        id = id,
        groups = groups,
        advance_per_group = advance_per_group,
        seed = seed,
        group_home_away = group_home_away,
        group_best_of = group_best_of,
        group_tiebreakers = group_tiebreakers,
        knockout_type = knockout_type,
        knockout_seed = knockout_seed,
        third_place = third_place,
        grand_final_reset = grand_final_reset,
        knockout_best_of = knockout_best_of,
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}

#' @rdname set_result
#' @export
set_result.group_stage_knockout <- function(bracket, match_id, score1, score2,
                                            stage_id = NULL, overwrite = FALSE,
                                            auto_advance = FALSE) {
    if (bracket$phase == "groups") {
        row <- bracket$group_match_map[bracket$group_match_map$id == match_id, ]
        if (nrow(row) == 0) stop("Invalid match_id: ", match_id)
        group <- row$group[1]
        local_id <- row$local_id[1]
        bracket$group_brackets[[group]] <- set_result(
            bracket$group_brackets[[group]],
            local_id,
            score1,
            score2
        )
        return(bracket)
    }

    if (is.null(bracket$knockout_bracket)) {
        stop("Knockout bracket not initialized")
    }

    bracket$knockout_bracket <- set_result(
        bracket$knockout_bracket,
        match_id,
        score1,
        score2
    )
    bracket
}

#' @rdname set_winner
#' @export
set_winner.group_stage_knockout <- function(bracket, match_id, winner) {
    if (bracket$phase == "groups") {
        row <- bracket$group_match_map[bracket$group_match_map$id == match_id, ]
        if (nrow(row) == 0) stop("Invalid match_id: ", match_id)
        group <- row$group[1]
        local_id <- row$local_id[1]
        bracket$group_brackets[[group]] <- set_winner(
            bracket$group_brackets[[group]],
            local_id,
            winner
        )
        return(bracket)
    }

    if (is.null(bracket$knockout_bracket)) {
        stop("Knockout bracket not initialized")
    }

    bracket$knockout_bracket <- set_winner(
        bracket$knockout_bracket,
        match_id,
        winner
    )
    bracket
}

#' @rdname get_matches
#' @export
get_matches.group_stage_knockout <- function(bracket, round = NULL, status = "all") {
    status <- match.arg(status, c("pending", "complete", "all"))

    if (bracket$phase == "groups") {
        rows <- list()
        for (label in names(bracket$group_brackets)) {
            df <- get_matches(bracket$group_brackets[[label]], round = round, status = status)
            if (nrow(df) == 0) next
            df$group <- label
            df$local_id <- df$id
            map <- bracket$group_match_map
            df$id <- map$id[match(paste(label, df$local_id), paste(map$group, map$local_id))]
            df$phase <- "groups"
            rows[[length(rows) + 1L]] <- df
        }
        if (length(rows) == 0) return(data.frame())
        return(do.call(rbind, rows))
    }

    df <- get_matches(bracket$knockout_bracket, round = round, status = status)
    if (nrow(df) == 0) return(df)
    df$phase <- "knockout"
    df$group <- NA_character_
    df$local_id <- df$id
    df
}

#' @rdname get_standings
#' @export
get_standings.group_stage_knockout <- function(bracket) {
    if (bracket$phase == "groups") {
        rows <- list()
        for (label in names(bracket$group_brackets)) {
            df <- get_standings(bracket$group_brackets[[label]])
            df$group <- label
            rows[[length(rows) + 1L]] <- df
        }
        if (length(rows) == 0) return(data.frame())
        return(do.call(rbind, rows))
    }

    get_standings(bracket$knockout_bracket)
}

#' @rdname advance
#' @export
advance.group_stage_knockout <- function(x, stage = NULL, ...) {
    bracket <- x
    if (bracket$phase == "groups") {
        # Check all groups complete
        for (label in names(bracket$group_brackets)) {
            if (!is_complete(bracket$group_brackets[[label]])) {
                stop("Group ", label, " is not complete")
            }
        }

        qualifiers <- data.frame(
            name = character(0),
            seed = integer(0),
            group = character(0),
            rank = integer(0),
            stringsAsFactors = FALSE
        )

        group_labels <- names(bracket$group_brackets)
        for (i in seq_along(group_labels)) {
            label <- group_labels[[i]]
            standings <- get_standings(bracket$group_brackets[[label]])
            top <- standings[seq_len(bracket$advance_per_group), ]
            seed_offset <- (top$rank - 1L) * length(group_labels) + i
            qualifiers <- rbind(qualifiers, data.frame(
                name = top$participant,
                seed = seed_offset,
                group = label,
                rank = top$rank,
                stringsAsFactors = FALSE
            ))
        }

        # Build knockout bracket
        if (bracket$knockout_type == "single_elim") {
            bracket$knockout_bracket <- single_elim.default(
                qualifiers[, c("name", "seed")],
                seed = bracket$knockout_seed,
                third_place = bracket$third_place,
                best_of = bracket$knockout_best_of
            )
        } else if (bracket$knockout_type == "double_elim") {
            bracket$knockout_bracket <- double_elim.default(
                qualifiers[, c("name", "seed")],
                seed = bracket$knockout_seed,
                grand_final_reset = bracket$grand_final_reset,
                best_of = bracket$knockout_best_of
            )
        } else {
            stop("Unsupported knockout_type: ", bracket$knockout_type)
        }

        bracket$phase <- "knockout"
        return(bracket)
    }

    if (is.null(bracket$knockout_bracket)) {
        stop("Knockout bracket not initialized")
    }

    bracket$knockout_bracket <- advance(bracket$knockout_bracket)
    bracket
}

#' @rdname get_winner
#' @export
get_winner.group_stage_knockout <- function(bracket) {
    if (bracket$phase != "knockout") return(NA_character_)
    get_winner(bracket$knockout_bracket)
}

#' @rdname is_complete
#' @export
is_complete.group_stage_knockout <- function(bracket) {
    if (bracket$phase != "knockout") return(FALSE)
    is_complete(bracket$knockout_bracket)
}

#' @rdname print
#' @export
print.group_stage_knockout <- function(x, ...) {
    cat("Group Stage + Knockout Tournament\n")
    cat(sprintf("Participants: %d\n", length(x$participants)))
    cat(sprintf("Groups: %d\n", x$groups))
    cat(sprintf("Advance per group: %d\n", x$advance_per_group))
    cat(sprintf("Phase: %s\n", x$phase))

    if (x$phase == "groups") {
        for (label in names(x$group_brackets)) {
            cat(sprintf("\nGroup %s:\n", label))
            print(x$group_brackets[[label]])
        }
    } else {
        cat("\nKnockout:\n")
        print(x$knockout_bracket)
    }

    invisible(x)
}
