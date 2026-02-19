#' Create a stage specification
#'
#' Stage specifications describe how to materialize a stage bracket from a
#' participant set inside a `tournament_spec` graph.
#'
#' @param seed Logical or character seed method.
#' @param third_place Logical; include third-place match.
#' @param best_of Optional best-of value (must be odd).
#' @param reseed Logical; reseed between rounds for supported formats.
#'
#' @return A `stage_spec` object.
single_elim_stage <- function(seed = TRUE, third_place = FALSE,
                              best_of = NULL, reseed = FALSE) {
    validate_stage_flag(third_place, arg = "third_place")
    validate_stage_flag(reseed, arg = "reseed")
    best_of <- validate_best_of(best_of)

    params <- list(
        seed = seed,
        third_place = third_place,
        best_of = best_of,
        reseed = reseed
    )

    new_stage_spec(
        class_name = "single_elim_stage",
        type = "single_elim",
        params = params,
        builder = function(participants) {
            do.call(single_elim.default, c(list(participants = participants), params))
        }
    )
}

#' @rdname single_elim_stage
#' @param grand_final_reset Logical; allow grand-final reset.
double_elim_stage <- function(seed = TRUE, grand_final_reset = TRUE,
                              best_of = NULL, reseed = FALSE) {
    validate_stage_flag(grand_final_reset, arg = "grand_final_reset")
    validate_stage_flag(reseed, arg = "reseed")
    best_of <- validate_best_of(best_of)

    params <- list(
        seed = seed,
        grand_final_reset = grand_final_reset,
        best_of = best_of,
        reseed = reseed
    )

    new_stage_spec(
        class_name = "double_elim_stage",
        type = "double_elim",
        params = params,
        builder = function(participants) {
            do.call(double_elim.default, c(list(participants = participants), params))
        }
    )
}

#' @rdname single_elim_stage
#' @param home_away Logical; whether repeated pairings alternate home/away.
#' @param n_rounds Number of round-robin cycles.
#' @param tiebreakers Ordered tiebreakers.
round_robin_stage <- function(home_away = FALSE, n_rounds = NULL,
                              best_of = NULL, tiebreakers = NULL,
                              groups = NULL) {
    validate_stage_flag(home_away, arg = "home_away")
    best_of <- validate_best_of(best_of)
    if (!is.null(n_rounds)) {
        if (!is.numeric(n_rounds) || length(n_rounds) != 1L ||
            is.na(n_rounds) || n_rounds < 1 || n_rounds %% 1 != 0) {
            stop("`n_rounds` must be a positive integer")
        }
        n_rounds <- as.integer(n_rounds)
    }
    if (!is.null(groups)) {
        if (!is.numeric(groups) || length(groups) != 1L ||
            is.na(groups) || groups < 1 || groups %% 1 != 0) {
            stop("`groups` must be a positive integer")
        }
        groups <- as.integer(groups)
    }

    params <- list(
        home_away = home_away,
        n_rounds = n_rounds,
        best_of = best_of,
        tiebreakers = tiebreakers,
        groups = groups
    )

    new_stage_spec(
        class_name = "round_robin_stage",
        type = "round_robin",
        params = params,
        builder = function(participants) {
            do.call(round_robin.default, c(list(participants = participants), params))
        }
    )
}

#' @rdname single_elim_stage
#' @param rounds Number of Swiss rounds.
#' @param allow_ties Logical; whether ties are allowed.
#' @param bye_points Points awarded for a bye.
swiss_stage <- function(rounds = NULL, seed = TRUE, allow_ties = TRUE,
                        bye_points = 1, best_of = NULL, tiebreakers = NULL) {
    validate_stage_flag(allow_ties, arg = "allow_ties")
    best_of <- validate_best_of(best_of)
    if (!is.null(rounds)) {
        if (!is.numeric(rounds) || length(rounds) != 1L ||
            is.na(rounds) || rounds < 1 || rounds %% 1 != 0) {
            stop("`rounds` must be a positive integer")
        }
        rounds <- as.integer(rounds)
    }
    if (!is.numeric(bye_points) || length(bye_points) != 1L || is.na(bye_points)) {
        stop("`bye_points` must be a numeric scalar")
    }

    params <- list(
        rounds = rounds,
        seed = seed,
        allow_ties = allow_ties,
        bye_points = bye_points,
        best_of = best_of,
        tiebreakers = tiebreakers
    )

    new_stage_spec(
        class_name = "swiss_stage",
        type = "swiss",
        params = params,
        builder = function(participants) {
            do.call(swiss.default, c(list(participants = participants), params))
        }
    )
}

#' @rdname single_elim_stage
#' @param groups Number of groups.
#' @param advance_per_group Number of qualifiers per group.
#' @param group_home_away Logical; home/away behavior in groups.
#' @param group_best_of Optional best-of in groups.
#' @param group_tiebreakers Ordered group-stage tiebreakers.
#' @param knockout_type Knockout format: `"single_elim"` or `"double_elim"`.
#' @param knockout_seed Logical or character seed method for knockout.
#' @param knockout_best_of Optional knockout best-of value.
group_stage_knockout_stage <- function(groups = 2, advance_per_group = 2,
                                       seed = TRUE, group_home_away = FALSE,
                                       group_best_of = NULL, group_tiebreakers = NULL,
                                       knockout_type = "single_elim",
                                       knockout_seed = TRUE, third_place = FALSE,
                                       grand_final_reset = TRUE,
                                       knockout_best_of = NULL) {
    validate_stage_flag(group_home_away, arg = "group_home_away")
    validate_stage_flag(third_place, arg = "third_place")
    validate_stage_flag(grand_final_reset, arg = "grand_final_reset")

    if (!is.numeric(groups) || length(groups) != 1L || is.na(groups) ||
        groups < 1 || groups %% 1 != 0) {
        stop("`groups` must be a positive integer")
    }
    if (!is.numeric(advance_per_group) || length(advance_per_group) != 1L ||
        is.na(advance_per_group) || advance_per_group < 1 ||
        advance_per_group %% 1 != 0) {
        stop("`advance_per_group` must be a positive integer")
    }
    knockout_type <- match.arg(knockout_type, c("single_elim", "double_elim"))
    group_best_of <- validate_best_of(group_best_of)
    knockout_best_of <- validate_best_of(knockout_best_of)

    params <- list(
        groups = as.integer(groups),
        advance_per_group = as.integer(advance_per_group),
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

    new_stage_spec(
        class_name = "group_stage_knockout_stage",
        type = "group_stage_knockout",
        params = params,
        builder = function(participants) {
            do.call(group_stage_knockout.default, c(list(participants = participants), params))
        }
    )
}

#' @rdname single_elim_stage
#' @param away_goals Logical; enable away-goals tiebreaker.
two_leg_stage <- function(seed = TRUE, third_place = FALSE,
                          away_goals = TRUE, reseed = FALSE) {
    validate_stage_flag(third_place, arg = "third_place")
    validate_stage_flag(away_goals, arg = "away_goals")
    validate_stage_flag(reseed, arg = "reseed")

    params <- list(
        seed = seed,
        third_place = third_place,
        away_goals = away_goals,
        reseed = reseed
    )

    new_stage_spec(
        class_name = "two_leg_stage",
        type = "two_leg_knockout",
        params = params,
        builder = function(participants) {
            do.call(two_leg_knockout.default, c(list(participants = participants), params))
        }
    )
}

new_stage_spec <- function(class_name, type, params, builder) {
    if (!is.character(class_name) || length(class_name) != 1L ||
        is.na(class_name) || class_name == "") {
        stop("`class_name` must be a non-empty string")
    }
    if (!is.character(type) || length(type) != 1L || is.na(type) || type == "") {
        stop("`type` must be a non-empty string")
    }
    if (!is.list(params)) {
        stop("`params` must be a list")
    }
    if (!is.function(builder)) {
        stop("`builder` must be a function")
    }

    structure(
        list(
            type = type,
            params = params,
            builder = builder
        ),
        class = c(class_name, "stage_spec")
    )
}

validate_stage_flag <- function(value, arg) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        stop("`", arg, "` must be TRUE or FALSE")
    }
    invisible(value)
}
