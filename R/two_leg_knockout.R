#' Create a two-leg knockout bracket
#'
#' Two-leg knockout tournament with home/away legs and aggregate scoring.
#' Pass per-leg scores as length-2 vectors to `set_result()`.
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a 'name' column and optional 'seed' column.
#' @param ... Additional arguments passed to bracket constructors or
#'   tournament stage-verb dispatch methods.
#'
#' @return A two_leg_knockout object
#' @keywords internal

two_leg_knockout <- function(participants, ...) {
    if (inherits(participants, "bracketeer_spec")) {
        return(two_leg_knockout.bracketeer_spec(participants, ...))
    }
    stop(
        "`two_leg()` expected a `bracketeer_spec` input (from `spec()` or `tournament()`) ",
        "but got class `", paste(class(participants), collapse = "/"), "`. ",
        "For standalone bracket construction, use `new_two_leg_bracket()`."
    )
}

#' @keywords internal
two_leg_knockout.default <- function(participants, seed = TRUE,
                                     third_place = FALSE, away_goals = TRUE,
                                     reseed = FALSE) {
    bracket <- single_elim.default(
        participants = participants,
        seed = seed,
        third_place = third_place,
        best_of = NULL,
        reseed = reseed
    )

    bracket$type <- "two_leg_knockout"
    bracket$legs <- 2L
    bracket$home_away <- TRUE
    bracket$away_goals <- isTRUE(away_goals)
    class(bracket) <- c("two_leg_knockout", class(bracket))

    bracket
}

#' Internal two-leg bracket constructor
#'
#' @param participants Character vector of participant names, or a data.frame
#'   with a `name` column and optional `seed` column.
#' @param seed Seeding policy forwarded to internal seeding helpers.
#' @param third_place Whether to include a third-place match.
#' @param away_goals Whether away goals break aggregate-score ties.
#' @param reseed Whether to reseed participants between rounds.
#' @return A two_leg_knockout object.
#' @keywords internal
new_two_leg_bracket <- function(participants, seed = TRUE,
                                third_place = FALSE, away_goals = TRUE,
                                reseed = FALSE) {
    two_leg_knockout.default(
        participants = participants,
        seed = seed,
        third_place = third_place,
        away_goals = away_goals,
        reseed = reseed
    )
}

#' @keywords internal
two_leg_knockout.bracketeer_spec <- function(participants, id, seed = TRUE,
                                             third_place = FALSE, away_goals = TRUE,
                                             reseed = FALSE,
                                             from = NULL, take = NULL,
                                             seeding = "by_source_rank",
                                             consume = TRUE, allow_overlap = FALSE,
                                             priority = 1L, transition_id = NULL) {
    add_stage_verb(
        participants,
        id = id,
        stage = two_leg_stage(
            seed = seed,
            third_place = third_place,
            away_goals = away_goals,
            reseed = reseed
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
two_leg_knockout.tournament_spec <- function(participants, id, seed = TRUE,
                                             third_place = FALSE, away_goals = TRUE,
                                             reseed = FALSE,
                                             from = NULL, take = NULL,
                                             seeding = "by_source_rank",
                                             consume = TRUE, allow_overlap = FALSE,
                                             priority = 1L, transition_id = NULL) {
    two_leg_knockout.bracketeer_spec(
        participants = participants,
        id = id,
        seed = seed,
        third_place = third_place,
        away_goals = away_goals,
        reseed = reseed,
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
two_leg_knockout.tournament <- function(participants, id, seed = TRUE,
                                        third_place = FALSE, away_goals = TRUE,
                                        reseed = FALSE,
                                        from = NULL, take = NULL,
                                        seeding = "by_source_rank",
                                        consume = TRUE, allow_overlap = FALSE,
                                        priority = 1L, transition_id = NULL) {
    two_leg_knockout.bracketeer_spec(
        participants = participants,
        id = id,
        seed = seed,
        third_place = third_place,
        away_goals = away_goals,
        reseed = reseed,
        from = from,
        take = take,
        seeding = seeding,
        consume = consume,
        allow_overlap = allow_overlap,
        priority = priority,
        transition_id = transition_id
    )
}
