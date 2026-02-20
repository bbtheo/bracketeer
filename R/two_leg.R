#' Create a two-leg stage or bracket
#'
#' Alias for `two_leg_knockout()` used by the tournament stage-verb API.
#'
#' @param participants Participants, a spec object, or a tournament object.
#' @param ... Additional arguments forwarded to `two_leg_knockout()`.
#'
#' @return A bracket, spec, or tournament depending on `participants`.
#' @examples
#' # Two-leg knockout (Champions League style)
#' teams <- paste("Club", sprintf("%02d", 1:16))
#' trn <- tournament(teams) |>
#'   round_robin("groups", groups = 4) |>
#'   two_leg("knockouts", take = top_per_group(2))
#' @export
two_leg <- function(participants, ...) {
    two_leg_knockout(participants, ...)
}
