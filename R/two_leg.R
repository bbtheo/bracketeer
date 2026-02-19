#' Create a two-leg stage or bracket
#'
#' Alias for `two_leg_knockout()` used by the tournament stage-verb API.
#'
#' @param participants Participants, a spec object, or a tournament object.
#' @param ... Additional arguments forwarded to `two_leg_knockout()`.
#'
#' @return A bracket, spec, or tournament depending on `participants`.
#' @export
two_leg <- function(participants, ...) {
    two_leg_knockout(participants, ...)
}
