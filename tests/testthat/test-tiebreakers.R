test_that("head-to-head tiebreaker ranks correctly", {
    teams <- c("A", "B", "C", "D")
    b <- new_round_robin_bracket(teams, tiebreakers = c("points", "head_to_head", "score_diff"))

    set_win <- function(bracket, p1, p2, winner) {
        df <- get_matches(bracket)
        row <- df[(df$participant1 == p1 & df$participant2 == p2) |
            (df$participant1 == p2 & df$participant2 == p1), ]
        id <- row$id[1]
        if (row$participant1[1] == winner) {
            return(set_result(bracket, id, 1, 0))
        }
        set_result(bracket, id, 0, 1)
    }

    b <- set_win(b, "A", "B", "A")
    b <- set_win(b, "A", "C", "A")
    b <- set_win(b, "A", "D", "D")
    b <- set_win(b, "B", "C", "B")
    b <- set_win(b, "B", "D", "B")
    b <- set_win(b, "C", "D", "C")

    standings <- get_standings(b)
    expect_equal(standings$participant[1], "A")
    expect_equal(standings$participant[2], "B")
})

test_that("tiebreaker_chain preserves order and deduplicates", {
    chain <- tiebreaker_chain("points", "score_diff", "points", "head_to_head")
    expect_equal(chain, c("points", "score_diff", "head_to_head"))

    b <- new_round_robin_bracket(c("A", "B", "C", "D"), tiebreakers = chain)
    expect_equal(b$tiebreakers[1:3], c("points", "score_diff", "head_to_head"))
})

test_that("tiebreaker_chain validates inputs", {
    expect_error(tiebreaker_chain(), "at least one")
    expect_error(tiebreaker_chain("points", NA_character_), "non-empty strings")
    expect_error(tiebreaker_chain("points", ""), "non-empty strings")
})
