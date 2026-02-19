test_that("round robin generates correct number of matches", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_round_robin_bracket(teams)

    expect_equal(length(b$matches), 6L)

    pairs <- vapply(b$matches, function(m) {
        paste(sort(c(m$participant1, m$participant2)), collapse = "-")
    }, character(1))

    expect_equal(length(unique(pairs)), 6L)
})

test_that("round robin supports repeated matchups via n_rounds", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_round_robin_bracket(teams, n_rounds = 4)

    expect_equal(b$rounds, 12L)
    expect_equal(length(b$matches), 24L)

    pairs <- vapply(b$matches, function(m) {
        paste(sort(c(m$participant1, m$participant2)), collapse = "-")
    }, character(1))

    counts <- table(pairs)
    expect_equal(length(counts), 6L)
    expect_true(all(as.integer(counts) == 4L))
})

test_that("round robin validates n_rounds", {
    teams <- paste("Team", LETTERS[1:4])

    expect_error(
        new_round_robin_bracket(teams, n_rounds = 0),
        "n_rounds"
    )

    expect_error(
        new_round_robin_bracket(teams, n_rounds = 1.5),
        "n_rounds"
    )
})

test_that("round robin home_away alternates assignments across rounds", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_round_robin_bracket(teams, home_away = TRUE, n_rounds = 4)
    df <- get_matches(b)

    pairs <- unique(vapply(seq_len(nrow(df)), function(i) {
        paste(sort(c(df$participant1[i], df$participant2[i])), collapse = "-")
    }, character(1)))

    for (pair in pairs) {
        team_pair <- strsplit(pair, "-", fixed = TRUE)[[1]]
        rows <- df[vapply(seq_len(nrow(df)), function(i) {
            paste(sort(c(df$participant1[i], df$participant2[i])), collapse = "-") == pair
        }, logical(1)), ]

        expect_equal(nrow(rows), 4L)
        expect_equal(sum(rows$participant1 == team_pair[1]), 2L)
        expect_equal(sum(rows$participant1 == team_pair[2]), 2L)
    }
})

test_that("round robin randomizes assignment when home_away is FALSE", {
    teams <- paste("Team", LETTERS[1:4])
    set.seed(100)
    b1 <- new_round_robin_bracket(teams, home_away = FALSE, n_rounds = 4)
    set.seed(100)
    b2 <- new_round_robin_bracket(teams, home_away = FALSE, n_rounds = 4)
    set.seed(200)
    b3 <- new_round_robin_bracket(teams, home_away = FALSE, n_rounds = 4)

    ordered1 <- vapply(b1$matches, function(m) {
        paste(m$participant1, m$participant2, sep = ">")
    }, character(1))
    ordered2 <- vapply(b2$matches, function(m) {
        paste(m$participant1, m$participant2, sep = ">")
    }, character(1))
    ordered3 <- vapply(b3$matches, function(m) {
        paste(m$participant1, m$participant2, sep = ">")
    }, character(1))

    expect_identical(ordered1, ordered2)
    expect_false(identical(ordered1, ordered3))
})

test_that("round robin warns when n_rounds is odd", {
    teams <- paste("Team", LETTERS[1:4])

    expect_warning(
        new_round_robin_bracket(teams, home_away = TRUE, n_rounds = 3),
        "odd"
    )
})

test_that("round robin does not warn on odd n_rounds when home_away is FALSE", {
    teams <- paste("Team", LETTERS[1:4])

    expect_silent(
        new_round_robin_bracket(teams, home_away = FALSE, n_rounds = 3)
    )
})

test_that("round robin home_away defaults to two meetings per pair", {
    teams <- paste("Team", LETTERS[1:4])
    b <- new_round_robin_bracket(teams, home_away = TRUE)

    pairs <- vapply(b$matches, function(m) {
        paste(sort(c(m$participant1, m$participant2)), collapse = "-")
    }, character(1))
    counts <- table(pairs)
    expect_true(all(as.integer(counts) == 2L))
})
