resolve_repo_file <- function(path) {
    candidates <- c(
        path,
        file.path("..", path),
        file.path("..", "..", path)
    )

    for (candidate in candidates) {
        if (file.exists(candidate)) {
            return(candidate)
        }
    }

    NULL
}

test_that("FIFA vignette avoids classic-format positioning", {
    files <- c("vignettes/fifa-world-cup.Rmd")

    banned_patterns <- c(
        "Quick Start: Classic Formats",
        "Classic API",
        "classic group-to-knockout",
        "group_stage_knockout\\(",
        "new_single_elim_bracket\\(",
        "new_double_elim_bracket\\(",
        "new_round_robin_bracket\\(",
        "new_swiss_bracket\\(",
        "new_two_leg_bracket\\(",
        "new_group_stage_knockout_bracket\\("
    )

    for (path in files) {
        resolved <- resolve_repo_file(path)
        skip_if(is.null(resolved), paste("Could not locate file:", path))
        text <- paste(readLines(resolved, warn = FALSE), collapse = "\n")
        for (pattern in banned_patterns) {
            expect_false(
                grepl(pattern, text, perl = TRUE),
                info = paste0(path, " contains banned pattern: ", pattern)
            )
        }
        expect_true(
            !grepl("\\[debug\\]", text),
            info = paste0(path, " contains debug tags")
        )
    }
})
