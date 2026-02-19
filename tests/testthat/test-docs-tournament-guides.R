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

test_that("lifecycle vignette documents new-api workflow", {
    files <- c("vignettes/tournament-lifecycle.Rmd")

    lifecycle_patterns <- c(
        "\\bDefine\\b",
        "\\bValidate\\b",
        "\\bBuild\\b",
        "Enter results",
        "\\bRun\\b",
        "\\bInspect\\b",
        "tournament\\(",
        "spec\\(",
        "build\\(",
        "validate\\(",
        "stage_status\\(",
        "matches\\(",
        "standings\\(",
        "routing_log\\(",
        "advance\\(",
        "top_n\\("
    )

    for (path in files) {
        resolved <- resolve_repo_file(path)
        skip_if(is.null(resolved), paste("Could not locate file:", path))
        text <- paste(readLines(resolved, warn = FALSE), collapse = "\n")

        for (pattern in lifecycle_patterns) {
            expect_true(
                grepl(pattern, text, perl = TRUE),
                info = paste0(path, " missing required pattern: ", pattern)
            )
        }

        expect_true(
            !grepl("\\[debug\\]", text),
            info = paste0(path, " contains debug tags")
        )
    }
})

test_that("Error catalog vignette covers required diagnostics", {
    files <- c("vignettes/error-catalog.Rmd")

    required_patterns <- c(
        "infeasible transition count",
        "top_per_group\\(",
        "previous_stage\\(",
        "overlapping selectors",
        "overwrite blocked",
        "duplicate stage IDs",
        "invalid `score` argument"
    )

    for (path in files) {
        resolved <- resolve_repo_file(path)
        skip_if(is.null(resolved), paste("Could not locate file:", path))
        text <- paste(readLines(resolved, warn = FALSE), collapse = "\n")

        for (pattern in required_patterns) {
            expect_true(
                grepl(pattern, text, perl = TRUE, ignore.case = TRUE),
                info = paste0(path, " missing required pattern: ", pattern)
            )
        }
    }
})

test_that("lifecycle vignette includes deep function walkthrough", {
    path <- "vignettes/tournament-lifecycle.Rmd"
    resolved <- resolve_repo_file(path)
    skip_if(is.null(resolved), paste("Could not locate file:", path))
    text <- paste(readLines(resolved, warn = FALSE), collapse = "\n")

    deep_dive_patterns <- c(
        "Mental model",
        "tournament\\(",
        "spec\\(",
        "round_robin\\(",
        "single_elim\\(",
        "top_n\\(",
        "bottom_n\\(",
        "slice_range\\(",
        "remaining\\(",
        "losers\\(",
        "filter_by\\(",
        "validate\\(",
        "build\\(",
        "result\\(",
        "results\\(",
        "teardown\\(",
        "advance\\(",
        "matches\\(",
        "standings\\(",
        "stage_status\\(",
        "winner\\(",
        "rankings\\(",
        "routing_log\\("
    )

    for (pattern in deep_dive_patterns) {
        expect_true(
            grepl(pattern, text, perl = TRUE),
            info = paste0(path, " missing deep-dive pattern: ", pattern)
        )
    }
})

test_that("NHL vignette uses a single tournament workflow", {
    path <- "vignettes/nhl-stanley-cup.Rmd"
    resolved <- resolve_repo_file(path)
    skip_if(is.null(resolved), paste("Could not locate file:", path))
    text <- paste(readLines(resolved, warn = FALSE), collapse = "\n")

    required_patterns <- c(
        "tournament\\(",
        "single_elim\\(",
        "result\\(",
        "matches\\(",
        "standings\\(",
        "winner\\(trn\\)"
    )

    for (pattern in required_patterns) {
        expect_true(
            grepl(pattern, text, perl = TRUE),
            info = paste0(path, " missing single-spec pattern: ", pattern)
        )
    }

    forbidden_patterns <- c(
        "\\beastern_bracket\\b",
        "\\bwestern_bracket\\b",
        "\\bcup_final\\b"
    )

    for (pattern in forbidden_patterns) {
        expect_false(
            grepl(pattern, text, perl = TRUE),
            info = paste0(path, " contains multi-bracket artifact: ", pattern)
        )
    }
})
