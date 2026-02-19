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

test_that("Swiss top-cut vignette artifacts exist and are DSL-oriented", {
    files <- c("vignettes/swiss-top-cut.Rmd")

    for (path in files) {
        resolved <- resolve_repo_file(path)
        skip_if(is.null(resolved), paste("Could not locate file:", path))
        text <- paste(readLines(resolved, warn = FALSE), collapse = "\n")

        expect_true(grepl("Swiss", text), info = paste(path, "missing Swiss context"))
        expect_true(grepl("top-cut|Top-Cut", text), info = paste(path, "missing top-cut context"))
        expect_true(grepl("LoL|League of Legends|Worlds", text), info = paste(path, "missing LoL Worlds context"))
        expect_true(grepl("validate\\(", text), info = paste(path, "missing validate example"))
        expect_true(grepl("result\\(", text), info = paste(path, "missing result example"))
        expect_true(grepl("routing_log\\(", text), info = paste(path, "missing routing_log example"))
        expect_true(grepl("winner\\(", text), info = paste(path, "missing winner extraction"))
        expect_true(grepl("winner\\(|rankings\\(|Outcomes", text), info = paste(path, "missing full simulation completion context"))
        expect_false(grepl("\\[debug\\]", text), info = paste(path, "contains debug tags"))
    }
})

test_that("no generated markdown vignette artifacts are committed", {
    vignette_dir <- resolve_repo_file("vignettes")
    skip_if(is.null(vignette_dir), "Could not locate vignettes directory")
    md_files <- list.files(vignette_dir, pattern = "\\.md$", full.names = FALSE)

    expect_equal(
        length(md_files),
        0L,
        info = paste("remove:", paste(md_files, collapse = ", "))
    )
})
