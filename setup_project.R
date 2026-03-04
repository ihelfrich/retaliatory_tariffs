args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check-only" %in% args

resolve_project_root <- function() {
  if (file.exists("README.md") && dir.exists("data/data_cleaning_code")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }

  script_file <- NULL
  cli_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", cli_args, value = TRUE)
  if (length(file_flag) > 0) {
    script_file <- sub("^--file=", "", file_flag[1])
  } else {
    frame_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
    if (!is.null(frame_file)) {
      script_file <- frame_file
    }
  }

  if (!is.null(script_file)) {
    script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
    if (file.exists(file.path(script_dir, "README.md")) &&
        dir.exists(file.path(script_dir, "data", "data_cleaning_code"))) {
      return(script_dir)
    }
  }

  stop("Could not locate project root. Run setup_project.R from the repository root.")
}

project_root <- resolve_project_root()
setwd(project_root)

options(repos = c(CRAN = "https://cloud.r-project.org"))

cran_packages <- c(
  "readr",
  "dplyr",
  "purrr",
  "tidyr",
  "stringr",
  "jsonlite",
  "httr2",
  "ggplot2",
  "scales",
  "forcats",
  "shiny",
  "DT",
  "sf",
  "tigris",
  "viridis",
  "broom",
  "sandwich",
  "lmtest",
  "remotes",
  "tibble"
)

installed <- rownames(installed.packages())
missing_cran <- setdiff(cran_packages, installed)

if (length(missing_cran) > 0) {
  if (check_only) {
    message("Missing CRAN packages: ", paste(missing_cran, collapse = ", "))
  } else {
    install.packages(missing_cran)
  }
}

if (!requireNamespace("concordance", quietly = TRUE)) {
  if (check_only) {
    message("Missing GitHub package: concordance (insongkim/concordance)")
  } else {
    remotes::install_github("insongkim/concordance")
  }
}

if (check_only) {
  message("Dependency check complete.")
} else {
  message("Setup complete. From project root, run:")
  message("  source('run_all.R')")
}
