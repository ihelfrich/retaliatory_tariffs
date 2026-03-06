args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check-only" %in% args
build_teaching <- "--build-teaching" %in% args

resolve_project_root <- function() {
  if (file.exists("README.md") && file.exists("setup_project.R")) {
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
        file.exists(file.path(script_dir, "setup_project.R"))) {
      return(script_dir)
    }
  }

  stop("Could not locate project root. Run run_all.R from the repository root.")
}

project_root <- resolve_project_root()
setwd(project_root)

source("setup_project.R")

core_scripts <- c(
  "data/data_cleaning_code/build_county_exposure.R",
  "data/data_cleaning_code/clean_tariff_retaliation.R",
  "code/comprehensive_results_geospatial.R",
  "code/overhaul_open_data_panel.R",
  "code/validate_core_outputs.R",
  "code/build_analysis_outputs.R",
  "code/build_site_data.R",
  "code/build_bivariate_maps.R",
  "code/build_fixest_models.R"
)

missing_scripts <- core_scripts[!file.exists(core_scripts)]
qa_scripts <- c("code/check_client_text.R", "analysis/render_teaching_series.R")
missing_qa_scripts <- qa_scripts[!file.exists(qa_scripts)]
if (length(missing_scripts) > 0) {
  stop("Missing required scripts: ", paste(missing_scripts, collapse = ", "))
}
if (length(missing_qa_scripts) > 0) {
  stop("Missing QA scripts: ", paste(missing_qa_scripts, collapse = ", "))
}

if (check_only) {
  message("run_all.R check complete. Core scripts are available.")
  quit(save = "no", status = 0)
}

for (script_path in core_scripts) {
  message("Running ", script_path)
  source(script_path)
}

message("Running code/check_client_text.R")
source("code/check_client_text.R")

if (file.exists("analysis/report.qmd") &&
    requireNamespace("quarto", quietly = TRUE) &&
    !is.null(quarto::quarto_path())) {
  message("Rendering analysis/report.qmd")
  quarto::quarto_render("analysis/report.qmd")
} else {
  message("Skipping Quarto render (analysis/report.qmd missing or Quarto unavailable).")
}

donations_cache <- "./data/public/raw/fec_donations_by_zip_focus_committees.csv"
spending_cache <- "./data/public/raw/fec_spending_by_zip_focus_committees.csv"
has_fec_key <- nzchar(Sys.getenv("FEC_API_KEY"))
has_fec_cache <- file.exists(donations_cache) && file.exists(spending_cache)

if (has_fec_key || has_fec_cache) {
  source("code/add_campaign_finance_county_measures.R")
} else {
  message("Skipping campaign-finance extension (no FEC_API_KEY and no cached FEC ZIP files).")
}

if (build_teaching) {
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    message("Rendering teaching notebook HTML files")
    source("analysis/render_teaching_series.R")
  } else {
    message("Skipping teaching notebook render (--build-teaching set, but rmarkdown is unavailable).")
  }
} else {
  message("Skipping teaching notebook render (use --build-teaching to include it).")
}

message("Pipeline finished.")
