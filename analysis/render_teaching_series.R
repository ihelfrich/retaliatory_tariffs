suppressPackageStartupMessages({
  library(rmarkdown)
})

resolve_project_root <- function() {
  if (file.exists("README.md") && file.exists("retaliatory-tariff.Rproj")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }

  if (file.exists("../README.md") && file.exists("../retaliatory-tariff.Rproj")) {
    return(normalizePath("..", winslash = "/", mustWork = TRUE))
  }

  stop("Could not locate project root. Run from repo root or analysis/.")
}

setwd(resolve_project_root())

files <- c(
  "analysis/session1_foundations_followalong.Rmd",
  "analysis/session2_measurement_mapping_followalong.Rmd",
  "analysis/session3_models_extensions_followalong.Rmd",
  "analysis/master_tariff_teaching_workbook.Rmd",
  "analysis/student_workbook_client_facing.Rmd"
)

for (f in files) {
  message("Rendering: ", f)
  render(
    input = f,
    output_format = "html_document",
    clean = TRUE,
    envir = new.env(parent = globalenv())
  )
}

message("Done. Teaching series HTML files are in analysis/.")
