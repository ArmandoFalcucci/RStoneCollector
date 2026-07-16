# install.R  -  run once to set up packages
# Usage:  Rscript install.R     (from terminal)
#     or: source("install.R")   (inside R)

pkgs <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",   # shinyWidgets is NEW
  "DT",
  "ggplot2", "dplyr", "readr", "tidyr",
  "jsonlite", "later", "rlang", "scales",
  "base64enc",   # for IMAGE field preview
  "testthat"     # for unit tests in tests/testthat/
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("All required packages already installed.")
}

invisible(lapply(pkgs, function(p) {
  ok <- requireNamespace(p, quietly = TRUE)
  message(if (ok) "[ok] " else "[FAIL] ", p)
}))

cat("\nLaunch RStone from this folder:\n  shiny::runApp()\n",
    "\nRun unit tests:\n  testthat::test_dir('tests/testthat')\n\n", sep = "")
