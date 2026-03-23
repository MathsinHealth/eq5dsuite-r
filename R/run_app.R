#' Launch the eq5dsuite Shiny Application
#'
#' Opens an interactive Shiny application for uploading, processing,
#' analysing, and exporting EQ-5D data using the eq5dsuite package.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   such as \code{port} or \code{launch.browser}.
#' @return Called for its side effect of launching a Shiny application.
#'   Returns invisibly.
#' @export
#' @examples
#' \dontrun{
#'   eq5dsuite::run_app()
#' }
run_app <- function(...) {
  required <- c("shiny", "bslib", "DT", "readxl")
  missing  <- required[
    !vapply(required, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "The following packages must be installed to run the eq5dsuite app:\n",
      paste0("  - ", missing, collapse = "\n"),
      "\n\nInstall with:\n  install.packages(c(",
      paste0('"', missing, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }
  app_dir <- system.file("shiny", package = "eq5dsuite")
  if (!nzchar(app_dir)) {
    stop(
      "Shiny app directory not found. Try reinstalling eq5dsuite.",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
