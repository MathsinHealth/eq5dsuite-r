# The app's Health Profile Grid is reproduced by the generated script.
#
# The HPG is the one analysis that takes a value set rather than a value
# column, so its record carries the instrument, the country and the two
# timepoints. The script must rebuild the same grid from them.

q <- function(expr) suppressWarnings(suppressMessages(expr))

hpg_session <- function(e, fu_levels, country = "GB") {
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    session$setInputs(version = "3L", col_mo = "mo", col_sc = "sc",
                      col_ua = "ua", col_pd = "pd", col_ad = "ad",
                      col_fu = "time", col_groupvar = "procedure",
                      col_id = "id", col_vas = "vas", col_age = "",
                      col_sex = "", col_utility = "",
                      fu_order = c("Pre-op", "Post-op"), confirm = 1)
  }))
  q(shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    session$setInputs(proceed = 1)
  }))
  rv[["t_hpg"]] <- list(fu = fu_levels, country = country)
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    a <- shiny::isolate(rv$t_hpg)
    session$setInputs(component = "profile")
    session$setInputs(output = "125fig")
    session$setInputs(country = a$country, fu_levels = a$fu)
    session$setInputs(run = 1)
  }))
  rv
}

run_generated <- function(rv) {
  lines <- eq5dsuite:::script_from_session(shiny::isolate(rv$steps),
                                           shiny::isolate(rv$results))
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(lines, f)
  env <- new.env(parent = globalenv())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  q(sys.source(f, envir = env))
  env
}

test_that("the script rebuilds the app's Health Profile Grid exactly", {
  skip_unless_app()
  e <- app_env()
  rv <- hpg_session(e, c("Pre-op", "Post-op"))
  res <- shiny::isolate(rv$results)
  expect_length(res, 1L)
  expect_identical(res[[1]]$call$fn, "eq5d_profile_health_profile_grid")
  expect_identical(res[[1]]$call$args$country, "GB")

  env <- run_generated(rv)
  obj <- eq5dsuite:::.result_object_names(res)
  got <- env[[obj]]
  expect_s3_class(got$p, "ggplot")
  # The points plotted, and the plot's own data, are the app's.
  expect_equal(got$p$data, res[[1]]$plot$data, tolerance = 0)
  expect_equal(ggplot2::layer_data(got$p), ggplot2::layer_data(res[[1]]$plot),
               tolerance = 0)
  expect_gt(nrow(got$plot_data), 1000L)
})

test_that("the timepoints the user chose are the ones the script compares", {
  skip_unless_app()
  e <- app_env()
  rv <- hpg_session(e, c("Post-op", "Pre-op"))
  res <- shiny::isolate(rv$results)
  expect_identical(res[[1]]$call$args$levels_fu, c("Post-op", "Pre-op"))
  env <- run_generated(rv)
  obj <- eq5dsuite:::.result_object_names(res)
  expect_equal(env[[obj]]$p$data, res[[1]]$plot$data, tolerance = 0)
})
