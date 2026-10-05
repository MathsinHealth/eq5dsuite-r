# Saved results and the generated script describe the same data.
#
# A result is computed from one dataset, one mapping and the value columns
# that existed when it was run. Confirming a new dataset or mapping starts a
# new revision: everything computed from the previous one -- saved results,
# calculated value columns and the steps that produced them -- is cleared.
# Merely choosing a file is not confirming it, and changes nothing. A value
# column that is overwritten takes with it the saved results that used it.
#
# Before, an old result survived a new dataset, and the script recalculated
# it against the new one; choosing a file replaced the recorded load step at
# once, so the script described data the analysis had never seen.

q <- function(expr) suppressWarnings(suppressMessages(expr))

new_rv <- function() {
  shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                        processed_data = NULL, results = list(),
                        load_example = 0L, value_cols = character(0L),
                        steps = list())
}

map_inputs <- function(groupvar = "procedure") {
  list(version = "3L", col_mo = "mo", col_sc = "sc", col_ua = "ua",
       col_pd = "pd", col_ad = "ad", col_fu = "time",
       col_groupvar = groupvar, col_id = "id", col_vas = "vas",
       col_age = "ageband", col_sex = "gender", col_utility = "",
       fu_order = c("Pre-op", "Post-op"))
}

# Load (example or file), map and confirm, then validate.
load_and_confirm <- function(e, rv, path = NULL, groupvar = "procedure") {
  rv[["t_path"]] <- path
  rv[["t_map"]] <- map_inputs(groupvar)
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    p <- shiny::isolate(rv$t_path)
    if (is.null(p)) session$setInputs(use_example = 1)
    else session$setInputs(file = list(name = basename(p), datapath = p))
    do.call(session$setInputs, shiny::isolate(rv$t_map))
    session$setInputs(confirm = 1)
  }))
  q(shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    session$setInputs(proceed = 1)
  }))
}

# Choose a file and set the mapping, without confirming.
choose_only <- function(e, rv, path) {
  rv[["t_path"]] <- path
  rv[["t_map"]] <- map_inputs()
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(file = list(name = basename(shiny::isolate(rv$t_path)),
                                  datapath = shiny::isolate(rv$t_path)))
    do.call(session$setInputs, shiny::isolate(rv$t_map))
  }))
}

add_value <- function(e, rv, country, col = "utility") {
  rv[["t_vs"]] <- c(country, col)
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    v <- shiny::isolate(rv$t_vs)
    session$setInputs(method = "direct", country = v[1], col_name = v[2],
                      add = 1)
  }))
}

run_output <- function(e, rv, component, output, utility_col = NULL) {
  rv[["t_run"]] <- list(component, output, utility_col)
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    r <- shiny::isolate(rv$t_run)
    session$setInputs(component = r[[1]])
    session$setInputs(output = r[[2]])
    if (!is.null(r[[3]])) session$setInputs(utility_col = r[[3]])
    session$setInputs(run = 1)
  }))
}

kinds <- function(rv) vapply(shiny::isolate(rv$steps), `[[`, "", "kind")

small_csv <- function() {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(example_data[1:2000, ], f, row.names = FALSE)
  f
}

# A session on example_data with a value column and two saved results.
populated <- function(e) {
  rv <- new_rv()
  load_and_confirm(e, rv)
  add_value(e, rv, "GB")
  run_output(e, rv, "profile", "111")
  run_output(e, rv, "values", "31", utility_col = "utility")
  rv
}

test_that("choosing a file without confirming changes nothing", {
  skip_unless_app()
  e <- app_env()
  rv <- populated(e)
  before <- shiny::isolate(list(results = rv$results, steps = rv$steps,
                                data = rv$processed_data,
                                mapping = rv$mapping,
                                value_cols = rv$value_cols))
  expect_length(before$results, 2L)

  choose_only(e, rv, small_csv())

  after <- shiny::isolate(list(results = rv$results, steps = rv$steps,
                               data = rv$processed_data,
                               mapping = rv$mapping,
                               value_cols = rv$value_cols))
  expect_identical(after, before)
  # The script still describes the data the results came from.
  load <- Filter(function(s) s$kind == "load", after$steps)[[1]]
  expect_identical(load$source, "example")
})

test_that("confirming a new dataset clears what the old one produced", {
  skip_unless_app()
  e <- app_env()
  rv <- populated(e)
  path <- small_csv()

  load_and_confirm(e, rv, path)

  expect_length(shiny::isolate(rv$results), 0L)
  expect_identical(kinds(rv), c("load", "map"))
  expect_identical(shiny::isolate(rv$value_cols), character(0L))
  expect_false("utility" %in% names(shiny::isolate(rv$processed_data)))
  expect_identical(nrow(shiny::isolate(rv$processed_data)), 2000L)
  load <- shiny::isolate(rv$steps)[[1]]
  expect_identical(load$source, "file")
  expect_identical(load$file, basename(path))

  # A result saved now is reproduced by the script on the new data.
  run_output(e, rv, "profile", "111")
  lines <- eq5dsuite:::script_from_session(shiny::isolate(rv$steps),
                                           shiny::isolate(rv$results))
  lines <- fill_data_path(lines, path)
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(lines, f)
  env <- new.env(parent = globalenv())
  q(sys.source(f, envir = env))
  expect_identical(nrow(env$analysis_data), 2000L)
  expect_equal(env$profile_level_summary,
               shiny::isolate(rv$results)[[1]]$data, tolerance = 0)
})

test_that("confirming a new mapping of the same data clears it too", {
  skip_unless_app()
  e <- app_env()
  rv <- populated(e)

  load_and_confirm(e, rv, groupvar = "year")

  expect_length(shiny::isolate(rv$results), 0L)
  expect_identical(kinds(rv), c("load", "map"))
  expect_identical(shiny::isolate(rv$mapping$name_groupvar), "year")
})

test_that("an example dataset loaded but not confirmed changes nothing", {
  skip_unless_app()
  e <- app_env()
  rv <- new_rv()
  path <- small_csv()
  load_and_confirm(e, rv, path)
  run_output(e, rv, "profile", "111")
  before <- shiny::isolate(list(rv$results, rv$steps, rv$processed_data))

  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
  }))

  expect_identical(shiny::isolate(list(rv$results, rv$steps,
                                       rv$processed_data)), before)
})

test_that("overwriting a value column takes the results that used it", {
  skip_unless_app()
  e <- app_env()
  rv <- populated(e)
  labels <- function() vapply(shiny::isolate(rv$results), `[[`, "", "label")
  expect_length(labels(), 2L)

  add_value(e, rv, "DK")

  # The profile table did not use the column and stays; the value summary did.
  res <- shiny::isolate(rv$results)
  expect_length(res, 1L)
  expect_identical(res[[1]]$call$fn, "eq5d_profile_level_summary")

  # One value step for the column, the new one.
  vals <- Filter(function(s) s$kind == "value", shiny::isolate(rv$steps))
  expect_length(vals, 1L)
  expect_identical(vals[[1]]$country, "DK")

  # A summary run now is on the new values, and the script agrees.
  run_output(e, rv, "values", "31", utility_col = "utility")
  lines <- eq5dsuite:::script_from_session(shiny::isolate(rv$steps),
                                           shiny::isolate(rv$results))
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(lines, f)
  env <- new.env(parent = globalenv())
  q(sys.source(f, envir = env))
  res <- shiny::isolate(rv$results)
  objs <- eq5dsuite:::.result_object_names(res)
  for (i in seq_along(res))
    expect_equal(env[[objs[i]]], res[[i]]$data, tolerance = 0)
  expect_identical(env$analysis_data$utility,
                   shiny::isolate(rv$processed_data$utility))
})

test_that("adding a different value column keeps every result", {
  skip_unless_app()
  e <- app_env()
  rv <- populated(e)
  add_value(e, rv, "DK", col = "utility_dk")
  expect_length(shiny::isolate(rv$results), 2L)
  expect_length(Filter(function(s) s$kind == "value",
                       shiny::isolate(rv$steps)), 2L)
})
