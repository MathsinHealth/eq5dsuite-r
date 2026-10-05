# A saved result reproduces in the generated script on the same rows.
#
# The VAS summary can be restricted to one group. The app filtered its data
# before the call, but the record the script is built from kept only the
# function and its arguments, so the script analysed every row: on
# example_data, 1,878 at each visit in the app and 5,000 in the script.

q <- function(expr) suppressWarnings(suppressMessages(expr))

# example_data with group labels a script has to quote with care, and some
# rows with no group at all.
awkward_groups_csv <- function() {
  d <- example_data
  relabel <- c("Groin Hernia"     = NA,
               "Hip Replacement"  = 'Hip "total" replacement',
               "Knee Replacement" = "It's a knee",
               "Varicose Vein"    = "Varicose vein, left  side")
  d$procedure <- unname(relabel[d$procedure])
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(d, f, row.names = FALSE)
  f
}

# Load and map a CSV in the app, as a user would.
mapped_rv <- function(e, path) {
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(file = list(name = basename(path), datapath = path))
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
  rv
}

# Save one VAS summary restricted to `group` ("All" for no restriction).
save_vas <- function(e, rv, group) {
  # The selector's value for the group ("All": no restriction).
  rv[["t_group"]] <- if (identical(group, "All")) e$GROUP_FILTER_ALL
                     else e$group_filter_value(group)
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "vas")
    session$setInputs(output = "21")
    session$setInputs(group_filter = shiny::isolate(rv$t_group))
    session$setInputs(run = 1)
  }))
}

total_sample <- function(tab)
  unlist(tab[tab[[1]] == "Total sample", -1L], use.names = FALSE)

script_env <- function(rv) {
  lines <- eq5dsuite:::script_from_session(shiny::isolate(rv$steps),
                                           shiny::isolate(rv$results))
  f <- withr::local_tempfile(fileext = ".R")
  lines <- fill_data_path(lines, shiny::isolate(rv$t_path))
  writeLines(lines, f)
  env <- new.env(parent = globalenv())
  q(sys.source(f, envir = env))
  list(env = env, lines = lines)
}

test_that("every group filter is reproduced by the script, unrounded", {
  skip_unless_app()
  e <- app_env()
  path <- awkward_groups_csv()
  rv <- mapped_rv(e, path)
  rv[["t_path"]] <- path

  groups <- c('Hip "total" replacement', "It's a knee",
              "Varicose vein, left  side", "All")
  for (g in groups) save_vas(e, rv, g)
  results <- shiny::isolate(rv$results)
  expect_length(results, length(groups))

  # The app itself: each group's sample is that group's rows, per visit, and
  # a row with no group is in no group -- not a row of NAs.
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(groups)) {
    g <- groups[i]
    rows <- if (g == "All") raw else raw[which(raw$procedure == g), ]
    expect_equal(total_sample(results[[i]]$data),
                 as.numeric(table(factor(rows$time, c("Pre-op", "Post-op")))),
                 info = g)
  }

  # The script reproduces every one of them exactly.
  run <- script_env(rv)
  objs <- eq5dsuite:::.result_object_names(results)
  for (i in seq_along(results))
    expect_equal(run$env[[objs[i]]], results[[i]]$data, tolerance = 0,
                 info = groups[i])

  # Restricting one result does not restrict the data the others use.
  expect_identical(nrow(run$env$analysis_data), nrow(raw))
})

test_that("reordering saved results keeps each one with its own filter", {
  skip_unless_app()
  e <- app_env()
  path <- awkward_groups_csv()
  rv <- mapped_rv(e, path)
  rv[["t_path"]] <- path

  for (g in c("It's a knee", "All", 'Hip "total" replacement'))
    save_vas(e, rv, g)
  shiny::isolate(rv$results <- rev(rv$results))
  results <- shiny::isolate(rv$results)

  run <- script_env(rv)
  objs <- eq5dsuite:::.result_object_names(results)
  for (i in seq_along(results))
    expect_equal(run$env[[objs[i]]], results[[i]]$data, tolerance = 0)
  # And the three really are different populations.
  expect_length(unique(lapply(results, function(r) total_sample(r$data))), 3L)
})

test_that("the restriction is written out where a reader can see it", {
  skip_unless_app()
  e <- app_env()
  path <- awkward_groups_csv()
  rv <- mapped_rv(e, path)
  rv[["t_path"]] <- path
  save_vas(e, rv, "It's a knee")

  run <- script_env(rv)
  expect_true(any(grepl("It's a knee", run$lines, fixed = TRUE)))
  expect_silent(parse(text = paste(run$lines, collapse = "\n")))
  # The code shown in the app says so too.
  shown <- shiny::isolate(rv$results)[[1]]$fn_call
  expect_match(shown, "It's a knee", fixed = TRUE)
})

test_that("no restriction leaves the call on the whole data, as before", {
  skip_unless_app()
  e <- app_env()
  path <- awkward_groups_csv()
  rv <- mapped_rv(e, path)
  rv[["t_path"]] <- path
  save_vas(e, rv, "All")

  rec <- shiny::isolate(rv$results)[[1]]$call
  expect_null(rec$filter)
  run <- script_env(rv)
  # The call is on the shared data, with no per-result copy.
  expect_true(any(grepl("df = analysis_data\\b", run$lines)))
  expect_false(any(grepl("vas_summary_data", run$lines, fixed = TRUE)))
})
