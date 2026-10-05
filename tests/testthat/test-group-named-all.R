# A group that is really called "All" is a group, not "every group".
#
# The VAS summary's "Restrict to group" selector used the value "All" for
# "no restriction" and each group's label as its value, so a group labelled
# "All" had the same value as the no-restriction choice. Choosing it analysed
# every row, and the saved result and the generated script said so. These
# tests pick options the way a user does -- from the selector the app renders,
# by the label shown -- rather than by assuming what the values are.

q <- function(expr) suppressWarnings(suppressMessages(expr))

csv_with_group_all <- function() {
  d <- example_data
  relabel <- c("Groin Hernia"     = NA,
               "Hip Replacement"  = "Hip",
               "Knee Replacement" = "Knee",
               "Varicose Vein"    = "All")
  d$procedure <- unname(relabel[d$procedure])
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(d, f, row.names = FALSE)
  f
}

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

# The options of the rendered "Restrict to group" selector: label -> value.
group_options <- function(e, rv) {
  out <- NULL
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "vas")
    session$setInputs(output = "21")
    html <- as.character(output$opts$html)
    sel <- regmatches(html, regexpr('<select[^>]*group_filter.*?</select>',
                                    html))
    m <- regmatches(sel, gregexpr('<option value="([^"]*)"[^>]*>([^<]*)</option>',
                                  sel))[[1]]
    out <<- stats::setNames(sub('^<option value="([^"]*)".*$', "\\1", m),
                            sub('^.*>([^<]*)</option>$', "\\1", m))
  }))
  out
}

# Save a VAS summary with the option whose value is `value`.
save_vas <- function(e, rv, value) {
  rv[["t_value"]] <- value
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "vas")
    session$setInputs(output = "21")
    session$setInputs(group_filter = shiny::isolate(rv$t_value))
    session$setInputs(run = 1)
  }))
}

total_sample <- function(tab)
  unlist(tab[tab[[1]] == "Total sample", -1L], use.names = FALSE)

per_visit <- function(rows)
  as.numeric(table(factor(rows$time, c("Pre-op", "Post-op"))))

test_that("the selector offers 'every group' and the group 'All' separately", {
  skip_unless_app()
  e <- app_env()
  rv <- mapped_rv(e, csv_with_group_all())
  opts <- group_options(e, rv)
  # One option per group plus the no-restriction one, all with distinct values.
  expect_identical(sort(setdiff(names(opts), "All groups")),
                   c("All", "Hip", "Knee"))
  expect_false(anyDuplicated(unname(opts)) > 0L)
  expect_identical(names(opts)[1], "All groups")
})

test_that("choosing the group 'All' analyses that group only, as does the script", {
  skip_unless_app()
  e <- app_env()
  path <- csv_with_group_all()
  rv <- mapped_rv(e, path)
  opts <- group_options(e, rv)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)

  save_vas(e, rv, opts[["All"]])         # the group called "All"
  save_vas(e, rv, opts[["All groups"]])  # no restriction
  res <- shiny::isolate(rv$results)
  expect_length(res, 2L)

  # The app: the group "All" is the 454 varicose-vein rows; no restriction is
  # every row, including those with no group.
  expect_equal(total_sample(res[[1]]$data),
               per_visit(raw[which(raw$procedure == "All"), ]))
  expect_equal(total_sample(res[[2]]$data), per_visit(raw))
  expect_false(isTRUE(all.equal(res[[1]]$data, res[[2]]$data)))
  expect_identical(res[[1]]$call$filter$value, "All")
  expect_null(res[[2]]$call$filter)

  # The script reproduces both, unrounded.
  lines <- eq5dsuite:::script_from_session(shiny::isolate(rv$steps), res)
  lines <- fill_data_path(lines, path)
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(lines, f)
  env <- new.env(parent = globalenv())
  q(sys.source(f, envir = env))
  objs <- eq5dsuite:::.result_object_names(res)
  expect_equal(env[[objs[1]]], res[[1]]$data, tolerance = 0)
  expect_equal(env[[objs[2]]], res[[2]]$data, tolerance = 0)
})

test_that("every other group still selects its own rows", {
  skip_unless_app()
  e <- app_env()
  path <- csv_with_group_all()
  rv <- mapped_rv(e, path)
  opts <- group_options(e, rv)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)
  for (g in c("Hip", "Knee")) save_vas(e, rv, opts[[g]])
  res <- shiny::isolate(rv$results)
  expect_equal(total_sample(res[[1]]$data),
               per_visit(raw[which(raw$procedure == "Hip"), ]))
  expect_equal(total_sample(res[[2]]$data),
               per_visit(raw[which(raw$procedure == "Knee"), ]))
})
