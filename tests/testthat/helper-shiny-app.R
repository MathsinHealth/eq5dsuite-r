# Shared harness for the tests of the bundled Shiny app.
#
# The app is not part of the package namespace: Shiny sources
# inst/shiny/global.R and inst/shiny/modules/*.R at startup, so the tests do
# the same into a throwaway environment. That environment is deliberately
# built so that eq5dsuite is *not* reachable on its search path, which is what
# would catch the app calling an exported function by bare name: run_app()
# loads the package but does not attach it.
#
# testthat loads helper-*.R before every test file, so both test-shiny-app.R
# and test-shiny-formatting.R use this.

skip_unless_app <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if(!dir.exists(app_dir()), "bundled Shiny app not available")
}

app_dir <- function() {
  d <- system.file("shiny", package = "eq5dsuite")
  if (!nzchar(d)) d <- test_path("..", "..", "inst", "shiny")
  d
}

# An environment sitting between the app and the global environment, in which
# every eq5dsuite export is bound to a stub that refuses to run. An app file
# that calls, say, eq5d_profile_level_summary() by bare name hits the stub;
# one that reaches it through eq5dsuite:: or getExportedValue() is unaffected.
mask_exports <- function() {
  m <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("eq5dsuite")) {
    local({
      fn <- nm
      assign(fn, function(...) {
        stop("the app reached '", fn, "' through the search path; it must ",
             "call eq5dsuite::", fn, "() explicitly", call. = FALSE)
      }, envir = m)
    })
  }
  m
}

# Source the app the way Shiny does: global.R first, then the modules (the
# analysis registry is built at source time and needs global.R's constants).
# The library() calls and the module-sourcing loop are skipped, the latter
# because it uses local = FALSE and would write to the global environment.
app_env <- function() {
  d <- app_dir()
  e <- new.env(parent = mask_exports())
  for (ex in parse(file.path(d, "global.R"))) {
    if (is.call(ex) &&
        as.character(ex[[1L]])[1L] %in% c("library", "require", "for", "rm")) {
      next
    }
    eval(ex, envir = e)
  }
  for (f in list.files(file.path(d, "modules"), pattern = "\\.R$",
                       full.names = TRUE)) {
    sys.source(f, envir = e)
  }
  e
}

DIMS <- c("mo", "sc", "ua", "pd", "ad")

# The mapping the Data page produces for example_data. The EQ-5D value
# analyses take a value column, which the user maps here or produces on the
# Calculate EQ-5D values page; `with_value = FALSE` is the state before that.
example_mapping <- function(with_value = TRUE) {
  list(eq5d_version = "3L", names_eq5d = DIMS,
       name_fu = "time", name_groupvar = "procedure", name_id = "id",
       name_vas = "vas", name_age = "ageband", name_sex = "gender",
       name_utility = if (with_value) "value" else NULL, country = "")
}

# example_data with the GB values the value analyses summarise.
valued_example_data <- function() {
  d <- eq5dsuite::example_data
  d$value <- suppressWarnings(suppressMessages(
    eq5dsuite::eq5d3l(d[, DIMS], country = "GB")))
  d
}

# rv as it stands once the user has been through Data and Validation.
processed_rv <- function(e, with_value = TRUE) {
  m <- example_mapping(with_value)
  d <- if (with_value) valued_example_data() else eq5dsuite::example_data
  shiny::reactiveValues(
    raw_data = d, mapping = m, processed_data = eq5d_apply_mapping(d, m),
    results = list(), load_example = 0L,
    # eq5d_apply_mapping() renames a mapped value column to "utility".
    value_cols = if (with_value) "utility" else character(0L))
}

# A small EQ-5D-5L dataset, standing in for an upload.
five_l_data <- function() {
  set.seed(42)
  n <- 400
  d <- data.frame(
    patient = rep(seq_len(n / 2), each = 2),
    visit   = rep(c("Baseline", "Month 6"), n / 2),
    mobility = sample(1:5, n, TRUE), selfcare = sample(1:5, n, TRUE),
    usual = sample(1:5, n, TRUE), pain = sample(1:5, n, TRUE),
    anxiety = sample(1:5, n, TRUE), eqvas = sample(0:100, n, TRUE),
    arm = rep(c("Control", "Treatment"), each = n / 2),
    age = sample(18:90, n, TRUE),
    sex = sample(c("Male", "Female"), n, TRUE),
    stringsAsFactors = FALSE)
  d$value <- suppressWarnings(suppressMessages(eq5dsuite::eq5d5l(
    d[, c("mobility", "selfcare", "usual", "pain", "anxiety")],
    country = "GB",
    dim.names = c("mobility", "selfcare", "usual", "pain", "anxiety"))))
  d
}

five_l_mapping <- function() {
  list(eq5d_version = "5L",
       names_eq5d = c("mobility", "selfcare", "usual", "pain", "anxiety"),
       name_fu = "visit", name_groupvar = "arm", name_id = "patient",
       name_vas = "eqvas", name_age = "age", name_sex = "sex",
       name_utility = "value", country = "")
}

# ---------------------------------------------------------------------------
# The app must not rely on eq5dsuite being attached
# ---------------------------------------------------------------------------

test_that("no app file calls an eq5dsuite export by bare name", {
  skip_unless_app()

  exported <- getNamespaceExports("eq5dsuite")
  files <- list.files(app_dir(), pattern = "\\.R$", recursive = TRUE,
                      full.names = TRUE)

  # Ask R's own parser which names are function calls, rather than matching
  # text: the files talk about these functions in comments and in the R code
  # they show the user, and neither is a call.
  bare <- character(0L)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% exported, ]
    if (nrow(calls) == 0L) next
    for (i in seq_len(nrow(calls))) {
      # A qualified call is preceded by :: or ::: in the token stream.
      before <- pd[pd$terminal & pd$line1 == calls$line1[i] &
                     pd$col2 < calls$col1[i], ]
      qualified <- nrow(before) > 0L &&
        before$token[nrow(before)] %in% c("NS_GET", "NS_GET_INT")
      if (!qualified) {
        bare <- c(bare, sprintf("%s:%d %s()", basename(f),
                                calls$line1[i], calls$text[i]))
      }
    }
  }
  expect_identical(bare, character(0L))
})

test_that("global.R does not attach eq5dsuite", {
  skip_unless_app()
  txt <- readLines(file.path(app_dir(), "global.R"), warn = FALSE)
  expect_length(grep("^\\s*(library|require)\\s*\\(\\s*[\"']?eq5dsuite", txt), 0L)
})

test_that("the masked environment would catch a bare call", {
  skip_unless_app()
  e <- app_env()
  # Sanity check on the harness: a bare name resolves to the stub, so every
  # test below runs as if the package were not attached.
  expect_error(get("eq5d_profile_level_summary", envir = e)(),
               "must call eq5dsuite::", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# The analysis registry
# ---------------------------------------------------------------------------

test_that("every registered analysis names a real eq5dsuite export", {
  skip_unless_app()
  e <- app_env()
  fns <- vapply(e$ANALYSES, `[[`, character(1L), "fn")
  expect_identical(setdiff(fns, getNamespaceExports("eq5dsuite")), character(0L))
})

test_that("the registry covers all three components with unique ids", {
  skip_unless_app()
  e <- app_env()
  ids <- vapply(e$ANALYSES, `[[`, character(1L), "id")
  expect_false(anyDuplicated(ids) > 0L)
  comp <- table(vapply(e$ANALYSES, `[[`, character(1L), "component"))
  expect_equal(as.integer(comp[c("profile", "values", "vas")]), c(19L, 7L, 4L))
  # Every component's Output select is non-empty and grouped.
  for (cp in c("profile", "values", "vas")) {
    ch <- e$analysis_choices(cp)
    expect_gt(length(ch), 0L)
    expect_gt(length(unlist(ch)), 0L)
  }
})

fake_results <- function(labels) {
  lapply(seq_along(labels), function(i)
    list(id = paste0("r", i), timestamp = Sys.time(), label = labels[i],
         fn_call = "f()", result_type = "table",
         data = data.frame(x = i), plot = NULL))
}

# Put the path of the data file into a generated script, in place of the
# placeholder the script is written with. The line is replaced by exact
# match, never through a regular expression: in sub()'s replacement a
# backslash is an escape, so a deparsed Windows path ("C:\\Users\\...") came
# out as "C:\Users\..." and the script failed to parse with "'\U' used
# without hex digits". The path is written by .r_string(), the package's one
# way of writing a string into R code.
fill_data_path <- function(lines, path) {
  at <- lines == 'data_path <- "REPLACE BY ACTUAL PATH"'
  lines[at] <- paste0("data_path <- ", eq5dsuite:::.r_string(path))
  lines
}

