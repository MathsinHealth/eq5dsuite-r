# The name offered for a new EQ-5D value column says what it holds:
# utility_<METHOD>_<CODE>, with the value set's code, not its label. It
# follows the method and value set until the user types a name of their own,
# which is kept. A name that is already a column is handled openly: a value
# column may be replaced (and the note says what that removes); any other
# column of the data may not.

q <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("the convention, with the actual value-set code", {
  skip_unless_app()
  e <- app_env()
  expect_identical(e$suggest_value_col("direct", "CA"), "utility_CA")
  expect_identical(e$suggest_value_col("xwr", "CA"), "utility_XWR_CA")
  expect_identical(e$suggest_value_col("xw", "DK"), "utility_XW_DK")
  expect_identical(e$suggest_value_col("direct", "DE_TTO"), "utility_DE_TTO")
  expect_identical(e$suggest_value_col("direct", "NL_2006"), "utility_NL_2006")
  expect_identical(e$suggest_value_col("uk", "ignored"), "utility_DSU_GB")
  # No value set chosen yet.
  expect_identical(e$suggest_value_col("direct", ""), "utility")
  expect_identical(e$suggest_value_col("xwr", NULL), "utility_XWR")
  # Every built-in code gives a syntactic R name.
  for (v in c("3L", "5L")) {
    codes <- unname(e$get_country_choices(v))
    nm <- vapply(codes, e$suggest_value_col, "", method = "direct")
    expect_identical(make.names(nm), unname(nm), info = v)
  }
})

# Run the values module, recording every name it offers.
offered_names <- function(e, rv, steps) {
  offered <- character(0)
  local_mocked_bindings(
    updateTextInput = function(session, inputId, label = NULL, value = NULL, ...)
      if (identical(inputId, "col_name")) offered <<- c(offered, value),
    .package = "shiny")
  rv[["t_steps"]] <- steps
  # As the browser does: an offered name goes into the field.
  rv[["t_offered"]] <- function() offered
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    for (st in shiny::isolate(rv$t_steps)) {
      n <- length(shiny::isolate(rv$t_offered)())
      do.call(session$setInputs, st)
      session$flushReact()
      now <- shiny::isolate(rv$t_offered)()
      if (length(now) > n) session$setInputs(col_name = utils::tail(now, 1L))
    }
  }))
  offered
}

test_that("the offer follows the method and the value set", {
  skip_unless_app()
  e <- app_env()
  got <- offered_names(e, processed_rv(e), list(
    list(method = "direct", col_name = "utility"),
    list(country = "CA"),
    list(method = "xwr", country = "CA"),
    list(method = "uk")))
  expect_true(all(c("utility_CA", "utility_XWR_CA", "utility_DSU_GB") %in% got))
  expect_identical(utils::tail(got, 1L), "utility_DSU_GB")
})

test_that("a name the user typed is kept when the method or value set changes", {
  skip_unless_app()
  e <- app_env()
  got <- offered_names(e, processed_rv(e), list(
    list(method = "direct", col_name = "utility"),
    list(country = "CA"),                 # offered utility_CA
    list(col_name = "my_values"),         # the user's own
    list(country = "DK"),
    list(method = "xwr", country = "CA")))
  # After the user typed a name, nothing more was offered.
  expect_identical(utils::tail(got, 1L), "utility_CA")
  expect_false(any(c("utility_DK", "utility_XWR_CA") %in% got))
})

test_that("clearing the name hands it back to the suggestion", {
  skip_unless_app()
  e <- app_env()
  got <- offered_names(e, processed_rv(e), list(
    list(method = "direct", col_name = "utility"),
    list(country = "CA"),
    list(col_name = "mine"),
    list(col_name = ""),
    list(country = "DK")))
  expect_identical(utils::tail(got, 1L), "utility_DK")
})

test_that("a data column cannot be replaced, and the page says why", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  before <- shiny::isolate(rv$processed_data)
  said <- character(0)
  local_mocked_bindings(
    showNotification = function(ui, ...) said <<- c(said, as.character(ui)),
    .package = "shiny")
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB", col_name = "vas")
    expect_match(as.character(output$name_note$html),
                 "is a column of your data", fixed = TRUE)
    session$setInputs(add = 1)
  }))
  expect_identical(shiny::isolate(rv$processed_data), before)
  expect_true(any(grepl("cannot be replaced", said, fixed = TRUE)))
  # A dimension too.
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB", col_name = "mo",
                      add = 1)
  }))
  expect_identical(shiny::isolate(rv$processed_data), before)
})

test_that("an existing value column may be replaced, and the note says what goes", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e, with_value = FALSE)
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility_GB", add = 1)
  }))
  rv$results <- list(list(id = "r1", label = "x", result_type = "table",
                          call = list(fn = "eq5d_utility_summary",
                                      args = list(name_utility = "utility_GB"))))
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "DK",
                      col_name = "utility_GB")
    note <- as.character(output$name_note$html)
    expect_match(note, "already exists", fixed = TRUE)
    expect_match(note, "removes the 1 saved result that used it", fixed = TRUE)
    session$setInputs(add = 1)
  }))
  # Replaced, and the dependent result removed (the F08 safeguard).
  expect_length(shiny::isolate(rv$results), 0L)
  vals <- Filter(function(s) s$kind == "value", shiny::isolate(rv$steps))
  expect_identical(vapply(vals, `[[`, "", "country"), "DK")
})

test_that("a free name has no note", {
  skip_unless_app()
  e <- app_env()
  q(shiny::testServer(e$mod_values_server, args = list(rv = processed_rv(e)), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "brand_new")
    expect_null(output$name_note$html)
  }))
})
