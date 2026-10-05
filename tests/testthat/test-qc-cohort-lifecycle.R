# Review of 2026-10-04, group 4: cohort eligibility and the result lifecycle.
#
# Q09 the catalogue judged eligibility on every timepoint in the data, not on
#     the timepoints selected on the Data page: with levels pre, mid and pairs
#     only at pre/post, it offered the Health Profile Grid, which then failed.
# Q12 confirming new data cleared the saved results but not the result shown
#     on the Analysis page, which still showed the old data's mean.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")
avail <- function(e, rv) vapply(shiny::isolate(e$available_analyses(rv)),
                                `[[`, "", "id")

# ── Q09 ───────────────────────────────────────────────────────────────────────

review_q09 <- function(levels_fu) {
  pair <- data.frame(id = c(1, 1, 2, 2, 3), time = c("pre", "post", "pre", "post", "mid"),
                     mo = c(3, 2, 2, 1, 1), sc = 1, ua = 1, pd = 1, ad = 1)
  m <- list(names_eq5d = DIMS, eq5d_version = "3L", name_id = "id",
            name_fu = "time", levels_fu = levels_fu)
  shiny::reactiveValues(raw_data = pair, mapping = m,
                        processed_data = eq5d_apply_mapping(pair, m),
                        results = list(), value_cols = character(0),
                        steps = list())
}

PAIRED <- c("122", "123", "124", "121fig", "122fig", "123fig", "124fig", "125fig")

test_that("eligibility uses the selected timepoints, not every one in the data", {
  skip_unless_app()
  e <- app_env()
  rv <- review_q09(c("pre", "mid"))
  facts <- shiny::isolate(e$analysis_data_facts(rv))
  expect_identical(facts$timepoints, 2L)         # pre and mid; post is not selected
  expect_false(facts$paired)                      # nobody at both pre and mid
  expect_false(any(PAIRED %in% avail(e, rv)))
  # With pre and post selected, the same data support them.
  rv2 <- review_q09(c("pre", "post"))
  expect_true(shiny::isolate(e$analysis_data_facts(rv2))$paired)
  # Both respondents improved: the figures of worsening and mixed change
  # have nothing to show and are not offered.
  got <- avail(e, rv2)
  expect_true(all(setdiff(PAIRED, c("123fig", "124fig")) %in% got))
  expect_false(any(c("123fig", "124fig") %in% got))
})

test_that("every analysis offered runs on the review's data", {
  skip_unless_app()
  e <- app_env()
  for (lv in list(c("pre", "mid"), c("pre", "post"))) {
    rv <- review_q09(lv)
    for (id in avail(e, rv)) {
      s <- e$analysis_spec(id)
      p <- shiny::isolate(s$prep(list(country = "GB", fu_levels = lv, topn = 10L,
                                      group_filter = e$GROUP_FILTER_ALL), rv))
      expect_no_error(q(do.call(e$pkg_fn(s$fn), c(list(df = p$df), p$args))),
                      message = paste(paste(lv, collapse = "/"), id))
    }
  }
})

test_that("the Analysis page does not offer to run what the data cannot support", {
  skip_unless_app()
  e <- app_env()
  rv <- review_q09(c("pre", "mid"))
  rv$processed_data <- shiny::isolate(rv$processed_data)
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "profile", output = "125fig")
    html <- as.character(output$run_ui$html)
    expect_match(html, "a respondent with valid profiles at two timepoints",
                 fixed = TRUE)
    expect_false(grepl(">Run<", html, fixed = TRUE))
    # The timepoints offered are the selected ones, in their order.
    expect_identical(timepoints(), c("pre", "mid"))
  }))
})

test_that("values at excluded timepoints do not make value analyses available", {
  skip_unless_app()
  e <- app_env()
  d <- data.frame(id = 1:4, time = c("pre", "pre", "post", "post"),
                  mo = 1, sc = 1, ua = 1, pd = 1, ad = 1,
                  utility = c(NA, NA, 0.8, 0.9))
  m <- list(names_eq5d = DIMS, eq5d_version = "3L", name_id = "id",
            name_fu = "time", levels_fu = "pre", name_utility = "utility")
  rv <- shiny::reactiveValues(raw_data = d, mapping = m,
                              processed_data = eq5d_apply_mapping(d, m),
                              results = list(), value_cols = "utility",
                              steps = list())
  got <- avail(e, rv)
  expect_false("fig31" %in% got)   # values by timepoint: none at "pre"
})

# ── Q12 ───────────────────────────────────────────────────────────────────────

# The data, validation, values and analysis modules, mounted together and
# kept mounted for the whole test, as in the app.
whole_app <- function(e) function(id, rv) shiny::moduleServer(id, function(input, output, session) {
  e$mod_data_server("data", rv)
  e$mod_validation_server("validation", rv)
  e$mod_values_server("values", rv)
  e$mod_analysis_server("analysis", rv)
})

new_rv <- function() shiny::reactiveValues(
  raw_data = NULL, mapping = NULL, processed_data = NULL, results = list(),
  load_example = 0L, value_cols = character(0L), steps = list())

shown_label <- function(output) as.character(output[["analysis-result"]]$html)

test_that("one mounted app: the displayed result follows every accepted revision", {
  skip_unless_app()
  e <- app_env()
  rv <- new_rv()
  csv <- withr::local_tempfile(fileext = ".csv")
  b <- example_data[1:400, ]
  b$mo <- 3L                                     # clearly different values
  utils::write.csv(b, csv, row.names = FALSE)
  rv[["t_csv"]] <- csv
  vars <- function(groupvar = "procedure")
    list(`data-version` = "3L", `data-col_mo` = "mo", `data-col_sc` = "sc",
         `data-col_ua` = "ua", `data-col_pd` = "pd", `data-col_ad` = "ad",
         `data-col_fu` = "time", `data-col_groupvar` = groupvar,
         `data-col_id` = "id", `data-col_vas` = "vas", `data-col_age` = "",
         `data-col_sex` = "", `data-col_utility` = "",
         `data-fu_order` = c("Pre-op", "Post-op"))
  rv[["t_vars"]] <- vars()
  rv[["t_vars2"]] <- vars("year")

  q(shiny::testServer(whole_app(e), args = list(rv = rv), {
    mean_of <- function(col) mean(rv$processed_data[[col]], na.rm = TRUE)
    run_summary <- function() {
      session$setInputs(`analysis-component` = "values")
      session$setInputs(`analysis-output` = "31")
      session$setInputs(`analysis-utility_col` = "utility_GB")
      session$setInputs(`analysis-run` = (input$`analysis-run` %||% 0) + 1)
    }
    accept <- function(vars, n) {
      do.call(session$setInputs, vars)
      session$setInputs(`data-confirm` = n)
      session$setInputs(`validation-proceed` = n)
      session$setInputs(`values-method` = "direct", `values-country` = "GB",
                        `values-col_name` = "utility_GB")
      session$setInputs(`values-add` = n)
    }

    # Example data, a value column, a result shown.
    session$setInputs(`data-use_example` = 1)
    accept(shiny::isolate(rv$t_vars), 1)
    run_summary()
    expect_match(shown_label(output), "Table 3.1", fixed = TRUE)
    first_mean <- mean_of("utility_GB")
    expect_length(rv$results, 1L)

    # An upload chosen but not confirmed changes nothing.
    p <- shiny::isolate(rv$t_csv)
    session$setInputs(`data-file` = list(name = basename(p), datapath = p))
    expect_match(shown_label(output), "Table 3.1", fixed = TRUE)
    expect_length(rv$results, 1L)
    expect_equal(mean_of("utility_GB"), first_mean)

    # Confirmed: the shown result goes with the saved ones.
    accept(shiny::isolate(rv$t_vars), 2)
    expect_false(grepl("card-header", shown_label(output), fixed = TRUE))
    expect_length(rv$results, 0L)
    # Run again: the result is the new data's.
    run_summary()
    expect_match(shown_label(output), "Table 3.1", fixed = TRUE)
    expect_false(isTRUE(all.equal(mean_of("utility_GB"), first_mean)))
    saved <- rv$results[[1]]$data
    pre <- rv$processed_data[rv$processed_data$fu %in% "Pre-op", ]
    expect_equal(saved[["Pre-op"]][saved$name == "Mean"],
                 mean(pre$utility_GB, na.rm = TRUE))

    # A new variable selection of the same data: cleared again.
    accept(shiny::isolate(rv$t_vars2), 3)
    expect_false(grepl("card-header", shown_label(output), fixed = TRUE))

    # Overwriting the value column the shown result used: cleared.
    run_summary()
    expect_match(shown_label(output), "Table 3.1", fixed = TRUE)
    session$setInputs(`values-method` = "direct", `values-country` = "DK",
                      `values-col_name` = "utility_GB")
    session$setInputs(`values-add` = 99)
    expect_false(grepl("card-header", shown_label(output), fixed = TRUE))
  }))
})

test_that("start_revision() moves the revision on", {
  skip_unless_app()
  e <- app_env()
  rv <- new_rv()
  shiny::isolate({
    before <- rv$revision %||% 0L
    e$start_revision(rv, list(source = "example"), list())
    expect_identical(rv$revision, before + 1L)
  })
})
