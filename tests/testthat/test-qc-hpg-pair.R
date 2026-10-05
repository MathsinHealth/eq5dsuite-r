# Verification of 2026-10-04, V01: the rest of review Q09.
#
# With three timepoints selected on the Data page -- pre, post, mid -- and
# pairs only at pre/post, the Health Profile Grid was rightly offered: some
# pair of timepoints has data. But with pre/mid chosen as the two to compare,
# the Run button was still shown, and running it failed ("No respondent has a
# valid EQ-5D profile at both ...") with nothing saved.
#
# The catalogue still asks whether some choice of options can run; the Run
# button now asks whether the chosen options can. Everything below happens
# in one mounted app, across several confirmed revisions of the data.

DIMS <- c("mo", "sc", "ua", "pd", "ad")

whole_app <- function(e) function(id, rv) shiny::moduleServer(id, function(input, output, session) {
  e$mod_data_server("data", rv)
  e$mod_validation_server("validation", rv)
  e$mod_values_server("values", rv)
  e$mod_analysis_server("analysis", rv)
  e$mod_export_server("export", rv)
})

new_rv <- function() shiny::reactiveValues(
  raw_data = NULL, mapping = NULL, processed_data = NULL, results = list(),
  load_example = 0L, value_cols = character(0L), steps = list())

csv_of <- function(d) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE, na = "")
  f
}

# Revision 1, the report's data: pairs at pre/post, patient 3 only at mid.
# Patient 3 is in group B, the only one without EQ VAS scores.
d1 <- data.frame(id = c(1, 1, 2, 2, 3), time = c("pre", "post", "pre", "post", "mid"),
                 mo = c(3, 2, 2, 1, 1), sc = 1, ua = 1, pd = 1, ad = 1,
                 grp = c("A", "A", "A", "A", "B"), vas = c(50, 60, 70, 80, NA))
# Revision 2: two timepoints, shuffled, with partial profiles. Patient 1 is
# incomplete at mid, patient 2 at pre; only patient 3 has a valid pair.
d2 <- data.frame(id = c(3, 1, 2, 3, 2, 1), time = c("mid", "mid", "pre", "pre", "mid", "pre"),
                 mo = c(1, NA, 2, 2, 1, 3), sc = c(1, 1, NA, 1, 1, 1), ua = 1, pd = 1, ad = 1,
                 grp = "A", vas = 50)
# Revision 3: as revision 2 without patient 3's mid: no valid pair at all.
d3 <- d2[!(d2$id == 3 & d2$time == "mid"), ]

test_that("one mounted app: the HPG Run button follows the pair chosen, across revisions", {
  skip_unless_app()
  e <- app_env()
  rv <- new_rv()
  rv[["t_files"]] <- lapply(list(d1, d2, d3), csv_of)
  rv[["t_gB"]] <- e$group_filter_value("B")
  rv[["t_gA"]] <- e$group_filter_value("A")
  rv[["t_avail"]] <- function() vapply(shiny::isolate(e$available_analyses(rv)),
                                       `[[`, "", "id")

  suppressWarnings(suppressMessages(shiny::testServer(whole_app(e), args = list(rv = rv), {
    run_ui <- function() paste(as.character(output$`analysis-run_ui`$html), collapse = "")
    has_run <- function() grepl(">Run<", run_ui(), fixed = TRUE) ||
      grepl("analysis-run\"", run_ui(), fixed = TRUE)
    script <- function() paste(readLines(output$`export-download_script`), collapse = "\n")
    click <- function() session$setInputs(`analysis-run` = (input$`analysis-run` %||% 0) + 1)
    upload <- function(k, order, n) {
      p <- shiny::isolate(rv$t_files)[[k]]
      session$setInputs(`data-file` = list(name = basename(p), datapath = p))
      session$setInputs(`data-version` = "3L", `data-col_mo` = "mo", `data-col_sc` = "sc",
                        `data-col_ua` = "ua", `data-col_pd` = "pd", `data-col_ad` = "ad",
                        `data-col_fu` = "time", `data-col_groupvar` = "grp",
                        `data-col_id` = "id", `data-col_vas` = "vas", `data-col_age` = "",
                        `data-col_sex` = "", `data-col_utility` = "",
                        `data-fu_order` = order)
      session$setInputs(`data-confirm` = n)
      session$setInputs(`validation-proceed` = n)
    }
    hpg <- function(pair) {
      session$setInputs(`analysis-component` = "profile")
      session$setInputs(`analysis-output` = "125fig", `analysis-country` = "GB")
      session$setInputs(`analysis-fu_levels` = pair)
    }
    patients <- function(r) length(unique(r$plot$data$id))

    # ── Revision 1: three timepoints; pairs only at pre/post ──
    upload(1, c("pre", "post", "mid"), 1)
    expect_identical(rv$mapping$levels_fu, c("pre", "post", "mid"))
    # The catalogue offers it: some pair can run.
    expect_true("125fig" %in% rv$t_avail())

    hpg(c("pre", "mid"))
    expect_false(has_run())
    expect_match(run_ui(), "valid profiles at both", fixed = TRUE)
    expect_match(run_ui(), "pre", fixed = TRUE)
    expect_match(run_ui(), "mid", fixed = TRUE)
    click()                                   # a stale click does nothing
    expect_length(rv$results, 0L)
    expect_false(grepl("health_profile_grid", script(), fixed = TRUE))

    # The same mounted module, another pair: it runs, on patients 1 and 2.
    hpg(c("pre", "post"))
    expect_true(has_run())
    click()
    expect_length(rv$results, 1L)
    r <- rv$results[[1]]
    expect_identical(r$call$args$levels_fu, c("pre", "post"))
    expect_identical(patients(r), 2L)
    expect_setequal(r$plot$data$id, c(1, 2))
    expect_match(script(), "levels_fu = c(\"pre\", \"post\")", fixed = TRUE)

    # And back: post/mid has nobody at both either.
    hpg(c("post", "mid"))
    expect_false(has_run())
    click()
    expect_length(rv$results, 1L)

    # The grouping variable restricts nothing here: HPG has no group option.
    hpg(c("pre", "post"))
    expect_true(has_run())

    # A group filter does restrict, where an analysis has one: group B has
    # no EQ VAS scores.
    session$setInputs(`analysis-component` = "vas")
    session$setInputs(`analysis-output` = "21")
    session$setInputs(`analysis-group_filter` = rv$t_gB)
    expect_false(has_run())
    expect_match(run_ui(), "EQ VAS scores", fixed = TRUE)
    click()
    expect_length(rv$results, 1L)
    session$setInputs(`analysis-group_filter` = rv$t_gA)
    expect_true(has_run())

    # ── Revision 2: two timepoints, shuffled, partial profiles ──
    upload(2, c("pre", "mid"), 2)
    expect_length(rv$results, 0L)              # cleared by the revision
    hpg(c("pre", "mid"))
    expect_true(has_run())
    click()
    expect_length(rv$results, 1L)
    expect_identical(patients(rv$results[[1]]), 1L)
    expect_setequal(rv$results[[1]]$plot$data$id, 3)
    # The pair in the other order is the same pair of visits.
    hpg(c("mid", "pre"))
    expect_true(has_run())

    # ── Revision 3: the only complete pair removed ──
    upload(3, c("pre", "mid"), 3)
    expect_false("125fig" %in% rv$t_avail())
    hpg(c("pre", "mid"))
    expect_false(has_run())
    click()
    expect_length(rv$results, 0L)
    expect_false(grepl("health_profile_grid", script(), fixed = TRUE))
  })))
})

test_that("the selected-options check, unit by unit", {
  skip_unless_app()
  e <- app_env()
  m <- list(names_eq5d = DIMS, eq5d_version = "3L", name_id = "id",
            name_fu = "time", name_groupvar = "grp", name_vas = "vas",
            levels_fu = c("pre", "post", "mid"))
  rv <- shiny::reactiveValues(raw_data = d1, mapping = m,
                              processed_data = eq5d_apply_mapping(d1, m),
                              results = list(), value_cols = character(0),
                              steps = list())
  hpg <- e$analysis_spec("125fig")
  unmet <- function(spec, input) shiny::isolate(e$analysis_option_unmet(spec, input, rv))
  expect_match(unmet(hpg, list(fu_levels = c("pre", "mid"))), "both \"pre\" and \"mid\"",
               fixed = TRUE)
  expect_length(unmet(hpg, list(fu_levels = c("pre", "post"))), 0L)
  expect_length(unmet(hpg, list(fu_levels = c("post", "pre"))), 0L)
  # Not exactly two chosen: Run says so itself.
  expect_length(unmet(hpg, list(fu_levels = "pre")), 0L)
  vs <- e$analysis_spec("21")
  expect_length(unmet(vs, list(group_filter = e$GROUP_FILTER_ALL)), 0L)
  expect_length(unmet(vs, list(group_filter = e$group_filter_value("A"))), 0L)
  expect_match(unmet(vs, list(group_filter = e$group_filter_value("B"))), "EQ VAS")
})
