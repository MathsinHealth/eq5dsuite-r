# How the Shiny app formats the analysis tables it displays, and which
# analyses it offers. These are display rules: the package functions return
# proportions and the app shows them as percentages, leaving the exported CSV
# untouched.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS5 <- c("mo", "sc", "ua", "pd", "ad")

grouped_example <- function() {
  d <- example_data
  d$groupvar <- d$procedure
  d
}

five_level_long <- function(n = 600L) {
  set.seed(5)
  data.frame(id = rep(seq_len(n / 2), each = 2),
             time = rep(c("Pre", "Post"), n / 2),
             mo = sample(1:5, n, TRUE), sc = sample(1:5, n, TRUE),
             ua = sample(1:5, n, TRUE), pd = sample(1:5, n, TRUE),
             ad = sample(1:5, n, TRUE), groupvar = "All",
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Which columns hold proportions
# ---------------------------------------------------------------------------

test_that("1.1.3 shows its percentages as percentages", {
  skip_unless_app()
  e <- app_env()
  r <- q(eq5d_profile_top_states(example_data, names_eq5d = DIMS5,
                                 eq5d_version = "3L", n = 3))

  # The function returns proportions under those headings.
  expect_lt(max(r$Percentage, na.rm = TRUE), 1)
  expect_equal(eq5dsuite:::.proportion_columns(r),
               c("Percentage", "Cumulative percentage"))

  shown <- eq5d_format_table(r)
  expect_equal(shown$Percentage[1], "22.5%")
  expect_equal(shown$`Cumulative percentage`[2], "36.5%")
  # The count keeps its thousands separator and no decimals.
  expect_equal(shown$Frequency[1], "2,142")
})

test_that("1.2.2's _p columns are percentages, its _n columns counts", {
  skip_unless_app()
  e <- app_env()
  r <- q(eq5d_profile_pchc_table(grouped_example(), name_id = "id",
                                 name_groupvar = "groupvar",
                                 names_eq5d = DIMS5, name_fu = "time"))

  pct <- eq5dsuite:::.proportion_columns(r)
  expect_true(all(grepl("_p$", pct)))
  expect_length(pct, sum(grepl("_p$", names(r))))

  # eq5d_format_table() also tidies the headings, as the app and the report do.
  shown <- eq5d_format_table(r)
  expect_match(shown[["Groin Hernia Post-op %"]][1], "%$")
  expect_equal(shown[["Groin Hernia Post-op n"]][1], "255")
})

test_that("a _p column only counts when its _n partner is there", {
  skip_unless_app()
  e <- app_env()
  # A group whose name ends in _p must not be mistaken for a proportion.
  df <- data.frame(name = "Mean", group_p = 0.5, other = 1.0)
  expect_equal(eq5dsuite:::.proportion_columns(df), character(0L))
  df$group_n <- 10
  expect_equal(eq5dsuite:::.proportion_columns(df), "group_p")
})

test_that("the dimension-change and LFS tables are percentages too", {
  skip_unless_app()
  e <- app_env()

  r124 <- q(eq5d_profile_dimension_change_table(
    grouped_example(), name_id = "id", names_eq5d = DIMS5, name_fu = "time"))
  expect_true(all(grepl("% Total$|% Type$", eq5dsuite:::.proportion_columns(r124))))
  expect_length(eq5dsuite:::.proportion_columns(r124), 10L)

  r132 <- q(eq5d_profile_lfs_distribution(example_data, names_eq5d = DIMS5,
                                          eq5d_version = "3L"))
  expect_setequal(eq5dsuite:::.proportion_columns(r132), c("%", "Cum (%)"))
})

test_that("tables with no proportions are left alone", {
  skip_unless_app()
  e <- app_env()
  d <- grouped_example()
  d$value <- q(eq5d3l(d[, DIMS5], country = "GB"))

  for (r in list(
    q(eq5d_utility_summary_by_group(d, name_utility = "value",
                                    name_groupvar = "groupvar")),
    q(eq5d_vas_summary(d, name_vas = "vas", name_fu = "time",
                       levels_fu = c("Pre-op", "Post-op"))),
    q(eq5d_profile_shannon(d, names_eq5d = DIMS5, eq5d_version = "3L")),
    q(eq5d_profile_lfs_mean_utility(d, names_eq5d = DIMS5,
                                    name_utility = "value",
                                    eq5d_version = "3L")))) {
    expect_equal(eq5dsuite:::.proportion_columns(r), character(0L))
  }
})

test_that("the underlying data is untouched by the display rules", {
  skip_unless_app()
  e <- app_env()
  r <- q(eq5d_profile_top_states(example_data, names_eq5d = DIMS5,
                                 eq5d_version = "3L", n = 3))
  before <- r$Percentage
  invisible(eq5d_format_table(r))
  expect_identical(r$Percentage, before)
  # So the CSV export still writes the proportion.
  expect_lt(r$Percentage[1], 1)
})

# ---------------------------------------------------------------------------
# 1.2.1: which way round the change is read
# ---------------------------------------------------------------------------

test_that("the timepoint order is taken from the mapping, not the row order", {
  skip_unless_app()
  e <- app_env()

  # A file whose Post-op rows come first. Without a stated order, the change
  # would be read backwards, which is what this guards against.
  d <- example_data[order(example_data$id, example_data$time), ]
  expect_equal(unique(as.character(d$time))[1], "Post-op")

  m <- list(eq5d_version = "3L", names_eq5d = DIMS5, name_fu = "time",
            levels_fu = c("Pre-op", "Post-op"), name_groupvar = "procedure",
            name_id = "id", name_vas = "vas", name_age = "ageband",
            name_sex = "gender", name_utility = NULL, country = "")
  rv <- shiny::reactiveValues(raw_data = d, mapping = m,
                              processed_data = eq5d_apply_mapping(d, m),
                              results = list(), value_cols = character(0L))

  suppressWarnings(shiny::testServer(e$mod_analysis_server,
                                     args = list(rv = rv), {
    session$setInputs(component = "profile", output = "121")
    session$setInputs(run = 1)
    r <- res$data
    prob <- r[grepl("^Number reporting", r$level), ]
    chg  <- r[grepl("^Change in numbers", r$level), ]

    pre  <- prob[["n_Pre-op_mo"]]
    post <- prob[["n_Post-op_mo"]]
    # Reported against the later timepoint, as later minus earlier.
    expect_equal(chg[["n_Post-op_mo"]], post - pre)
    expect_true(is.na(chg[["n_Pre-op_mo"]]))
    expect_lt(chg[["n_Post-op_mo"]], 0)   # problems fell after surgery

    # And the order reaches the function, not just the result.
    expect_match(rv$results[[1L]]$fn_call,
                 'levels_fu = c("Pre-op", "Post-op")', fixed = TRUE)
  }))
})

test_that("the Data page offers the timepoint order and stores it", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L))

  shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    session$setInputs(col_fu = "time")
    html <- as.character(output$fu_order_ui$html)
    expect_match(html, "Timepoint order", fixed = TRUE)
    expect_match(html, "later minus earlier", fixed = TRUE)

    # Default: the order the values appear in the file.
    expect_equal(fu_levels(), c("Pre-op", "Post-op"))

    # The user's own order is kept.
    session$setInputs(fu_order = c("Post-op", "Pre-op"))
    expect_equal(fu_levels(), c("Post-op", "Pre-op"))

    session$setInputs(version = "3L", col_mo = "mo", col_sc = "sc",
                      col_ua = "ua", col_pd = "pd", col_ad = "ad",
                      col_groupvar = "procedure", col_id = "id",
                      col_vas = "vas", col_age = "ageband", col_sex = "gender",
                      col_utility = "", confirm = 1)
    expect_equal(rv$mapping$levels_fu, c("Post-op", "Pre-op"))
  })
})

# ---------------------------------------------------------------------------
# 1.2.3 and 1.2.4 on EQ-5D-5L data
# ---------------------------------------------------------------------------

test_that("1.2.3 and 1.2.4 run on EQ-5D-5L data", {
  skip_unless_app()
  e <- app_env()
  d <- five_level_long()
  m <- list(eq5d_version = "5L", names_eq5d = DIMS5, name_fu = "time",
            levels_fu = c("Pre", "Post"), name_groupvar = "groupvar",
            name_id = "id", name_vas = NULL, name_age = NULL, name_sex = NULL,
            name_utility = NULL, country = "")

  for (out in c("123", "124")) {
    rv <- shiny::reactiveValues(raw_data = d, mapping = m,
                                processed_data = eq5d_apply_mapping(d, m),
                                results = list(), value_cols = character(0L))
    suppressWarnings(shiny::testServer(e$mod_analysis_server,
                                       args = list(rv = rv), {
      session$setInputs(component = "profile", output = out)
      expect_equal(missing(), character(0L), info = out)   # not gated
      session$setInputs(run = 1)
      expect_s3_class(res$data, "data.frame")
      expect_gt(nrow(res$data), 0L)
    }))
  }
})

test_that("1.2.4 covers all 25 level transitions on 5L", {
  r <- q(eq5d_profile_dimension_change_table(
    five_level_long(2000L), name_id = "id", names_eq5d = DIMS5,
    name_fu = "time", levels_fu = c("Pre", "Post")))
  expect_equal(nrow(r), 25L)
  expect_setequal(r$level_change,
                  as.vector(outer(1:5, 1:5, function(a, b) paste0(a, "-", b))))
  # Each dimension's "% Total" column accounts for everyone.
  for (cn in grep("% Total$", names(r), value = TRUE))
    expect_equal(sum(r[[cn]]), 1, tolerance = 1e-9)
})

test_that("no analysis is restricted to one instrument any more", {
  skip_unless_app()
  e <- app_env()
  needs <- unlist(lapply(e$ANALYSES, `[[`, "needs"))
  expect_false("3L" %in% needs)
})
