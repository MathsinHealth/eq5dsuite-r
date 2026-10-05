# .prep_fu() factorises the follow-up column against the levels the caller
# asked for. factor() turns anything not among them into NA, and the analysis
# functions then drop those rows, so a misspelled or forgotten level used to
# remove respondents in silence:
#
#   .prep_fu(data.frame(t = c("Pre-op", "Post-op", "MidOp")), name = "t",
#            levels = c("Pre-op", "Post-op"))
#   #  Pre-op Post-op    <NA>
#   #       1       1       1
#
# It now says how many rows will go and which values are responsible.

prep_fu_warnings <- function(v, levels) {
  w <- character(0)
  withCallingHandlers(
    .prep_fu(data.frame(t = v), name = "t", levels = levels),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  w
}

# ---------------------------------------------------------------------------
# The warning
# ---------------------------------------------------------------------------

test_that("unlisted follow-up values are reported", {
  w <- prep_fu_warnings(c("Pre-op", "Post-op", "MidOp"),
                        c("Pre-op", "Post-op"))

  expect_length(w, 1L)
  expect_match(w, "^1 row\\(s\\) will be excluded")
  expect_match(w, "levels_fu", fixed = TRUE)
  expect_match(w, "\"MidOp\"", fixed = TRUE)
})

test_that("the count is of rows, and the list is of distinct values", {
  w <- prep_fu_warnings(c("Pre-op", rep("MidOp", 3), rep("Late", 2)),
                        "Pre-op")

  expect_match(w, "^5 row\\(s\\)")
  expect_match(w, "\"MidOp\", \"Late\"", fixed = TRUE)
})

test_that("the list of values is capped", {
  w <- prep_fu_warnings(c("Pre-op", paste0("T", 1:13)), "Pre-op")

  expect_match(w, "^13 row\\(s\\)")
  expect_match(w, "\"T1\", \"T2\"", fixed = TRUE)
  expect_match(w, "and 3 more", fixed = TRUE)
  # Ten values listed, not thirteen.
  expect_identical(lengths(regmatches(w, gregexpr("\"", w))) / 2, 10)
})

test_that("the warning does not name the internal helper", {
  cnd <- tryCatch(
    .prep_fu(data.frame(t = c("Pre-op", "MidOp")), name = "t", levels = "Pre-op"),
    warning = function(w) w)
  expect_null(conditionCall(cnd))
})

# ---------------------------------------------------------------------------
# When it must stay quiet
# ---------------------------------------------------------------------------

test_that("values that are all listed raise nothing", {
  expect_no_warning(
    .prep_fu(data.frame(t = c("Pre-op", "Post-op")), name = "t",
             levels = c("Pre-op", "Post-op")))
})

test_that("values that were already NA are not counted", {
  # A missing follow-up is not a level the caller forgot.
  expect_no_warning(
    .prep_fu(data.frame(t = c("Pre-op", "Post-op", NA, NA)), name = "t",
             levels = c("Pre-op", "Post-op")))

  # ... and where there are both, only the unlisted values are counted.
  w <- prep_fu_warnings(c("Pre-op", NA, NA, "MidOp"), c("Pre-op", "Post-op"))
  expect_match(w, "^1 row\\(s\\)")
})

test_that("a level with no rows raises nothing", {
  expect_no_warning(
    .prep_fu(data.frame(t = c("Pre-op", "Post-op")), name = "t",
             levels = c("Pre-op", "Interim", "Post-op")))
})

test_that("an empty column raises nothing", {
  expect_no_warning(
    .prep_fu(data.frame(t = character(0)), name = "t", levels = "Pre-op"))
})

# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

test_that("factor and numeric follow-up columns are handled", {
  expect_match(prep_fu_warnings(factor(c("a", "b", "c")), c("a", "b")),
               "\"c\"", fixed = TRUE)
  expect_match(prep_fu_warnings(c(1, 2, 3), c(1, 2)), "\"3\"", fixed = TRUE)
})

test_that("the column is still renamed and factorised as before", {
  r <- suppressWarnings(
    .prep_fu(data.frame(t = c("Post-op", "Pre-op", "MidOp")), name = "t",
             levels = c("Pre-op", "Post-op")))

  expect_identical(names(r), "fu")
  expect_s3_class(r$fu, "factor")
  expect_identical(levels(r$fu), c("Pre-op", "Post-op"))
  expect_identical(as.character(r$fu), c("Post-op", "Pre-op", NA))
})

# ---------------------------------------------------------------------------
# Through an exported function
# ---------------------------------------------------------------------------

test_that("a mistyped level is reported by the analysis functions", {
  df <- example_data
  w <- character(0)
  withCallingHandlers(
    suppressMessages(eq5d_vas_summary(df, name_vas = "vas", name_fu = "time",
                                      levels_fu = c("Pre-op", "Postop"))),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })

  hit <- grep("will be excluded", w, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, paste0("^", sum(example_data$time == "Post-op"), " row\\(s\\)"))
  expect_match(hit, "\"Post-op\"", fixed = TRUE)
})

test_that("the package's own level names raise nothing", {
  w <- character(0)
  withCallingHandlers(
    suppressMessages(eq5d_vas_summary(example_data, name_vas = "vas",
                                      name_fu = "time",
                                      levels_fu = c("Pre-op", "Post-op"))),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_length(grep("will be excluded", w), 0L)
})
