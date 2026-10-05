# House-keeping the review turned up: inconsistent labels, inconsistent error
# style, one plot bypassing the shared theme, dead code and misspellings that
# had been added to the spell-check wordlist rather than corrected.

dims <- c("mo", "sc", "ua", "pd", "ad")

pkg_r_files <- function() {
  # The installed package keeps no R sources, so these tests run against the
  # source tree when there is one and skip otherwise.
  dir <- testthat::test_path("..", "..", "R")
  if (!dir.exists(dir)) character(0) else
    list.files(dir, pattern = "[.]R$", full.names = TRUE)
}

# ---------------------------------------------------------------------------
# L-4. Condition style
# ---------------------------------------------------------------------------

# The analyses of EQ-5D values take a value column rather than a value set.
valued <- function(df) {
  df$value <- suppressWarnings(suppressMessages(
    eq5d3l(df[, c("mo", "sc", "ua", "pd", "ad")], country = "GB")))
  df
}

test_that("analysis errors do not print the internal call", {
  bad <- c("not_a_column", "sc", "ua", "pd", "ad")

  for (f in list(
    function() eq5d_profile_top_states(example_data, names_eq5d = bad,
                                       eq5d_version = "3L", n = 3),
    function() eq5d_profile_lfs_distribution(example_data, names_eq5d = bad,
                                             eq5d_version = "3L"),
    function() eq5d_utility_summary(example_data,
                                    name_utility = "not_a_column"))) {
    cnd <- tryCatch(suppressMessages(f()), error = function(e) e)
    expect_s3_class(cnd, "error")
    # call. = FALSE, so the user is not shown .get_names() or .prep_eq5d().
    expect_null(conditionCall(cnd))
    # "Stopping." added nothing: R already prints "Error".
    expect_false(grepl("Stopping", conditionMessage(cnd), fixed = TRUE))
    expect_match(conditionMessage(cnd), "data frame", fixed = TRUE)
  }
})

test_that("no error message in the package still says \"Stopping.\"", {
  files <- pkg_r_files()
  skip_if(length(files) == 0, "package sources not available")

  hits <- unlist(lapply(files, function(f) grep("Stopping\\.", readLines(f, warn = FALSE),
                                                value = TRUE)))
  expect_identical(hits, character(0))
})

test_that("every stop() in the analysis code passes call. = FALSE", {
  files <- pkg_r_files()
  skip_if(length(files) == 0, "package sources not available")
  f <- grep("eq5d_devlin[.]R$", files, value = TRUE)
  skip_if(length(f) != 1L)

  # Most of these have since become .check_columns() calls (M-8), so there are
  # only a few left; whatever remains must not print the internal call.
  # Parsed, not matched with a regex: these calls span several lines and
  # contain nested parentheses.
  calls <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (is.name(e[[1]]) && identical(as.character(e[[1]]), "stop"))
      calls[[length(calls) + 1L]] <<- e
    for (x in as.list(e)) if (!missing(x)) try(walk(x), silent = TRUE)
  }
  for (e in parse(f)) walk(e)

  expect_gt(length(calls), 0L)
  for (cl in calls) {
    nms <- names(as.list(cl)[-1])
    expect_true(!is.null(nms) && "call." %in% nms,
                info = paste(deparse(cl)[1], "..."))
    expect_false(eval(as.list(cl)[["call."]]), info = paste(deparse(cl)[1]))
  }
})

# ---------------------------------------------------------------------------
# L-8. The shared theme
# ---------------------------------------------------------------------------

test_that("the HSDC plot uses the same theme as every other plot", {
  hsdc <- suppressWarnings(suppressMessages(
    eq5d_profile_density_curve(example_data, names_eq5d = dims,
                               eq5d_version = "3L")))$p
  other <- suppressWarnings(suppressMessages(
    eq5d_profile_lss_utility_plot(valued(example_data), names_eq5d = dims,
                                  name_utility = "value",
                                  eq5d_version = "3L")))$p

  # .modify_ggplot_theme() is what sets these; theme_minimal() does not.
  expect_identical(hsdc$theme$plot.title$hjust, 0.5)
  expect_identical(hsdc$theme$plot.title$hjust, other$theme$plot.title$hjust)
  expect_s3_class(hsdc$theme$panel.grid.major.x, "element_blank")
  expect_s3_class(hsdc$theme$axis.ticks.x, "element_blank")
  expect_identical(hsdc$theme$legend.position, other$theme$legend.position)
})

# ---------------------------------------------------------------------------
# L-1. One spelling per label
# ---------------------------------------------------------------------------

test_that("the Shiny app uses the same dimension labels as the package", {
  expect_identical(
    unname(DIM_LABELS),
    c("Mobility", "Self-care", "Usual activities", "Pain/discomfort",
      "Anxiety/depression"))
})

test_that("the utility column is labelled \"EQ-5D value\"", {
  r <- suppressWarnings(suppressMessages(
    eq5d_profile_lfs_mean_utility(valued(example_data), names_eq5d = dims,
                                  name_utility = "value",
                                  eq5d_version = "3L")))
  expect_true("EQ-5D value" %in% names(r))
  expect_false("EQ-5D Value" %in% names(r))
})

test_that("the package settles on one spelling of each label", {
  files <- pkg_r_files()
  skip_if(length(files) == 0, "package sources not available")
  root <- dirname(files[1])
  files <- c(files,
             list.files(file.path(dirname(root), "inst"), pattern = "[.]R$",
                        full.names = TRUE, recursive = TRUE))
  txt <- unlist(lapply(files, readLines, warn = FALSE))

  count <- function(p) sum(vapply(gregexpr(p, txt, fixed = TRUE),
                                  function(m) sum(m > 0), integer(1)))

  # The rejected variant of each pair.
  expect_identical(count("Self care"), 0L)
  expect_identical(count("Pain/Discomfort"), 0L)
  expect_identical(count("Anxiety/Depression"), 0L)
  expect_identical(count("EQ-5D Value"), 0L)
  # EuroQol's own term is "EQ VAS".
  expect_identical(count("EQ-VAS"), 0L)
  expect_gt(count("EQ VAS"), 0L)
})

# ---------------------------------------------------------------------------
# L-3. Dead code
# ---------------------------------------------------------------------------

test_that("the commented-out blocks are gone", {
  files <- pkg_r_files()
  skip_if(length(files) == 0, "package sources not available")
  txt <- unlist(lapply(files, readLines, warn = FALSE))

  for (p in c("assign(x = \"EQrxwmod7\"", "rename(utility = !!quo_name",
              "# .fixCountries <- function"))
    expect_false(any(grepl(p, txt, fixed = TRUE)), info = p)
})

test_that(".add_utility() still adds the utility column", {
  # The dead block sat immediately after the line that does the work, so this
  # guards against removing one line too many.
  d <- data.frame(mo = c(1L, 3L), sc = c(1L, 3L), ua = c(1L, 3L),
                  pd = c(1L, 3L), ad = c(1L, 3L))
  r <- .prep_eq5d(d, names = dims, add_state = TRUE, add_utility = TRUE,
                  eq5d_version = "3L", country = "GB")

  expect_true("utility" %in% names(r))
  expect_equal(unname(r$utility), unname(eq5d3l(c(11111, 33333), "GB")))
})

test_that("an unknown country is reported by name", {
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  cnd <- tryCatch(
    suppressMessages(utils::capture.output(
      .prep_eq5d(d, names = dims, add_state = TRUE, add_utility = TRUE,
                 eq5d_version = "3L", country = "NOWHERE"))),
    error = function(e) e)

  expect_s3_class(cnd, "error")
  expect_match(conditionMessage(cnd), "NOWHERE", fixed = TRUE)
  expect_null(conditionCall(cnd))
})

# ---------------------------------------------------------------------------
# L-5. Spelling
# ---------------------------------------------------------------------------

test_that("the spell-check wordlist holds no misspellings", {
  wl <- system.file("WORDLIST", package = "eq5dsuite")
  if (!nzchar(wl)) wl <- testthat::test_path("..", "..", "inst", "WORDLIST")
  skip_if(!file.exists(wl), "WORDLIST not available")

  words <- trimws(readLines(wl, warn = FALSE))
  # These were listed rather than corrected, which is how they survived.
  expect_false(any(c("standarized", "measurememnt", "subsequentyl",
                     "crosswealk", "prodcuces") %in% words))
  # ... as was a helper that has since been removed.
  expect_false("pchctab" %in% words)
})

test_that("a spell check actually runs", {
  f <- testthat::test_path("..", "spelling.R")
  skip_if(!file.exists(f), "source tree not available")
  expect_match(paste(readLines(f, warn = FALSE), collapse = "\n"),
               "spell_check_test", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# M-3. The HSDI is returned, not only drawn
#
# The Health State Density Index was computed, rounded to three decimals,
# written into the plot subtitle and then dropped. A user wanting it for a
# table had to scrape the subtitle, and got the rounded figure at that.
# ---------------------------------------------------------------------------

hsdc <- function(df = example_data, version = "3L")
  suppressWarnings(suppressMessages(
    eq5d_profile_density_curve(df, names_eq5d = dims, eq5d_version = version)))

test_that("eq5d_profile_density_curve() returns the HSDI", {
  r <- hsdc()
  expect_true("hsdi" %in% names(r))
  expect_type(r$hsdi, "double")
  expect_length(r$hsdi, 1L)
  expect_false(is.na(r$hsdi))
  # Twice the area between the curve and the diagonal: 0 to 1.
  expect_gte(r$hsdi, 0)
  expect_lte(r$hsdi, 1)
})

test_that("the returned HSDI is unrounded, and the subtitle is rounded", {
  r <- hsdc()
  subtitle <- ggplot2::ggplot_build(r$p)$plot$labels$subtitle

  expect_identical(subtitle, paste0("HSDI = ", round(r$hsdi, 3)))
  # The value itself carries more precision than the subtitle shows.
  expect_false(identical(r$hsdi, round(r$hsdi, 3)))
})

test_that("the HSDI matches the plot data it is derived from", {
  r <- hsdc()
  d <- r$plot_data
  area <- sum(diff(c(0, d$CumPropObservations)) *
                (utils::head(c(0, d$CumPropStates), -1) + d$CumPropStates) / 2)
  expect_equal(r$hsdi, 2 * area)
})

test_that("the existing elements are unchanged and keep their names", {
  r <- hsdc()
  # Additive: anything reading result$plot_data or result$p is unaffected.
  expect_identical(names(r), c("plot_data", "hsdi", "p"))
  expect_s3_class(r$plot_data, "data.frame")
  expect_identical(names(r$plot_data),
                   c("state", "Frequency", "CumPropObservations",
                     "CumPropStates"))
  expect_s3_class(r$p, "ggplot")
})

test_that("the index is relative to the profiles observed, as documented", {
  r <- hsdc()
  n <- nrow(r$plot_data)
  # CumPropStates runs 1/n .. 1 over the observed profiles, not over the 243
  # the EQ-5D-3L allows. example_data shows far fewer than 243.
  expect_lt(n, 243L)
  expect_equal(r$plot_data$CumPropStates, seq_len(n) / n)
})

# ---------------------------------------------------------------------------
# M-15. The kurtosis convention is stated in the output
#
# moments::kurtosis() is the population estimator m4 / m2^2, which is
# non-excess: 3 for a normal distribution. Stata's `summarize, detail` reports
# the same quantity; Excel's KURT() reports excess kurtosis, so it is about 3
# lower. The convention is unchanged; the row now says which one it is.
# ---------------------------------------------------------------------------

test_that("the kurtosis row names its convention", {
  r <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(example_data, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Post-op"))))

  expect_true("Kurtosis (non-excess)" %in% r$name)
  expect_false("Kurtosis" %in% r$name)
  expect_identical(sum(grepl("^Kurtosis", r$name)), 1L)
})

test_that("the convention itself is unchanged", {
  v <- example_data$vas[example_data$time == "Pre-op"]
  v <- v[v %in% 0:100]
  m <- mean(v)
  population_non_excess <- mean((v - m)^4) / mean((v - m)^2)^2

  r <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(example_data, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Post-op"))))
  got <- r[["Pre-op"]][r$name == "Kurtosis (non-excess)"]

  expect_equal(got, population_non_excess)
  # Non-excess, so a normal distribution would give 3, not 0.
  expect_equal(got, moments::kurtosis(v))
  expect_gt(got, 3)
})

test_that("skewness is the matching population estimator", {
  v <- example_data$vas[example_data$time == "Pre-op"]
  v <- v[v %in% 0:100]
  m <- mean(v)

  r <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(example_data, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Post-op"))))
  expect_equal(r[["Pre-op"]][r$name == "Skewness"],
               mean((v - m)^3) / mean((v - m)^2)^1.5)
})

test_that("eq5d_utility_summary() uses the same label", {
  d <- example_data
  d$value <- suppressWarnings(suppressMessages(
    eq5d3l(d[, dims], country = "GB")))
  r <- suppressWarnings(suppressMessages(
    eq5d_utility_summary(d, name_utility = "value", name_fu = "time",
                         levels_fu = c("Pre-op", "Post-op"))))
  expect_true("Kurtosis (non-excess)" %in% r$name)
})
