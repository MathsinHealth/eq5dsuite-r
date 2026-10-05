# When `eq5d_version` is not supplied, the analysis functions assume the
# EQ-5D-5L. That default is consequential: EQ-5D-3L levels are a subset of the
# 5L levels, so 3L responses valued on a 5L value set look perfectly in range
# and come back with a different number -- state 33333 is -0.594 as 3L and
# 0.604 as 5L. The bundled example_data is 3L, so the default disagrees with
# the package's own example dataset.
#
# The default is unchanged, but it is now a warning rather than a message, and
# it says how to specify the instrument. Where the data itself suggests the
# answer -- every observed level between 1 and 3 -- it says so.

dims <- c("mo", "sc", "ua", "pd", "ad")

version_warnings <- function(expr) {
  w <- character(0)
  withCallingHandlers(suppressMessages(expr),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  grep("No EQ-5D version", w, value = TRUE)
}

# ---------------------------------------------------------------------------
# The warning itself
# ---------------------------------------------------------------------------

test_that("omitting eq5d_version warns rather than messages", {
  w <- version_warnings(
    eq5d_profile_lfs_distribution(example_data, names_eq5d = dims))

  expect_length(w, 1L)
  expect_match(w, "the EQ-5D-5L is assumed", fixed = TRUE)
  expect_match(w, "eq5d_version = \"3L\"", fixed = TRUE)

  # It is a warning, and it does not name the internal helper as its source.
  # (The unrelated coercion warning for the 9 missing-data code is muffled.)
  expect_warning(
    withCallingHandlers(
      suppressMessages(eq5d_profile_lfs_distribution(example_data,
                                                     names_eq5d = dims)),
      warning = function(x)
        if (!grepl("No EQ-5D version", conditionMessage(x)))
          invokeRestart("muffleWarning")),
    "No EQ-5D version was provided")
  cnd <- tryCatch(
    suppressMessages(eq5d_profile_lfs_distribution(example_data, names_eq5d = dims)),
    warning = function(w) w)
  expect_null(conditionCall(cnd))
})

test_that("the default is still 5L", {
  # Unchanged behaviour: the warning is louder, the result is the same.
  a <- suppressWarnings(suppressMessages(
    eq5d_profile_lfs_distribution(example_data, names_eq5d = dims)))
  b <- suppressWarnings(suppressMessages(
    eq5d_profile_lfs_distribution(example_data, names_eq5d = dims,
                                  eq5d_version = "5L")))
  expect_identical(a, b)
})

test_that("supplying the version says nothing", {
  for (v in c("3L", "5L", "Y3L"))
    expect_length(
      version_warnings(eq5d_profile_lfs_distribution(
        example_data, names_eq5d = dims, eq5d_version = v)),
      0L)
})

# ---------------------------------------------------------------------------
# The hint drawn from the data
# ---------------------------------------------------------------------------

test_that("data with no level above 3 is flagged as possibly EQ-5D-3L", {
  w <- version_warnings(
    eq5d_profile_lfs_distribution(example_data, names_eq5d = dims))
  expect_match(w, "may be EQ-5D-3L data", fixed = TRUE)
})

test_that("a missing-data code does not suppress the hint", {
  # example_data codes missing as 9. That is not a level, and .prep_eq5d()
  # coerces it to NA whichever instrument this is, so it says nothing about
  # which one it is.
  expect_true(9 %in% example_data$mo)
  expect_match(version_warnings(
    eq5d_profile_lfs_distribution(example_data, names_eq5d = dims)),
    "may be EQ-5D-3L data", fixed = TRUE)
})

test_that("data with a level above 3 is not flagged", {
  d5 <- example_data
  d5$mo[1] <- 5L
  w <- version_warnings(eq5d_profile_lfs_distribution(d5, names_eq5d = dims))

  expect_length(w, 1L)
  expect_false(grepl("may be EQ-5D-3L", w, fixed = TRUE))
})

test_that("the hint reaches every analysis function that takes a version", {
  # Each one now passes `df` to .get_names(), so the hint is available
  # wherever the warning is.
  valued_data <- example_data
  valued_data$value <- suppressWarnings(suppressMessages(
    eq5d3l(valued_data[, dims], country = "GB")))
  checks <- list(
    function() eq5d_profile_top_states(example_data, names_eq5d = dims, n = 3),
    function() eq5d_profile_lfs_distribution(example_data, names_eq5d = dims),
    # density_curve is the one function with no default for eq5d_version, so
    # NULL has to be passed explicitly to reach the same path.
    function() eq5d_profile_density_curve(example_data, names_eq5d = dims,
                                          eq5d_version = NULL),
    function() eq5d_profile_lss_utility_summary(valued_data, names_eq5d = dims,
                                                name_utility = "value"),
    function() eq5d_profile_lfs_utility_summary(valued_data, names_eq5d = dims,
                                                name_utility = "value"))
  for (f in checks) {
    w <- version_warnings(f())
    expect_length(w, 1L)
    expect_match(w, "may be EQ-5D-3L data", fixed = TRUE)
  }
})

# ---------------------------------------------------------------------------
# The package's own code never triggers it
# ---------------------------------------------------------------------------

test_that("the vignettes and examples always name the instrument", {
  # Every call in the package's own examples and vignettes to a function that
  # takes eq5d_version supplies it, so none of them warns.
  ns <- asNamespace("eq5dsuite")
  takes_version <- Filter(function(f) {
    o <- get(f, envir = ns)
    is.function(o) && "eq5d_version" %in% names(formals(o))
  }, ls(ns))

  missing_arg <- character(0)
  collect <- function(exprs, where) {
    walk <- function(e) {
      if (!is.call(e)) return(invisible())
      fn <- e[[1]]
      if (is.name(fn) && as.character(fn) %in% takes_version) {
        nms <- names(as.list(e)[-1])
        if (is.null(nms) || !"eq5d_version" %in% nms)
          missing_arg <<- c(missing_arg, paste0(as.character(fn), " in ", where))
      }
      for (x in as.list(e)) if (!missing(x)) try(walk(x), silent = TRUE)
    }
    for (e in exprs) walk(e)
  }

  # The installed vignette code, and the installed help examples. Both are
  # present under R CMD check, which is where this test does its work; under
  # devtools::test() against a source tree there is nothing to read.
  vig <- list.files(system.file("doc", package = "eq5dsuite"),
                    pattern = "[.]R$", full.names = TRUE)
  rd <- tryCatch(tools::Rd_db("eq5dsuite"), error = function(e) list())
  skip_if(length(vig) == 0 && length(rd) == 0,
          "neither vignettes nor help pages are installed")

  for (f in vig) {
    e <- try(parse(f), silent = TRUE)
    if (!inherits(e, "try-error")) collect(e, basename(f))
  }
  for (nm in names(rd)) {
    ex <- tryCatch(paste(capture.output(tools::Rd2ex(rd[[nm]])), collapse = "\n"),
                   error = function(e) "")
    e <- try(parse(text = ex), silent = TRUE)
    if (!inherits(e, "try-error")) collect(e, nm)
  }
  expect_identical(missing_arg, character(0))
})
