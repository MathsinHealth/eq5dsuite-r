# Shannon's informativity indices, chapter 4 of Devlin et al. (2020). Nothing
# in the package computed them, although DESCRIPTION cites that book as the
# specification the analysis tools follow.
#
#   H'     = -sum(p_i * log2(p_i)) over the categories that occur
#   H'max  = log2(L), where L is the number of categories the *instrument*
#            allows: 3 or 5 per dimension, 243 or 3125 for the health state
#   J'     = H' / H'max, from 0 (everything in one category) to 1 (spread
#            evenly over all L)
#
# The expected values here are computed from first principles in the test, not
# read back from the function.

dims <- c("mo", "sc", "ua", "pd", "ad")
q <- function(expr) suppressWarnings(suppressMessages(expr))

# H' computed the long way round, for checking against.
shannon_by_hand <- function(x, L) {
  x <- x[!is.na(x)]
  p <- as.numeric(table(x)) / length(x)
  h <- -sum(p * log2(p))
  c(H = h, Hmax = log2(L), J = h / log2(L))
}

# ---------------------------------------------------------------------------
# The helper
# ---------------------------------------------------------------------------

test_that(".shannon() matches the definition on cases with a known answer", {
  # Everything in one category carries no information.
  expect_equal(unname(.shannon(rep(1L, 10), 3L)), c(0, log2(3), 0))

  # Spread evenly over all L categories is the maximum: H' = H'max, J' = 1.
  expect_equal(unname(.shannon(rep(1:3, 4), 3L)), c(log2(3), log2(3), 1))
  expect_equal(unname(.shannon(rep(1:5, 4), 5L)), c(log2(5), log2(5), 1))

  # Evenly over two of three: H' = 1 bit, but J' is measured against log2(3).
  expect_equal(unname(.shannon(rep(1:2, 6), 3L)), c(1, log2(3), 1 / log2(3)))
})

test_that(".shannon() drops NAs and reports nothing for an empty vector", {
  expect_equal(.shannon(c(1L, 1L, 2L, 2L, NA), 3L),
               .shannon(c(1L, 1L, 2L, 2L), 3L))
  expect_identical(.shannon(integer(0), 3L),
                   c(H = NA_real_, Hmax = NA_real_, J = NA_real_))
  expect_identical(.shannon(c(NA, NA), 3L),
                   c(H = NA_real_, Hmax = NA_real_, J = NA_real_))
})

test_that("a category with no observations contributes nothing", {
  # 0 * log2(0) is 0 by convention, so an unused level must not produce NaN.
  r <- .shannon(c(1L, 1L, 2L), 5L)
  expect_false(anyNA(r))
  expect_equal(unname(r["H"]), -sum(c(2/3, 1/3) * log2(c(2/3, 1/3))))
})

# ---------------------------------------------------------------------------
# The exported function
# ---------------------------------------------------------------------------

test_that("eq5d_profile_shannon() reports one row per dimension and the state", {
  r <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                              eq5d_version = "3L"))

  expect_s3_class(r, "data.frame")
  expect_identical(r$dimension, c(dims, "Health state"))
  expect_identical(names(r), c("dimension", "H_All", "Hmax_All", "J_All"))
  expect_false(anyNA(r[, -1]))
})

test_that("the dimension indices match a hand computation", {
  r <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                              eq5d_version = "3L"))

  for (d in dims) {
    # .prep_eq5d() coerces the 9 missing-data code to NA, so the hand
    # computation uses the in-range responses.
    x <- example_data[[d]]
    expected <- shannon_by_hand(x[x %in% 1:3], 3L)
    i <- which(r$dimension == d)
    expect_equal(r$H_All[i],    unname(expected["H"]),    info = d)
    expect_equal(r$Hmax_All[i], log2(3),                  info = d)
    expect_equal(r$J_All[i],    unname(expected["J"]),    info = d)
  }
})

test_that("the health state index uses complete profiles and all 243 states", {
  r <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                              eq5d_version = "3L"))
  i <- which(r$dimension == "Health state")

  state <- apply(example_data[, dims], 1, function(x)
    if (all(x %in% 1:3)) paste0(x, collapse = "") else NA_character_)
  expected <- shannon_by_hand(state, 243L)

  expect_equal(r$H_All[i], unname(expected["H"]))
  # L is what the instrument allows, not the 106 profiles observed.
  expect_equal(r$Hmax_All[i], log2(243))
  expect_equal(r$J_All[i], unname(expected["J"]))
})

test_that("H'max depends on the instrument, not on the data", {
  d3 <- data.frame(mo = c(1L, 2L, 3L), sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  d5 <- data.frame(mo = c(1L, 2L, 3L), sc = 1L, ua = 1L, pd = 1L, ad = 1L)

  r3 <- q(eq5d_profile_shannon(d3, names_eq5d = dims, eq5d_version = "3L"))
  r5 <- q(eq5d_profile_shannon(d5, names_eq5d = dims, eq5d_version = "5L"))

  # Identical responses, different denominators.
  expect_equal(r3$Hmax_All, c(rep(log2(3), 5), log2(243)))
  expect_equal(r5$Hmax_All, c(rep(log2(5), 5), log2(3125)))
  expect_equal(r3$H_All, r5$H_All)
  expect_true(all(r3$J_All >= r5$J_All))

  # EQ-5D-Y-3L is a three-level instrument.
  ry <- q(eq5d_profile_shannon(d3, names_eq5d = dims, eq5d_version = "Y3L"))
  expect_equal(ry$Hmax_All, r3$Hmax_All)
})

test_that("the bounds hold: J' runs from 0 to 1", {
  # Everyone in full health: one category used, so no information at all.
  same <- data.frame(mo = rep(1L, 20), sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  r <- q(eq5d_profile_shannon(same, names_eq5d = dims, eq5d_version = "3L"))
  expect_equal(r$H_All, rep(0, 6))
  expect_equal(r$J_All, rep(0, 6))

  # Every level of every dimension used equally: each dimension is maximally
  # informative.
  even <- data.frame(mo = rep(1:3, 9), sc = rep(1:3, 9), ua = rep(1:3, 9),
                     pd = rep(1:3, 9), ad = rep(1:3, 9))
  r2 <- q(eq5d_profile_shannon(even, names_eq5d = dims, eq5d_version = "3L"))
  expect_equal(r2$J_All[1:5], rep(1, 5))
  # ... but only three of the 243 profiles occur, so the state index is low.
  expect_lt(r2$J_All[6], 0.3)

  r3 <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                               eq5d_version = "3L"))
  expect_true(all(r3$J_All >= 0 & r3$J_All <= 1))
  expect_true(all(r3$H_All >= 0 & r3$H_All <= r3$Hmax_All))
})

# ---------------------------------------------------------------------------
# By follow-up
# ---------------------------------------------------------------------------

test_that("name_fu gives one set of columns per level, in order", {
  r <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                              eq5d_version = "3L", name_fu = "time",
                              levels_fu = c("Pre-op", "Post-op")))

  expect_identical(names(r),
                   c("dimension", "H_Pre-op", "Hmax_Pre-op", "J_Pre-op",
                     "H_Post-op", "Hmax_Post-op", "J_Post-op"))
  # The requested order, not the alphabetical one (H-3).
  r_rev <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                                  eq5d_version = "3L", name_fu = "time",
                                  levels_fu = c("Post-op", "Pre-op")))
  expect_identical(names(r_rev)[2], "H_Post-op")
  expect_identical(r_rev[["H_Pre-op"]], r[["H_Pre-op"]])
})

test_that("each follow-up level is computed on its own rows", {
  r <- q(eq5d_profile_shannon(example_data, names_eq5d = dims,
                              eq5d_version = "3L", name_fu = "time",
                              levels_fu = c("Pre-op", "Post-op")))

  pre <- example_data$mo[example_data$time == "Pre-op"]
  expected <- shannon_by_hand(pre[pre %in% 1:3], 3L)
  expect_equal(r[["H_Pre-op"]][1], unname(expected["H"]))
  expect_equal(r[["J_Pre-op"]][1], unname(expected["J"]))
})

test_that("an empty follow-up level is NA, with a warning naming it", {
  df <- example_data
  df$time <- as.character(df$time)
  w <- character(0)
  r <- withCallingHandlers(
    suppressMessages(eq5d_profile_shannon(
      df, names_eq5d = dims, eq5d_version = "3L", name_fu = "time",
      levels_fu = c("Pre-op", "Interim", "Post-op"))),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })

  expect_true(all(is.na(r[["H_Interim"]])))
  expect_true(all(is.na(r[["J_Interim"]])))
  expect_false(any(is.nan(unlist(r[, -1]))))
  expect_false(any(is.infinite(unlist(r[, -1]))))

  hit <- grep("follow-up level", w, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "\"Interim\"", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Plumbing shared with the other analysis functions
# ---------------------------------------------------------------------------

test_that("it validates its columns like every other analysis function", {
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  msg <- tryCatch(q(eq5d_profile_shannon(d, names_eq5d = dims,
                                         eq5d_version = "3L",
                                         name_fu = "visit")),
                  error = conditionMessage)
  expect_match(msg, "\"visit\" (from `name_fu`)", fixed = TRUE)

  # And it refuses data with nothing usable in it.
  bad <- data.frame(mo = 9L, sc = 9L, ua = 9L, pd = 9L, ad = 9L)
  msg2 <- tryCatch(q(eq5d_profile_shannon(bad, names_eq5d = dims,
                                          eq5d_version = "3L")),
                   error = conditionMessage)
  expect_match(msg2, "usable EQ-5D health state", fixed = TRUE)
})

test_that("it is listed among the package's analysis functions", {
  expect_true("eq5d_profile_shannon" %in% getNamespaceExports("eq5dsuite"))
})
