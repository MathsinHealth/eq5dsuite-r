# eqxw() and eqxwr() had good line coverage but nothing asserted a number: the
# tests exercised the plumbing, not the mapping. A change to the probability
# matrices in R/sysdata.rda, or to how .fixPkgEnv() derives xwsets/xwrsets from
# them, would not have been caught.
#
# eqxw()  is the crosswalk of van Hout et al. (2012): an EQ-5D-5L health state
#         valued on an EQ-5D-3L value set.
# eqxwr() is the reverse crosswalk of van Hout and Shaw (2021): an EQ-5D-3L
#         health state valued on an EQ-5D-5L value set.

xw_env <- function() local_eq_env(.local_envir = parent.frame())

# ---------------------------------------------------------------------------
# The probability matrices behind both mappings
# ---------------------------------------------------------------------------

test_that("the crosswalk matrix is a proper distribution over 3L states", {
  pkgenv <- xw_env()
  p <- pkgenv$probs5t3

  # One row per EQ-5D-5L state, one column per EQ-5D-3L state.
  expect_identical(dim(p), c(3125L, 243L))
  expect_false(anyNA(p))
  expect_false(any(p < 0))
  # Every 5L state maps onto the 3L states with total probability 1.
  expect_equal(unname(rowSums(p)), rep(1, 3125L))
})

test_that("the reverse crosswalk matrix is non-negative and complete", {
  pkgenv <- xw_env()
  p <- pkgenv$probs

  expect_identical(dim(p), c(243L, 3125L))
  expect_false(anyNA(p))
  expect_false(any(p < 0))
  expect_equal(unname(rowSums(p)), rep(1, 243L))
})

# ---------------------------------------------------------------------------
# The mapped values themselves
# ---------------------------------------------------------------------------

test_that("full health and the worst state map as expected", {
  xw_env()

  # 55555 can only come from 33333, and 11111 only from 11111, so both map to
  # the endpoints of the target value set. (The EQ-5D-3L tables are stored at
  # float precision, hence the tolerance.)
  expect_equal(unname(eqxw(c(11111, 55555), country = "GB")),
               c(1, -0.594), tolerance = 1e-6)
  expect_equal(unname(eqxw(c(11111, 55555), country = "US")),
               c(1, -0.102), tolerance = 1e-6)

  # The reverse direction is a probability-weighted average over 5L states, so
  # neither end is exact.
  expect_equal(unname(eqxwr(c(11111, 33333), country = "GB")),
               c(0.987208, -0.464926), tolerance = 1e-6)
  expect_equal(unname(eqxwr(c(11111, 33333), country = "US")),
               c(0.982955, -0.506695), tolerance = 1e-6)
})

test_that("mapped values stay inside the target value set's range", {
  pkgenv <- xw_env()

  three <- range(pkgenv$vsets3L_combined$GB)
  got <- eqxw(make_all_EQ_indexes("5L"), country = "GB")
  expect_length(got, 3125L)
  expect_false(anyNA(got))
  expect_gte(min(got), three[1] - 1e-9)
  expect_lte(max(got), three[2] + 1e-9)

  five <- range(pkgenv$vsets5L_combined$GB)
  got_r <- eqxwr(make_all_EQ_indexes("3L"), country = "GB")
  expect_length(got_r, 243L)
  expect_false(anyNA(got_r))
  expect_gte(min(got_r), five[1] - 1e-9)
  expect_lte(max(got_r), five[2] + 1e-9)
})

test_that("worsening a dimension never raises the mapped value", {
  xw_env()

  for (direction in c("xw", "xwr")) {
    nlev   <- if (direction == "xw") 5L else 3L
    states <- make_all_EQ_indexes(if (direction == "xw") "5L" else "3L")
    val    <- if (direction == "xw") eqxw(states, "GB") else eqxwr(states, "GB")
    lv <- do.call(rbind, lapply(strsplit(sprintf("%05d", states), ""), as.integer))

    viol <- 0L
    for (k in 1:5) {
      w <- lv
      w[, k] <- pmin(w[, k] + 1L, nlev)
      idx <- match(as.integer(apply(w, 1, paste0, collapse = "")), states)
      viol <- viol + sum(lv[, k] < nlev & val[idx] > val + 1e-9)
    }
    # Unlike some published value sets, both mappings are monotone for GB.
    expect_identical(viol, 0L, info = direction)
  }
})

test_that("eqxw() is eq5d(version = \"XW\"), and eqxwr() is \"XWR\"", {
  xw_env()
  states5 <- c(11111, 12345, 55555)
  states3 <- c(11111, 12321, 33333)

  expect_identical(eqxw(states5, country = "GB"),
                   eq5d(states5, country = "GB", version = "XW"))
  expect_identical(eqxwr(states3, country = "GB"),
                   eq5d(states3, country = "GB", version = "XWR"))
})

test_that("several value sets at once give one column each", {
  xw_env()
  r <- suppressWarnings(eqxw(c(11111, 55555), country = c("GB", "US")))
  expect_identical(colnames(r), c("GB", "US"))
  expect_identical(r[, "GB"], eqxw(c(11111, 55555), country = "GB"))

  rr <- suppressWarnings(eqxwr(c(11111, 33333), country = c("GB", "US")))
  expect_identical(colnames(rr), c("GB", "US"))
  expect_identical(rr[, "US"], eqxwr(c(11111, 33333), country = "US"))
})

test_that("invalid states come back as NA, in place", {
  xw_env()
  # 55555 is not an EQ-5D-3L state, so the reverse crosswalk cannot take it.
  expect_identical(eqxwr(c(11111, 55555, 33333), country = "GB")[2], NA_real_)
  # 66666 is not a state of either instrument.
  expect_identical(eqxw(c(11111, 66666), country = "GB")[2], NA_real_)
  expect_null(names(eqxw(c(11111, 66666), country = "GB")))
})
