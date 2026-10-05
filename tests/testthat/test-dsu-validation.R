# Validation of eqxw_UK() and eqxwr_UK() against the NICE DSU's own R command.
#
# The fixtures hold the DSU's results, produced by
# dsu_reference/validate_against_package.R, which runs the DSU's fun_map()
# directly. They let this comparison run as an ordinary test without the DSU
# material being present.
#
# Coverage:
#   health state input : every state x every age band x both sexes
#                        (31,250 rows for 5L -> 3L, 2,430 for 3L -> 5L)
#   value input        : 12 values x 5 bandwidths (including "default")
#                        x 3 age bands x both sexes, per direction
#
# Tolerance is 1e-6 throughout, as agreed. Observed agreement is much tighter;
# see the comment on the value tests for the one direction that is not exact.

tol <- 1e-6

quietly <- function(expr) suppressWarnings(suppressMessages(expr))

# ---- health state input -----------------------------------------------------

test_that("eqxw_UK() reproduces the DSU for every 5L state, band and sex", {
  fx <- readRDS(test_path("fixture-dsu-state-5Lto3L.rds"))
  expect_equal(nrow(fx), 31250L)
  expect_false(anyNA(fx$dsu))

  got <- as.numeric(quietly(eqxw_UK(fx$state, age = fx$age, male = fx$male)))
  expect_false(anyNA(got))
  expect_equal(got, fx$dsu, tolerance = tol)
})

test_that("eqxwr_UK() reproduces the DSU for every 3L state, band and sex", {
  fx <- readRDS(test_path("fixture-dsu-state-3Lto5L.rds"))
  expect_equal(nrow(fx), 2430L)
  expect_false(anyNA(fx$dsu))

  got <- as.numeric(quietly(eqxwr_UK(fx$state, age = fx$age, male = fx$male)))
  expect_false(anyNA(got))
  expect_equal(got, fx$dsu, tolerance = tol)
})

# ---- value ("score") input --------------------------------------------------

run_value_fixture <- function(fx, fun) {
  vapply(seq_len(nrow(fx)), function(i) {
    bw <- if (fx$bwidth[i] == "default") "default" else as.numeric(fx$bwidth[i])
    as.numeric(quietly(fun(fx$score[i], age = fx$age[i], male = fx$male[i],
                           bwidth = bw)))
  }, numeric(1L))
}

test_that("eqxw_UK() reproduces the DSU for value input across bandwidths", {
  fx  <- readRDS(test_path("fixture-dsu-value-5Lto3L.rds"))
  got <- run_value_fixture(fx, eqxw_UK)

  # Both implementations must return NA on exactly the same rows: those are
  # the values with no point of the source value set inside the bandwidth.
  expect_equal(is.na(got), is.na(fx$dsu))

  ok <- !is.na(fx$dsu)
  expect_gt(sum(ok), 0L)
  expect_equal(got[ok], fx$dsu[ok], tolerance = tol)
})

test_that("eqxwr_UK() reproduces the DSU for value input across bandwidths", {
  fx  <- readRDS(test_path("fixture-dsu-value-3Lto5L.rds"))
  got <- run_value_fixture(fx, eqxwr_UK)

  expect_equal(is.na(got), is.na(fx$dsu))

  ok <- !is.na(fx$dsu)
  expect_gt(sum(ok), 0L)

  # This direction agrees to about 8e-08 rather than machine precision. The
  # kernel matches on the source value set, which here is the UK EQ-5D-3L set
  # (Dolan 1997). The DSU's CSV stores those values rounded, while the package
  # holds them at full precision in .vsets3L$GB; the two differ by up to
  # 1.02e-07, which shifts the Epanechnikov weights very slightly. The 5L
  # source column agrees to 3.3e-16, which is why the other direction is exact.
  expect_equal(got[ok], fx$dsu[ok], tolerance = tol)
})

test_that("the recommended bandwidths are exercised by the fixtures", {
  for (f in c("fixture-dsu-value-5Lto3L.rds", "fixture-dsu-value-3Lto5L.rds")) {
    fx <- readRDS(test_path(f))
    expect_true("default" %in% fx$bwidth)
    expect_setequal(unique(fx$bwidth), c("0", "0.05", "0.1", "0.4", "default"))
  }
})
