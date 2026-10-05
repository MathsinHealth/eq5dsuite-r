# Review of 2026-10-04, group 2: input validation and units.
#
# Q02 fractional or non-finite health-state codes were truncated to a state:
#     11111.9 scored as 11111 (direct GB 1, crosswalk 1, reverse 0.987).
# Q03 an infinite exact age was put in the oldest NICE age band and mapped.
# Q06 the UK mapping of aggregate values read a factor's level codes:
#     factor(c(.2, .8)) gave 0.867, NA where c(.2, .8) gives 0.448, 0.829.
# Q07 dim.names = rep("mo", 5) was accepted and mapped mobility five times.
# Q10 "Missing (%)" held a proportion (0.33 for one missing of three).

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")

# ── Q02 ───────────────────────────────────────────────────────────────────────

test_that("a fractional or non-finite state code is NA, with a warning, for every scorer", {
  bad <- c(11111.9, Inf, -Inf, NaN)
  scorers <- list(
    direct3L = function(x) eq5d(x, country = "GB", version = "3L"),
    direct5L = function(x) eq5d(x, country = "GB", version = "5L"),
    y3l      = function(x) eq5d(x, country = "DE", version = "Y3L"),
    eq5d3l   = function(x) eq5d3l(x, country = "GB"),
    xw       = function(x) eqxw(x, country = "GB"),
    xwr      = function(x) eqxwr(x, country = "GB"))
  for (nm in names(scorers)) {
    f <- scorers[[nm]]
    # Neighbouring valid states keep their values and order.
    x <- c(11111, 11111.9, 12321, Inf)
    expect_warning(got <- suppressMessages(f(x)), "not a whole", info = nm)
    ref <- suppressMessages(f(c(11111, 12321)))
    expect_identical(got[c(1, 3)], ref, info = nm)
    expect_true(all(is.na(got[c(2, 4)])), info = nm)
    for (b in bad) expect_true(is.na(q(f(b))), info = paste(nm, b))
  }
})

test_that("states given as text or factor labels are read through the labels", {
  ref <- q(eq5d(c(33333, 11111), country = "GB", version = "3L"))
  expect_identical(q(eq5d(c("33333", "11111"), country = "GB", version = "3L")), ref)
  expect_identical(q(eq5d(factor(c("33333", "11111")), country = "GB",
                          version = "3L")), ref)
  expect_identical(q(eqxwr(factor(c("33333", "11111")), country = "GB")),
                   q(eqxwr(c(33333, 11111), country = "GB")))
})

test_that("toEQ5Ddims() does not truncate a fractional code", {
  w <- testthat::capture_warnings(d <- toEQ5Ddims(c(12321, 12321.5)))
  expect_true(any(grepl("not a whole", w)))
  expect_identical(unname(unlist(d[1, ])), c(1, 2, 3, 2, 1))
  expect_true(all(is.na(d[2, ])))
})

# ── Q03 ───────────────────────────────────────────────────────────────────────

test_that("a non-finite exact age returns NA with a warning; valid ages unchanged", {
  for (f in list(eqxw_UK, eqxwr_UK)) {
    ref <- q(f(c(11111, 11111), c(30, 70), 1))
    expect_warning(got <- f(rep(11111, 5), c(30, Inf, -Inf, NaN, 70), 1),
                   "not a finite")
    expect_identical(got[c(1, 5)], ref)
    expect_true(all(is.na(got[2:4])))
  }
})

test_that("age band boundaries are unchanged", {
  ages <- c(16, 34.9, 35, 44.9, 45, 54.9, 55, 64.9, 65, 120)
  expect_identical(.nice_age_band(ages, "t"),
                   c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(suppressWarnings(.nice_age_band(15.9, "t")), NA_integer_)
  # Labels and factors are read through their labels.
  expect_identical(.nice_age_band(factor(c("30", "70")), "t"), c(1L, 5L))
  expect_identical(.nice_age_band(c("30", "70"), "t"), c(1L, 5L))
})

# ── Q06 ───────────────────────────────────────────────────────────────────────

test_that("aggregate values as factors or text map as the numbers do", {
  for (f in list(eqxw_UK, eqxwr_UK)) for (bw in list(0.4, 0.1, "default")) {
    num <- q(f(c(0.2, 0.8), 30, 1, bwidth = bw))
    expect_false(anyNA(num))
    expect_identical(q(f(factor(c(0.2, 0.8)), 30, 1, bwidth = bw)), num)
    # Levels in another order: still the labels.
    fx <- factor(c("0.2", "0.8"), levels = c("0.8", "0.2"))
    expect_identical(q(f(fx, 30, 1, bwidth = bw)), num)
    expect_identical(q(f(c("0.2", "0.8"), 30, 1, bwidth = bw)), num)
  }
})

test_that("a malformed value label is NA with a warning, never its factor code", {
  expect_warning(got <- eqxwr_UK(factor(c("0.2", "high", NA)), 30, 1,
                                 bwidth = 0.4),
                 "not a number")
  expect_identical(got[1], q(eqxwr_UK(0.2, 30, 1, bwidth = 0.4)))
  expect_true(all(is.na(got[2:3])))
})

# ── Q07 ───────────────────────────────────────────────────────────────────────

test_that("duplicate dimension selections are refused", {
  dd <- data.frame(mo = 1, sc = 2, ua = 3, pd = 2, ad = 1)
  for (f in list(eqxwr_UK, function(x, a, m, dim.names) eq5d(x, "GB", "3L", dim.names))) {
    expect_error(f(dd, 30, 1, dim.names = rep("mo", 5)), "more than once")
    expect_error(f(dd, 30, 1, dim.names = c("mo", "MO", "ua", "pd", "ad")),
                 "more than once")
  }
  expect_error(toEQ5Dindex(dd, dim.names = c("mo", "MO", "ua", "pd", "ad")),
               "more than once")
  # Distinct custom names give the standard result, in order.
  cust <- stats::setNames(dd, c("a", "b", "c", "d", "e"))
  expect_identical(eqxwr_UK(cust, 30, 1, dim.names = c("a", "b", "c", "d", "e")),
                   eqxwr_UK(12321, 30, 1))
  expect_equal(eqxwr_UK(dd, 30, 1), 0.6282253, tolerance = 1e-7)
})

test_that("dimension positions work, and duplicate or bad positions are refused", {
  dd <- data.frame(id = 1, sc = 2, mo = 1, ua = 3, pd = 2, ad = 1)
  expect_identical(eqxwr_UK(dd, 30, 1, dim.names = c(3, 2, 4, 5, 6)),
                   eqxwr_UK(12321, 30, 1))
  expect_error(eqxwr_UK(dd, 30, 1, dim.names = c(3, 3, 4, 5, 6)), "more than once")
  expect_error(eqxwr_UK(dd, 30, 1, dim.names = c(3, 2, 4, 5, 9)), "position")
})

# ── Q10 ───────────────────────────────────────────────────────────────────────

missing_pct <- function(tab, col) tab[[col]][tab$name == "Missing (%)"]

test_that("Missing (%) is a percentage, 0 to 100", {
  for (k in 0:3) {
    v <- c(rep(NA, k), seq_len(3 - k) * 10)
    d <- data.frame(fu = "pre", vas = v, utility = v / 100)
    vs <- q(eq5d_vas_summary(d, name_vas = "vas", name_fu = "fu", levels_fu = "pre"))
    us <- q(eq5d_utility_summary(d, name_utility = "utility", name_fu = "fu",
                                 levels_fu = "pre"))
    expect_equal(missing_pct(vs, "pre"), 100 * k / 3, info = k)
    expect_equal(missing_pct(us, "pre"), 100 * k / 3, info = k)
  }
  # A level with no rows has no percentage.
  d <- data.frame(fu = factor("pre", levels = c("pre", "post")), vas = 50)
  vs <- q(eq5d_vas_summary(d, name_vas = "vas", name_fu = "fu",
                           levels_fu = c("pre", "post")))
  expect_true(is.na(missing_pct(vs, "post")))
})

test_that("the displayed and exported tables show the same percentage", {
  d <- data.frame(fu = "pre", vas = c(NA, 80, 90))
  vs <- q(eq5d_vas_summary(d, name_vas = "vas", name_fu = "fu", levels_fu = "pre"))
  shown <- eq5d_format_table(vs)
  expect_identical(shown$pre[shown$name == "Missing (%)"], "33.33")
  # CSV export writes the number as computed.
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(vs, f, row.names = FALSE)
  back <- utils::read.csv(f)
  expect_equal(back$pre[back$name == "Missing (%)"], 100 / 3)
})
