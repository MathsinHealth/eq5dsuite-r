# No test asserted a computed value for any of the 94 built-in value sets, so a
# regression in R/sysdata.rda or in .fixPkgEnv() would not have been caught.
#
# Two kinds of check here.
#
# 1. Invariants that hold for every set, checked against the sets themselves:
#    the right number of states, no missing values, full health as the maximum.
#    These are structural, not circular.
#
# 2. A snapshot of the value of the worst state in every set, rounded to four
#    decimals. This is generated from the current data and exists to fail if
#    the data change. A legitimate value set update should change it, and the
#    change should be reviewed -- 94 numbers in one place is the point.
#
# What is deliberately NOT asserted: monotonicity. Six of the 94 published sets
# contain dominance violations, so it is not an invariant of the data. See the
# test at the end, which records which ones and holds the count.

vs_env <- function() local_eq_env(.local_envir = parent.frame())

worst_state <- function(version) if (version == "5L") 55555L else 33333L

all_sets <- function(pkgenv) {
  out <- list()
  for (v in .cache_versions) {
    tab <- pkgenv[[paste0("vsets", v, "_combined")]]
    for (cc in setdiff(names(tab), "state"))
      out[[length(out) + 1L]] <- list(version = v, code = cc,
                                      state = tab$state, value = tab[[cc]])
  }
  out
}

# ---------------------------------------------------------------------------
# Invariants
# ---------------------------------------------------------------------------

test_that("every value set covers its whole state space, with no gaps", {
  pkgenv <- vs_env()
  sets <- all_sets(pkgenv)
  expect_identical(length(sets), 94L)

  for (s in sets) {
    expected_n <- if (s$version == "5L") 3125L else 243L
    info <- paste(s$version, s$code)
    expect_identical(length(s$value), expected_n, info = info)
    expect_type(s$value, "double")
    expect_false(anyNA(s$value), info = info)
    expect_false(anyDuplicated(s$state) > 0L, info = info)
    expect_setequal(s$state, make_all_EQ_indexes(if (s$version == "5L") "5L" else "3L"))
  }
})

test_that("full health is the highest value in every set", {
  pkgenv <- vs_env()
  for (s in all_sets(pkgenv)) {
    info <- paste(s$version, s$code)
    expect_identical(which.max(s$value), which(s$state == 11111L), info = info)
  }
})

test_that("full health is 1 in all but two sets", {
  pkgenv <- vs_env()
  not_one <- character(0)
  for (s in all_sets(pkgenv)) {
    v <- s$value[s$state == 11111L]
    if (!isTRUE(all.equal(v, 1, tolerance = 1e-12)))
      not_one <- c(not_one, paste0(s$version, "/", s$code, "=", round(v, 4)))
  }
  # Canada and Sweden (2020) anchor their EQ-5D-5L sets below 1. That is a
  # property of the published sets, not of the package.
  expect_identical(sort(not_one), c("5L/CA=0.9489", "5L/SE_2020=0.976"))
})

test_that("every set spans a plausible EQ-5D range", {
  pkgenv <- vs_env()
  for (s in all_sets(pkgenv)) {
    info <- paste(s$version, s$code)
    expect_lte(max(s$value), 1, label = info)
    expect_gte(min(s$value), -2, label = info)
    # A value set that collapsed to a handful of numbers would be a corrupted
    # import. Ties are ordinary -- EQ-5D-3L Germany (TTO) has the fewest, 63
    # distinct values across its 243 states -- so the bar is set well below
    # that.
    expect_gt(length(unique(s$value)), 50L, label = info)
  }
})

# ---------------------------------------------------------------------------
# The worst state, in every set
# ---------------------------------------------------------------------------

test_that("the worst state has the value it had when this was written", {
  pkgenv <- vs_env()

  expected <- list(
    "3L" = c(
      GB = -0.5940, US = -0.1020, CA = -0.3400, AR_TTO = -0.3760,
      AU = -0.2170, CL = -0.4970, CN = 0.1702, DK = -0.6240, FR = -0.5300,
      DE_TTO = -0.2050, HU = -0.8650, IR = -0.1130, IT = -0.3800,
      JP = -0.1110, PL = -0.5230, SG = -0.7694, SI_TTO = -0.4980,
      KR = -0.1710, ES = -0.6540, LK = -0.7110, SE = 0.3402, TW = -0.6740,
      TH = -0.4520, NL_2006 = -0.3290, TT = -0.1630, TN = -0.7960,
      PT = -0.4960, ZW = -0.1450, AR_VAS = -0.0220, BE_VAS = -0.1580,
      DE_VAS = 0.0207, MY_VAS = 0.1310, NZ_VAS = -0.0848, SI_VAS = 0.0219,
      FI_VAS = -0.0110, BM = -0.5470, PK = -0.1710, JO = -0.5630,
      BR = -0.1770, NL_2026 = -0.7230),
    "5L" = c(
      CA = -0.1482, CN = -0.3910, DE = -0.6610, HK = -0.8650, ID = -0.8650,
      IE = -0.9740, JP = -0.0254, KR = -0.0660, NL = -0.4463, ES = -0.4162,
      TH = -0.4211, UY = -0.2638, BE = -0.5330, DK = -0.7580, ET = -0.7184,
      FR = -0.5255, HU = -0.8480, MY = -0.4420, MX = -0.5960, PE = -1.0760,
      PL = -0.5900, PT = -0.6030, TW = -1.0259, US = -0.5730, UG = -1.1160,
      VN = -0.5115, IN = -0.9225, IT = -0.5710, AU = -0.3010, PH = -0.4381,
      RO = -0.3229, IR = -1.1900, SA = -0.6830, TT = -0.5630, NO = -0.4530,
      MA = -1.4910, AE = -0.6540, GH = -0.4930, SI = -1.0890, NZ = -0.8300,
      SE_2020 = 0.2430, SE_2022 = -0.3140, GB = -0.5670, NG = -0.7330),
    "Y3L" = c(
      DE = -0.2827, JP = 0.2890, SI = -0.6910, ES = -0.5392, BE = -0.4755,
      NL = -0.2180, ID = -0.0862, HU = -0.4850, CN = -0.0890, BR = -0.0060))

  for (v in .cache_versions) {
    tab <- pkgenv[[paste0("vsets", v, "_combined")]]
    codes <- setdiff(names(tab), "state")
    # Every set is accounted for, and nothing has appeared or vanished.
    expect_setequal(codes, names(expected[[v]]))

    got <- vapply(codes, function(cc) tab[[cc]][tab$state == worst_state(v)],
                  numeric(1))
    expect_equal(round(got, 4), expected[[v]][codes], tolerance = 0)
  }
})

test_that("the exported accessors return those same values", {
  vs_env()
  # The snapshot above reads the combined table; these go through the user-
  # facing path, so a break in .fixCountries() or eq5d() shows up here.
  expect_equal(round(unname(eq5d3l(33333, country = "GB")), 4), -0.594)
  expect_equal(round(unname(eq5d5l(55555, country = "GB")), 4), -0.567)
  expect_equal(round(unname(eq5dy3l(33333, country = "NL")), 4), -0.218)
  expect_equal(round(unname(eq5d5l(55555, country = "NG")), 4), -0.733)
  # The two sets that do not anchor full health at 1.
  expect_equal(round(unname(eq5d5l(11111, country = "CA")), 4), 0.9489)
  expect_equal(round(unname(eq5d5l(11111, country = "SE_2020")), 4), 0.976)
})

# ---------------------------------------------------------------------------
# Monotonicity: a property of most sets, not of the data as a whole
# ---------------------------------------------------------------------------

test_that("six published sets contain dominance violations, and no others do", {
  pkgenv <- vs_env()

  violations <- function(s) {
    nlev <- if (s$version == "5L") 5L else 3L
    lv <- do.call(rbind, lapply(strsplit(sprintf("%05d", s$state), ""),
                                as.integer))
    n <- 0L
    for (k in 1:5) {
      w <- lv
      w[, k] <- pmin(w[, k] + 1L, nlev)
      idx <- match(as.integer(apply(w, 1, paste0, collapse = "")), s$state)
      n <- n + sum(lv[, k] < nlev & s$value[idx] > s$value + 1e-12)
    }
    n
  }

  got <- character(0)
  for (s in all_sets(pkgenv)) {
    n <- violations(s)
    if (n > 0L) got <- c(got, paste0(s$version, "/", s$code, "=", n))
  }

  # Worsening on one dimension raises the value somewhere in each of these.
  # It is a feature of the published value sets; recorded so that a new one is
  # noticed, and so that nobody reads monotonicity into the other 88.
  expect_identical(sort(got),
                   sort(c("3L/US=8", "3L/AR_TTO=202", "3L/LK=12", "3L/PT=83",
                          "3L/AR_VAS=135", "3L/JO=2")))
})

test_that("the worst state is the minimum in all but nine sets", {
  pkgenv <- vs_env()
  n_not <- 0L
  for (s in all_sets(pkgenv))
    if (which.min(s$value) != which(s$state == worst_state(s$version)))
      n_not <- n_not + 1L
  # A consequence of the dominance violations above, plus Sweden 2020.
  expect_identical(n_not, 9L)
})
