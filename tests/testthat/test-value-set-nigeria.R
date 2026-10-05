# The EQ-5D-5L value set for Nigeria, in R/sysdata.rda since 2.1.0. The 3,125
# values were computed from the published coefficients and checked against the
# paper's anchor states.
#
# Source: Yusuf AH, Ardo BU, Thavorncharoensap M, Roudijk B, Purba FD,
# Yang Z, Liao M, Chaikledkaew U, Youngkong S, Thakkinstian A,
# Agada-Amade YA, Amole TG, Sambo MN, Ohiri K. The EQ-5D-5L valuation study
# in Nigeria. Quality of Life Research. 2026;35(8):206.
# doi:10.1007/s11136-026-04319-4
#
# The values are generated from the published hybrid model (model 26) and
# stored unrounded, so the anchor values below are compared after rounding to
# the three decimal places the paper reports.

# Values reported in the paper.
nigeria_reference <- c("11111" =  1,
                       "21111" =  0.963,
                       "23455" = -0.153,
                       "54555" = -0.653,
                       "55555" = -0.733)

# ---------------------------------------------------------------------------
# The value set itself
# ---------------------------------------------------------------------------

test_that("Nigeria has a complete set of 3,125 values", {
  pkgenv <- local_eq_env()
  v5 <- pkgenv$vsets5L_combined

  expect_true("NG" %in% names(v5))
  expect_equal(nrow(v5), 3125L)
  expect_equal(sum(!is.na(v5$NG)), 3125L)
  expect_false(anyNA(v5$NG))
  expect_type(v5$NG, "double")
})

test_that("Nigeria reproduces the values reported in the paper", {
  states <- as.integer(names(nigeria_reference))
  got    <- unname(eq5d5l(states, country = "NG"))

  expect_equal(round(got, 3), unname(nigeria_reference), tolerance = 0)

  # Full health is exactly 1, and the worst state is the minimum of the set.
  expect_identical(unname(eq5d5l(11111, country = "NG")), 1)
  all_states <- make_all_EQ_indexes("5L")
  all_values <- unname(eq5d5l(all_states, country = "NG"))
  expect_identical(all_states[which.min(all_values)], 55555L)
  expect_equal(round(min(all_values), 3), -0.733, tolerance = 0)
})

test_that("Nigeria values are logically consistent", {
  # For any two states where one is no worse on every dimension, its value
  # must be at least as high. Checking every single-level worsening is
  # sufficient: dominance follows by transitivity.
  states <- make_all_EQ_indexes("5L")
  values <- unname(eq5d5l(states, country = "NG"))
  lv     <- do.call(rbind, lapply(strsplit(sprintf("%05d", states), ""),
                                  as.integer))

  violations <- 0L
  for (k in 1:5) {
    worse <- lv
    worse[, k] <- pmin(worse[, k] + 1L, 5L)
    idx <- match(as.integer(apply(worse, 1, paste0, collapse = "")), states)
    # Only rows that actually worsened
    moved <- lv[, k] < 5L
    violations <- violations +
      sum(moved & values[idx] > values + 1e-12)
  }
  expect_equal(violations, 0L)
})

# ---------------------------------------------------------------------------
# Integration
# ---------------------------------------------------------------------------

test_that("eq5d5l() works for Nigeria", {
  got <- unname(eq5d5l(c(11111, 55555), country = "NG"))
  expect_length(got, 2L)
  expect_false(anyNA(got))
  expect_equal(round(got, 3), c(1, -0.733), tolerance = 0)

  # The generic entry point agrees.
  expect_identical(unname(eq5d(c(11111, 55555), country = "NG", version = "5L")), got)
})

test_that("Nigeria appears in eqvs_display() in the right alphabetical place", {
  pkgenv <- local_eq_env()
  df <- eqvs_display(version = "5L", return_df = TRUE)

  expect_true("NG" %in% df$VS_code)
  row <- df[df$VS_code == "NG", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$Name_short, "Nigeria")
  expect_identical(row$Country_code, "NG")

  i <- which(df$VS_code == "NG")
  expect_identical(df$Name_short[i - 1L], "New Zealand")
  expect_identical(df$Name_short[i + 1L], "Norway")

  # The whole 5L list is still sorted by Name_short, then VS_code.
  expect_identical(
    df$Name_short,
    df$Name_short[order(df$Name_short, df$VS_code, method = "radix")]
  )
})

test_that("Nigeria's metadata follows the schema used by the other sets", {
  pkgenv <- local_eq_env()
  cc  <- pkgenv$country_codes[["5L"]]
  row <- cc[cc$VS_code == "NG", ]

  expect_equal(nrow(row), 1L)
  expect_identical(row$Version, "5L")
  expect_identical(row$Name, "Nigeria")
  expect_identical(row$doi, "10.1007/s11136-026-04319-4")
  expect_match(row$citation, "^Yusuf AH, Ardo BU,")
  expect_match(row$citation, "The EQ-5D-5L valuation study in Nigeria")
  expect_match(row$citation, "Quality of Life Research\\. 2026;35\\(8\\):206")
  expect_match(row$citation, "doi:10\\.1007/s11136-026-04319-4$")
})

# ---------------------------------------------------------------------------
# Nothing else moved
# ---------------------------------------------------------------------------

test_that("adding Nigeria did not disturb the other value sets", {
  pkgenv <- local_eq_env()

  # Counts, after the addition.
  expect_equal(nrow(pkgenv$country_codes[["5L"]]),  44L)
  expect_equal(nrow(pkgenv$country_codes[["3L"]]),  40L)
  expect_equal(nrow(pkgenv$country_codes[["Y3L"]]), 10L)

  # Codes are unique within each version, and metadata still matches the
  # value tables.
  for (v in c("3L", "5L", "Y3L")) {
    codes <- pkgenv$country_codes[[v]]$VS_code
    expect_false(anyDuplicated(codes) > 0L)
    tab <- pkgenv[[paste0("vsets", v, "_combined")]]
    expect_setequal(codes, setdiff(names(tab), "state"))
  }

  # A spot check on other value sets, at full stored precision. These are
  # independent of Nigeria and must not move.
  expect_identical(unname(eq5d5l(11111, country = "GB")), 1)
  expect_equal(unname(eq5d5l(55555, country = "GB")), -0.567, tolerance = 1e-12)
  expect_identical(unname(eq5d3l(11111, country = "GB")), 1)
  # the EQ-5D-3L table is stored at float precision, hence the loose tolerance
  expect_equal(unname(eq5d3l(33333, country = "GB")), -0.594, tolerance = 1e-7)

  # NG is a 5L code only; it must not have leaked into the other instruments.
  expect_false("NG" %in% pkgenv$country_codes[["3L"]]$VS_code)
  expect_false("NG" %in% pkgenv$country_codes[["Y3L"]]$VS_code)
})
