# A user-defined value set code that duplicates a built-in one used to be
# accepted with nothing but a "was added" message, and it broke every lookup
# for that instrument. .fixPkgEnv() combines the built-in and user-defined
# tables with merge(), which treats a shared column name as an extra join key:
#
#   eqvs_add(..., VSCode = "GB")      # accepted
#   nrow(vsets5L_combined)            # 0, was 3125
#   eq5d5l(11111, "GB")               # Error: Multiple value sets ... 'GB', 'GB'
#
# A code differing only in case did the same damage by a different route:
# .fixCountries() matches case-insensitively, so "gb" and the built-in "GB"
# were reported as ambiguous for every GB lookup.
#
# The realistic trigger is a value set that is user-defined today and built-in
# tomorrow: a user adds it by hand, then upgrades the package.

builtin_5l <- function(code = "GB") {
  df <- data.frame(state = make_all_EQ_indexes("5L"),
                   value = round(seq(1, -0.5, length.out = 3125L), 4))
  names(df)[2] <- code
  df
}

builtin_3l <- function(code = "GB") {
  df <- data.frame(state = make_all_EQ_indexes("3L"),
                   value = round(seq(1, -0.5, length.out = 243L), 4))
  names(df)[2] <- code
  df
}

# ---------------------------------------------------------------------------
# The guard
# ---------------------------------------------------------------------------

test_that("a built-in code is rejected", {
  local_eq_env()
  expect_error(
    eqvs_add(builtin_5l("GB"), version = "5L", VSCode = "GB", saveOption = 1),
    "Value set code 'GB' is already used by the built-in value set 'GB'",
    fixed = TRUE)
})

test_that("the error names the code, the instruments and the way out", {
  local_eq_env()
  msg <- tryCatch(
    eqvs_add(builtin_5l("GB"), version = "5L", VSCode = "GB", saveOption = 1),
    error = conditionMessage)

  expect_match(msg, "'GB'", fixed = TRUE)                 # the conflicting code
  expect_match(msg, "(EQ-5D-3L, EQ-5D-5L)", fixed = TRUE) # where it is built in
  expect_match(msg, "choose a different code", fixed = TRUE)
  expect_match(msg, "`VSCode`", fixed = TRUE)
  expect_match(msg, "eqvs_display(version = \"5L\")", fixed = TRUE)
})

test_that("matching is case-insensitive", {
  for (code in c("gb", "Gb", "gB")) {
    local_eq_env()
    expect_error(
      eqvs_add(builtin_5l(code), version = "5L", VSCode = code, saveOption = 1),
      "already used by the built-in value set 'GB'", fixed = TRUE)
  }

  # A suffixed code too, where the case is mixed within the suffix.
  local_eq_env()
  expect_error(
    eqvs_add(builtin_3l("de_tto"), version = "3L", VSCode = "de_tto",
             saveOption = 1),
    "already used by the built-in value set 'DE_TTO'", fixed = TRUE)
})

test_that("codes built in for another instrument are rejected too", {
  # NG is an EQ-5D-5L code only; BE is 5L and Y-3L; DE_TTO is 3L only.
  local_eq_env()
  expect_error(eqvs_add(builtin_3l("NG"), version = "3L", VSCode = "NG",
                        saveOption = 1),
               "(EQ-5D-5L)", fixed = TRUE)

  local_eq_env()
  expect_error(eqvs_add(builtin_5l("DE_TTO"), version = "5L",
                        VSCode = "DE_TTO", saveOption = 1),
               "(EQ-5D-3L)", fixed = TRUE)

  local_eq_env()
  expect_error(eqvs_add(builtin_5l("BE"), version = "5L", VSCode = "BE",
                        saveOption = 1),
               "(EQ-5D-5L, EQ-5D-Y-3L)", fixed = TRUE)
})

test_that("the code is taken from the second column when VSCode is omitted", {
  local_eq_env()
  expect_error(eqvs_add(builtin_5l("GB"), version = "5L", saveOption = 1),
               "already used by the built-in value set 'GB'", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Nothing is left half-done
# ---------------------------------------------------------------------------

test_that("a rejected value set leaves the package environment untouched", {
  pkgenv <- local_eq_env()
  before <- list(n5 = nrow(pkgenv$vsets5L_combined),
                 u5 = colnames(pkgenv$uservsets5L),
                 cc = pkgenv$country_codes[["5L"]])

  expect_error(eqvs_add(builtin_5l("GB"), version = "5L", VSCode = "GB",
                        saveOption = 1))

  expect_identical(nrow(pkgenv$vsets5L_combined), 3125L)
  expect_identical(nrow(pkgenv$vsets5L_combined), before$n5)
  expect_identical(colnames(pkgenv$uservsets5L), before$u5)
  expect_identical(pkgenv$country_codes[["5L"]], before$cc)
  expect_false("user_defined_5L" %in% names(pkgenv))

  # The built-in set is still reachable, which was the whole point.
  expect_equal(unname(eq5d5l(11111, country = "GB")), 1)
})

test_that("a rejected value set is not written to the cache", {
  cache_dir <- withr::local_tempdir()
  local_eq_env(cache_dir)

  expect_error(eqvs_add(builtin_5l("GB"), version = "5L", VSCode = "GB",
                        saveOption = 2))
  expect_false(file.exists(file.path(cache_dir, .cache_basename)))
})

# ---------------------------------------------------------------------------
# What must still be allowed
# ---------------------------------------------------------------------------

test_that("codes that are not built in are still accepted", {
  for (code in c("FAN", "GB_MINE", "XX", "NGX")) {
    local_eq_env()
    expect_message(
      eqvs_add(builtin_5l(code), version = "5L", VSCode = code,
               saveOption = 1),
      "was added")
    expect_true(code %in% eqvs_display(version = "5L",
                                       return_df = TRUE)$VS_code)
  }
})

test_that("a user-defined set may still reuse a built-in country code", {
  # Deliberate, and covered by test-ambiguous-country.R and
  # test-value-set-metadata.R: .fixCountries() resolves an exact value set code
  # ahead of a country code, so this stays reachable. Only VS_code collides.
  local_eq_env()
  expect_message(
    eqvs_add(builtin_5l("GB_MINE"), version = "5L", country = "My GB set",
             countryCode = "GB", VSCode = "GB_MINE", saveOption = 1),
    "was added")
  expect_equal(unname(eq5d5l(11111, country = "GB_MINE")), 1)
  expect_equal(unname(eq5d5l(11111, country = "GB")), 1)
})

test_that("'UK' is still free, because the built-in code is now 'GB'", {
  local_eq_env()
  expect_false("UK" %in% .builtin_vs_codes())
  expect_message(
    eqvs_add(builtin_3l("UK"), version = "3L", country = "My UK set",
             countryCode = "UK", VSCode = "UK", saveOption = 1),
    "was added")
})

# ---------------------------------------------------------------------------
# The helpers
# ---------------------------------------------------------------------------

test_that(".builtin_vs_codes() covers every instrument", {
  codes <- .builtin_vs_codes()
  expect_false(anyDuplicated(codes) > 0L)
  expect_false("state" %in% codes)
  for (v in .cache_versions) {
    expect_true(all(.cntrcodes$VS_code[.cntrcodes$Version == v] %in% codes))
    tab <- get(paste0(".vsets", v), envir = asNamespace("eq5dsuite"))
    expect_true(all(setdiff(colnames(tab), "state") %in% codes))
  }
  expect_true(all(c("GB", "NG", "DE_TTO", "BE") %in% codes))
})

test_that(".builtin_vs_versions() reports instruments in a fixed order", {
  expect_identical(.builtin_vs_versions("GB"), c("EQ-5D-3L", "EQ-5D-5L"))
  expect_identical(.builtin_vs_versions("gb"), c("EQ-5D-3L", "EQ-5D-5L"))
  expect_identical(.builtin_vs_versions("NG"), "EQ-5D-5L")
  expect_identical(.builtin_vs_versions("BE"), c("EQ-5D-5L", "EQ-5D-Y-3L"))
  expect_identical(.builtin_vs_versions("NOWHERE"), character(0))
})
