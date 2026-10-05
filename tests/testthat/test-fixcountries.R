# Issue 3: .fixCountries() must say what is wrong with the country table
# instead of quietly returning NA.

test_that(".fixCountries() resolves built-in and user-defined codes", {
  local_eq_env()

  expect_identical(unname(.fixCountries("GB", "3L")), "GB")
  expect_identical(unname(.fixCountries("gb", "3L")), "GB")
  expect_identical(unname(.fixCountries("NL", "5L")), "NL")

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 1)
  )
  expect_identical(unname(.fixCountries("FAN", "3L")), "FAN")
  expect_identical(unname(.fixCountries("FA", "3L")), "FAN")
})

test_that("an unknown country is still NA, not an error", {
  local_eq_env()

  expect_true(is.na(.fixCountries("NOWHERE", "3L")))
})

test_that("a malformed built-in table gives an informative error", {
  pkgenv <- local_eq_env()

  cc <- pkgenv$country_codes
  cc[["3L"]] <- cc[["3L"]][, setdiff(colnames(cc[["3L"]]),
                                     c("Country_code", "citation")),
                           drop = FALSE]
  assign("country_codes", cc, envir = pkgenv)

  expect_error(.fixCountries("GB", "3L"),
               "built-in value set table for EQ-5D-3L")
  expect_error(.fixCountries("GB", "3L"), "Country_code")
  expect_error(.fixCountries("GB", "3L"), "citation")
  # And it points at the real cause rather than the country argument.
  expect_error(.fixCountries("GB", "3L"), "corrupted eq5dsuite installation")
})

test_that("a missing built-in table gives an informative error", {
  pkgenv <- local_eq_env()

  cc <- pkgenv$country_codes
  cc[["3L"]] <- NULL
  assign("country_codes", cc, envir = pkgenv)

  expect_error(.fixCountries("GB", "3L"), "is missing")
})

test_that("a built-in table of the wrong type gives an informative error", {
  pkgenv <- local_eq_env()

  cc <- pkgenv$country_codes
  cc[["3L"]] <- "not a table"
  assign("country_codes", cc, envir = pkgenv)

  expect_error(.fixCountries("GB", "3L"), "not a data.frame")
})

test_that("a malformed user table warns and is skipped, not fatal", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 1)
  )

  broken <- pkgenv$user_defined_3L
  broken$citation <- NULL
  assign("user_defined_3L", broken, envir = pkgenv)

  expect_warning(.fixCountries("GB", "3L"),
                 "user-defined value set table for EQ-5D-3L")
  expect_warning(.fixCountries("GB", "3L"), "citation")
  expect_warning(.fixCountries("GB", "3L"), "eqvs_add\\(\\)")

  # Built-in codes still resolve despite the broken user table.
  expect_identical(unname(suppressWarnings(.fixCountries("GB", "3L"))), "GB")
})

test_that("an unknown EQ-5D version is reported as such", {
  local_eq_env()

  expect_error(.fixCountries("GB", "4L"), "Unknown EQ-5D version '4L'")
  expect_error(.fixCountries("GB", "XW"), "Unknown EQ-5D version")
  expect_error(.fixCountries("GB", c("3L", "5L")), "single string")
})

test_that("lower-case instrument versions are accepted", {
  local_eq_env()

  # Reaches .fixCountries() via .add_utility() when a user passes
  # eq5d_version = "3l", which .prep_eq5d() allows.
  expect_identical(unname(.fixCountries("GB", "3l")), "GB")
  expect_identical(unname(.fixCountries("SI", "y3l")), "SI")
})
