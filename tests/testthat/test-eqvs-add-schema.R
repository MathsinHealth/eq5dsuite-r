# Issue 1: eqvs_add() must build user_defined_* rows with the same columns,
# order and types as country_codes.

test_that("eqvs_add() produces a row matching country_codes exactly", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN",
             description = "doi:test", saveOption = 1)
  )

  builtin <- pkgenv$country_codes[["3L"]]
  user    <- pkgenv$user_defined_3L

  expect_identical(colnames(user), colnames(builtin))
  expect_identical(vapply(user, function(x) class(x)[1L], character(1L)),
                   vapply(builtin, function(x) class(x)[1L], character(1L)))
  expect_length(.validate_vs_meta(user), 0L)
})

test_that("the two tables can simply be rbind()ed", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 1)
  )

  combined <- expect_no_error(
    rbind(pkgenv$country_codes[["3L"]], pkgenv$user_defined_3L)
  )
  expect_identical(nrow(combined),
                   nrow(pkgenv$country_codes[["3L"]]) + 1L)
  expect_true("FAN" %in% combined$VS_code)
})

test_that("omitted optional arguments give typed NA, not logical NA", {
  pkgenv <- local_eq_env()

  # No country, countryCode, VSCode or description supplied.
  suppressMessages(
    eqvs_add(dummy_vs("3L", "ZZZ"), version = "3L", saveOption = 1)
  )

  user <- pkgenv$user_defined_3L
  expect_length(.validate_vs_meta(user), 0L)
  expect_type(user$Name, "character")
  expect_type(user$doi, "character")
  expect_type(user$citation, "character")
  expect_true(is.na(user$Name))
  # The value column name is used as the fallback identifier.
  expect_identical(user$VS_code, "ZZZ")
})

test_that("citation is present but NA for user-defined sets", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("5L", "FAN5"), version = "5L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN5", saveOption = 1)
  )

  expect_true("citation" %in% colnames(pkgenv$user_defined_5L))
  expect_true(is.na(pkgenv$user_defined_5L$citation))
})

test_that("the schema holds after adding several value sets", {
  pkgenv <- local_eq_env()

  suppressMessages({
    eqvs_add(dummy_vs("3L", "AAA", seed = 1), version = "3L",
             VSCode = "AAA", saveOption = 1)
    eqvs_add(dummy_vs("3L", "BBB", seed = 2), version = "3L",
             country = "Betaland", VSCode = "BBB", saveOption = 1)
  })

  user <- pkgenv$user_defined_3L
  expect_identical(nrow(user), 2L)
  expect_length(.validate_vs_meta(user), 0L)
  expect_identical(user$VS_code, c("AAA", "BBB"))
})

test_that("eqvs_display(return_df = TRUE) works with a custom value set", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN",
             description = "doi:test", saveOption = 1)
  )

  # return_df = TRUE prints nothing, so no output capture is needed.
  expect_silent(out <- eqvs_display(version = "3L", return_df = TRUE))

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Type", colnames(pkgenv$country_codes[["3L"]])) %in%
                    colnames(out)))
  expect_true("FAN" %in% out$VS_code)
  expect_identical(out$Type[out$VS_code == "FAN"], "User-defined")
  # No column was dropped or padded to make the two tables fit together.
  expect_false(anyNA(out$VS_code))
})

test_that("a custom value set can be used for scoring", {
  local_eq_env()

  vs <- dummy_vs("3L", "FAN")
  suppressMessages(
    eqvs_add(vs, version = "3L", country = "Fantasia", countryCode = "FA",
             VSCode = "FAN", saveOption = 1)
  )

  expect_equal(unname(eq5d3l(11111, country = "FAN")),
               vs[vs$state == 11111, "FAN"])
  # Built-in value sets are unaffected.
  expect_equal(unname(eq5d3l(11111, country = "GB")), 1)
})
