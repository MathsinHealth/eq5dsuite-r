# eqvs_drop() had the same unguarded repeat/readline() loop as .fixCountries(),
# plus an unguarded confirmation prompt that silently did nothing in a
# non-interactive session.

test_that("eqvs_drop(ask = FALSE) deletes in a non-interactive session", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 1)
  )
  expect_true("FAN" %in% colnames(pkgenv$uservsets3L))

  result <- suppressMessages(
    eqvs_drop(country = "FAN", version = "3L", saveOption = 1, ask = FALSE)
  )

  expect_true(result)
  expect_false("FAN" %in% colnames(pkgenv$uservsets3L))
  expect_null(pkgenv$user_defined_3L)
})

test_that("eqvs_drop() deletes without prompting when not interactive", {
  pkgenv <- local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", countryCode = "FA",
             VSCode = "FAN", saveOption = 1)
  )

  # ask = TRUE (the default), but interactive() is FALSE under testthat, so no
  # prompt is issued and the deletion goes ahead.
  result <- suppressMessages(
    eqvs_drop(country = "FAN", version = "3L", saveOption = 1)
  )

  expect_true(result)
  expect_false("FAN" %in% colnames(pkgenv$uservsets3L))
})

test_that("eqvs_drop() returns its result invisibly", {
  local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", countryCode = "FA",
             VSCode = "FAN", saveOption = 1)
  )

  expect_invisible(
    suppressMessages(
      eqvs_drop(country = "FAN", version = "3L", saveOption = 1, ask = FALSE))
  )
})

test_that("eqvs_drop() returns FALSE when there is nothing to drop", {
  local_eq_env()

  # No user-defined value sets at all.
  expect_false(
    suppressMessages(eqvs_drop(country = "FAN", version = "3L", ask = FALSE)))

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", countryCode = "FA",
             VSCode = "FAN", saveOption = 1)
  )

  # Present, but not this one.
  expect_false(
    suppressMessages(eqvs_drop(country = "NOPE", version = "3L", ask = FALSE)))
  # ... and the existing set is untouched.
  expect_true("FAN" %in% colnames(getOption("eq.env")$uservsets3L))
})

test_that("an ambiguous country code errors instead of prompting", {
  pkgenv <- local_eq_env()

  suppressMessages({
    eqvs_add(dummy_vs("3L", "XX_A", seed = 1), version = "3L",
             countryCode = "XX", VSCode = "XX_A", saveOption = 1)
    eqvs_add(dummy_vs("3L", "XX_B", seed = 2), version = "3L",
             countryCode = "XX", VSCode = "XX_B", saveOption = 1)
  })

  expect_error(eqvs_drop(country = "XX", version = "3L", ask = FALSE),
               "Multiple value sets are available for country code 'XX'",
               fixed = TRUE)
  expect_error(eqvs_drop(country = "XX", version = "3L", ask = FALSE),
               "XX_A, XX_B", fixed = TRUE)

  # Nothing was removed.
  expect_true(all(c("XX_A", "XX_B") %in% colnames(pkgenv$uservsets3L)))
  expect_identical(nrow(pkgenv$user_defined_3L), 2L)
})

test_that("the ambiguity error carries no call context", {
  local_eq_env()

  suppressMessages({
    eqvs_add(dummy_vs("3L", "XX_A", seed = 1), version = "3L",
             countryCode = "XX", VSCode = "XX_A", saveOption = 1)
    eqvs_add(dummy_vs("3L", "XX_B", seed = 2), version = "3L",
             countryCode = "XX", VSCode = "XX_B", saveOption = 1)
  })

  cnd <- tryCatch(eqvs_drop(country = "XX", version = "3L", ask = FALSE),
                  error = function(e) e)
  expect_s3_class(cnd, "error")
  expect_null(conditionCall(cnd))
})

test_that("a specific value set code drops exactly that value set", {
  pkgenv <- local_eq_env()

  suppressMessages({
    eqvs_add(dummy_vs("3L", "XX_A", seed = 1), version = "3L",
             countryCode = "XX", VSCode = "XX_A", saveOption = 1)
    eqvs_add(dummy_vs("3L", "XX_B", seed = 2), version = "3L",
             countryCode = "XX", VSCode = "XX_B", saveOption = 1)
  })

  expect_true(suppressMessages(
    eqvs_drop(country = "XX_A", version = "3L", saveOption = 1, ask = FALSE)))

  expect_false("XX_A" %in% colnames(pkgenv$uservsets3L))
  expect_true("XX_B" %in% colnames(pkgenv$uservsets3L))
  expect_identical(pkgenv$user_defined_3L$VS_code, "XX_B")

  # The survivor is still usable, and the metadata table is still valid.
  expect_length(.validate_vs_meta(pkgenv$user_defined_3L), 0L)
  expect_false(is.na(eq5d3l(11111, country = "XX_B")))
})

test_that("dropping persists to the cache", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", countryCode = "FA",
             VSCode = "FAN", saveOption = 3, savePath = dir)
  )
  suppressMessages(
    eqvs_drop(country = "FAN", version = "3L", saveOption = 3,
              savePath = dir, ask = FALSE)
  )

  saved <- new.env(parent = emptyenv())
  load(file.path(dir, .cache_basename), envir = saved)
  expect_false("user_defined_3L" %in% ls(saved))
  expect_identical(ncol(get("uservsets3L", envir = saved)), 1L)
})
