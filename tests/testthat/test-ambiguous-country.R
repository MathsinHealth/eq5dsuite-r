# A country code matching several value sets must raise an informative error
# in every kind of session, rather than prompting with readline(). The old
# repeat/readline() loop never terminated in a non-interactive session, which
# hung dependent packages.

test_that("an ambiguous country code errors and lists every matching code", {
  local_eq_env()

  expect_error(.fixCountries("DE", "3L"),
               "Multiple value sets are available for country code 'DE'",
               fixed = TRUE)
  expect_error(.fixCountries("DE", "3L"), "DE_TTO, DE_VAS", fixed = TRUE)
  expect_error(.fixCountries("DE", "3L"),
               "Please specify one of these value set codes", fixed = TRUE)
})

test_that("the error names the instrument version to look up", {
  local_eq_env()

  expect_error(.fixCountries("DE", "3L"),
               'eqvs_display(version = "3L")', fixed = TRUE)
  expect_error(.fixCountries("SE", "5L"),
               'eqvs_display(version = "5L")', fixed = TRUE)
})

test_that("every ambiguous built-in country code errors", {
  local_eq_env()

  ambiguous <- list(
    list(version = "3L", code = "AR", sets = c("AR_TTO", "AR_VAS")),
    list(version = "3L", code = "DE", sets = c("DE_TTO", "DE_VAS")),
    list(version = "3L", code = "NL", sets = c("NL_2006", "NL_2026")),
    list(version = "3L", code = "SI", sets = c("SI_TTO", "SI_VAS")),
    list(version = "5L", code = "SE", sets = c("SE_2020", "SE_2022"))
  )

  for (case in ambiguous) {
    expect_error(.fixCountries(case$code, case$version),
                 paste(case$sets, collapse = ", "), fixed = TRUE,
                 info = paste(case$version, case$code))
  }
})

test_that("matching is case-insensitive for the ambiguity check too", {
  local_eq_env()

  expect_error(.fixCountries("de", "3L"), "DE_TTO, DE_VAS", fixed = TRUE)
})

test_that("the error carries no call context", {
  local_eq_env()

  # call. = FALSE, so the condition has no call and printing it gives a bare
  # "Error: ..." rather than "Error in FUN(X[[i]], ...) : ...".
  cnd <- tryCatch(.fixCountries("DE", "3L"), error = function(e) e)
  expect_s3_class(cnd, "error")
  expect_null(conditionCall(cnd))
})

test_that("a specific value set code still resolves and scores", {
  local_eq_env()

  expect_identical(unname(.fixCountries("DE_TTO", "3L")), "DE_TTO")
  expect_identical(unname(.fixCountries("DE_VAS", "3L")), "DE_VAS")
  expect_identical(unname(.fixCountries("de_tto", "3L")), "DE_TTO")
  expect_identical(unname(.fixCountries("SE_2022", "5L")), "SE_2022")

  # The two German value sets give different values, so the choice matters.
  tto <- unname(eq5d3l(12321, country = "DE_TTO"))
  vas <- unname(eq5d3l(12321, country = "DE_VAS"))
  expect_false(is.na(tto))
  expect_false(is.na(vas))
  expect_false(isTRUE(all.equal(tto, vas)))
})

test_that("a country code with exactly one value set still works", {
  local_eq_env()

  expect_identical(unname(.fixCountries("GB", "3L")), "GB")
  expect_identical(unname(.fixCountries("US", "5L")), "US")
  expect_equal(unname(eq5d3l(11111, country = "GB")), 1)
  expect_false(is.na(eq5d3l(12321, country = "GB")))
})

test_that("the error reaches the user through the value functions", {
  local_eq_env()

  msg <- "Multiple value sets are available for country code 'DE'"

  expect_error(eq5d3l(11111, country = "DE"), msg, fixed = TRUE)
  expect_error(eq5d(11111, country = "DE", version = "3L"), msg, fixed = TRUE)
  expect_error(eq5d(11111, country = "DE", version = "XW"), msg, fixed = TRUE)
  expect_error(eqxw(11111, country = "DE"), msg, fixed = TRUE)

  expect_error(eq5d5l(11111, country = "SE"),
               "SE_2020, SE_2022", fixed = TRUE)
  expect_error(eqxwr(11111, country = "SE"),
               "SE_2020, SE_2022", fixed = TRUE)
})

test_that("the error is not replaced by the generic 'no valid countries' one", {
  local_eq_env()

  # .fixCountries() fires before the NA / length-0 handling in eq5d(), so the
  # user sees the specific message rather than "No valid countries listed."
  expect_error(eq5d3l(11111, country = "DE"), "Multiple value sets",
               fixed = TRUE)
  expect_error(eq5d3l(11111, country = "DE"), "^((?!No valid countries).)*$",
               perl = TRUE)
})

# The analysis functions take EQ-5D values from a column rather than choosing a
# value set -- with one exception. The Health Profile Grid ranks every state the
# instrument allows, including those absent from the data, so it has to value
# them itself and does take a value set. This test pins both halves of that,
# because it is what decides where an ambiguous country code can arise.
test_that("only the Health Profile Grid takes a value set", {
  analysis <- grep("^eq5d_(profile|utility|vas)_",
                   getNamespaceExports("eq5dsuite"), value = TRUE)
  expect_gt(length(analysis), 25L)

  takes_country <- Filter(
    function(fn) "country" %in% names(formals(get(fn))), analysis)
  expect_identical(takes_country, "eq5d_profile_health_profile_grid")

  # Every other function that analyses values takes a column.
  value_fns <- setdiff(grep("utility", analysis, value = TRUE), takes_country)
  expect_gt(length(value_fns), 10L)
  for (fn in value_fns)
    expect_true("name_utility" %in% names(formals(get(fn))), info = fn)
  expect_false("name_utility" %in%
                 names(formals(eq5d_profile_health_profile_grid)))
})

test_that("the Health Profile Grid rejects an ambiguous country code", {
  local_eq_env()
  d <- data.frame(id = c(1, 1), time = c("Pre-op", "Post-op"),
                  mo = c(2, 1), sc = c(2, 1), ua = c(2, 1),
                  pd = c(2, 1), ad = c(2, 1))
  # DE has a TTO and a VAS value set, so the code alone is not enough. The
  # grid values the states through eq5d(), so it inherits that refusal.
  expect_error(
    suppressWarnings(suppressMessages(eq5d_profile_health_profile_grid(
      d, names_eq5d = c("mo", "sc", "ua", "pd", "ad"), name_fu = "time",
      levels_fu = c("Pre-op", "Post-op"), name_id = "id",
      eq5d_version = "3L", country = "DE"))),
    "Multiple value sets are available")
})

test_that("an ambiguous code in a multi-country request errors", {
  local_eq_env()

  expect_error(eq5d3l(11111, country = c("GB", "DE")),
               "DE_TTO, DE_VAS", fixed = TRUE)
  # Unambiguous combinations are unaffected.
  out <- eq5d3l(11111, country = c("GB", "DE_TTO"))
  expect_identical(colnames(out), c("GB", "DE_TTO"))
})

test_that("a user-defined set sharing a country code errors on that code", {
  local_eq_env()

  # Two user-defined sets under one country code: neither is named by it.
  suppressMessages({
    eqvs_add(dummy_vs("3L", "XX_A", seed = 1), version = "3L",
             countryCode = "XX", VSCode = "XX_A", saveOption = 1)
    eqvs_add(dummy_vs("3L", "XX_B", seed = 2), version = "3L",
             countryCode = "XX", VSCode = "XX_B", saveOption = 1)
  })

  expect_error(.fixCountries("XX", "3L"), "XX_A, XX_B", fixed = TRUE)
  expect_error(eq5d3l(11111, country = "XX"),
               "Multiple value sets are available for country code 'XX'",
               fixed = TRUE)

  # Each remains reachable by its own value set code.
  expect_identical(unname(.fixCountries("XX_A", "3L")), "XX_A")
  expect_identical(unname(.fixCountries("XX_B", "3L")), "XX_B")
})

test_that("an exact value set code wins over a country code match", {
  local_eq_env()

  # A user-defined set reusing the built-in "GB" country code must not make the
  # built-in GB value set unreachable: "GB" is itself a value set code, so it
  # still resolves to the built-in set.
  suppressMessages(
    eqvs_add(dummy_vs("3L", "GB_MINE"), version = "3L", country = "Mine",
             countryCode = "GB", VSCode = "GB_MINE", saveOption = 1)
  )

  expect_identical(unname(.fixCountries("GB", "3L")), "GB")
  expect_equal(unname(eq5d3l(11111, country = "GB")), 1)
  expect_identical(unname(.fixCountries("GB_MINE", "3L")), "GB_MINE")
})
