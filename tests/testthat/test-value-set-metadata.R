# Metadata corrections applied to the registry in R/sysdata.rda: value set
# names, alphabetical ordering, and the UK -> GB code change.

# Reference values taken from the UK column of the value set tables *before*
# the rename, at full stored precision (the EQ-5D-3L set is stored at float
# precision, so the digits below are not typos). GB must reproduce them
# exactly: this change was metadata only.
uk_reference <- list(
  "3L" = c("11111" = 1,
           "12321" = 0.32900005599999999,
           "22222" = 0.516000032,
           "33333" = -0.59399998200000004),
  "5L" = c("11111" = 1,
           "12321" = 0.82000000000000006,
           "32423" = 0.53800000000000003,
           "55555" = -0.56699999999999995)
)

# ---------------------------------------------------------------------------
# Value set names
# ---------------------------------------------------------------------------

test_that("the Dutch EQ-5D-3L value sets use the bracket name format", {
  pkgenv <- local_eq_env()
  cc <- pkgenv$country_codes[["3L"]]

  expect_identical(cc$Name_short[cc$VS_code == "NL_2006"], "Netherlands (2006)")
  expect_identical(cc$Name_short[cc$VS_code == "NL_2026"], "Netherlands (2026)")
  # Both columns hold the same informal form, so both were updated.
  expect_identical(cc$Name[cc$VS_code == "NL_2006"], "Netherlands (2006)")
  expect_identical(cc$Name[cc$VS_code == "NL_2026"], "Netherlands (2026)")

  # The underscore form is gone everywhere.
  expect_false(any(grepl("Netherlands_", c(cc$Name, cc$Name_short))))
})

test_that("the value set codes NL_2006 and NL_2026 are unchanged", {
  local_eq_env()

  expect_identical(unname(.fixCountries("NL_2006", "3L")), "NL_2006")
  expect_identical(unname(.fixCountries("NL_2026", "3L")), "NL_2026")
  expect_false(is.na(eq5d3l(12321, country = "NL_2006")))
  expect_false(is.na(eq5d3l(12321, country = "NL_2026")))
})

test_that("the Swedish EQ-5D-5L value sets are distinguishable", {
  pkgenv <- local_eq_env()
  cc5 <- pkgenv$country_codes[["5L"]]

  expect_identical(cc5$Name_short[cc5$VS_code == "SE_2020"], "Sweden (2020)")
  expect_identical(cc5$Name_short[cc5$VS_code == "SE_2022"], "Sweden (2022)")
  expect_identical(cc5$Name[cc5$VS_code == "SE_2020"], "Sweden (2020)")

  # The EQ-5D-3L has a single Swedish value set, so it keeps the plain name.
  cc3 <- pkgenv$country_codes[["3L"]]
  expect_identical(cc3$Name_short[cc3$VS_code == "SE"], "Sweden")
})

test_that("informal country names were replaced", {
  pkgenv <- local_eq_env()

  for (version in c("3L", "5L")) {
    cc <- pkgenv$country_codes[[version]]
    expect_identical(cc$Name_short[cc$Country_code == "US"], "United States",
                     info = version)
    expect_identical(cc$Name_short[cc$Country_code == "KR"], "South Korea",
                     info = version)
    expect_false("USA" %in% cc$Name_short, info = version)
    expect_false("Korea" %in% cc$Name_short, info = version)
  }
})

test_that("formal names in the Name column are left alone", {
  pkgenv <- local_eq_env()
  cc <- pkgenv$country_codes[["5L"]]

  expect_identical(cc$Name[cc$Country_code == "US"], "United States of America")
  expect_identical(cc$Name[cc$Country_code == "KR"], "Republic of Korea")
})

# ---------------------------------------------------------------------------
# Ordering
# ---------------------------------------------------------------------------

test_that("each value set list is sorted alphabetically by country name", {
  pkgenv <- local_eq_env()

  for (version in c("3L", "5L", "Y3L")) {
    names_short <- pkgenv$country_codes[[version]]$Name_short
    expect_identical(names_short, sort(names_short, method = "radix"),
                     info = version)
  }
})

test_that("the two Dutch value sets sit next to each other", {
  pkgenv <- local_eq_env()
  names_short <- pkgenv$country_codes[["3L"]]$Name_short

  i <- match("Netherlands (2006)", names_short)
  expect_false(is.na(i))
  expect_identical(names_short[i + 1L], "Netherlands (2026)")
  # ... and no longer at the very end of the list.
  expect_false(names_short[length(names_short)] == "Netherlands (2026)")
})

test_that("eqvs_display() returns the list in sorted order", {
  local_eq_env()

  expect_silent(out <- eqvs_display(version = "3L", return_df = TRUE))
  expect_identical(out$Name_short, sort(out$Name_short, method = "radix"))
})

# ---------------------------------------------------------------------------
# UK -> GB
# ---------------------------------------------------------------------------

test_that("GB selects the UK value set in each instrument version", {
  pkgenv <- local_eq_env()

  expect_identical(unname(.fixCountries("GB", "3L")), "GB")
  expect_identical(unname(.fixCountries("GB", "5L")), "GB")

  for (version in c("3L", "5L")) {
    cc <- pkgenv$country_codes[[version]]
    expect_identical(cc$Name_short[cc$VS_code == "GB"], "United Kingdom",
                     info = version)
    expect_identical(cc$Country_code[cc$VS_code == "GB"], "GB", info = version)
  }

  # The EQ-5D-Y-3L has no UK value set, before or after.
  expect_false("GB" %in% pkgenv$country_codes[["Y3L"]]$VS_code)
})

test_that("GB matches exactly one value set per instrument version", {
  pkgenv <- local_eq_env()

  for (version in c("3L", "5L")) {
    cc <- pkgenv$country_codes[[version]]
    n <- sum(cc$Country_code == "GB" | cc$VS_code == "GB")
    expect_identical(n, 1L, info = version)
  }
  # So it never triggers the "multiple value sets" error.
  expect_no_error(eq5d3l(11111, country = "GB"))
  expect_no_error(eq5d5l(11111, country = "GB"))
})

test_that("the code UK no longer appears in the metadata", {
  pkgenv <- local_eq_env()

  for (version in c("3L", "5L", "Y3L")) {
    cc <- pkgenv$country_codes[[version]]
    expect_false("UK" %in% cc$Country_code, info = version)
    expect_false("UK" %in% cc$VS_code, info = version)
  }
})

test_that("GB gives the same values the UK value set gave before", {
  local_eq_env()

  for (version in names(uk_reference)) {
    f <- if (version == "3L") eq5d3l else eq5d5l
    reference <- uk_reference[[version]]
    got <- unname(f(as.integer(names(reference)), country = "GB"))
    # Exactly, not approximately: nothing about the numbers changed.
    expect_identical(got, unname(reference), info = version)
  }
})

# ---------------------------------------------------------------------------
# The deprecated UK alias
# ---------------------------------------------------------------------------

test_that("UK still works and gives the same values as GB", {
  local_eq_env()
  withr::local_options(rlib_message_verbosity = "quiet")

  expect_identical(unname(.fixCountries("UK", "3L")), "GB")
  expect_identical(unname(.fixCountries("UK", "5L")), "GB")
  expect_identical(unname(.fixCountries("uk", "3L")), "GB")

  for (version in names(uk_reference)) {
    f <- if (version == "3L") eq5d3l else eq5d5l
    states <- as.integer(names(uk_reference[[version]]))
    expect_equal(unname(f(states, country = "UK")),
                 unname(f(states, country = "GB")), info = version)
  }
})

test_that("UK works through the user-facing functions", {
  local_eq_env()
  withr::local_options(rlib_message_verbosity = "quiet")

  expect_equal(unname(eq5d(11111, country = "UK", version = "3L")), 1)
  expect_equal(unname(eqxw(11111, country = "UK")), 1)
  expect_false(is.na(eqxwr(12321, country = "UK")))
})

test_that("using UK explains that GB should be used instead", {
  local_eq_env()
  rlang::reset_message_verbosity("eq5dsuite_uk_to_gb")

  expect_message(.fixCountries("UK", "3L"), "deprecated", fixed = TRUE)

  rlang::reset_message_verbosity("eq5dsuite_uk_to_gb")
  expect_message(.fixCountries("UK", "3L"), 'use "GB" instead', fixed = TRUE)
})

test_that("the deprecation message is shown at most once per session", {
  local_eq_env()
  rlang::reset_message_verbosity("eq5dsuite_uk_to_gb")

  # First use warns the user ...
  expect_message(eq5d3l(11111, country = "UK"), "deprecated", fixed = TRUE)
  # ... and repeated use in the same session stays quiet, so scripts that call
  # these functions in a loop are not flooded.
  expect_no_message(eq5d3l(11111, country = "UK"))
  expect_no_message(eq5d5l(11111, country = "UK"))
  expect_no_message(.fixCountries("UK", "3L"))
})

test_that("GB itself never triggers the deprecation message", {
  local_eq_env()
  rlang::reset_message_verbosity("eq5dsuite_uk_to_gb")

  expect_no_message(eq5d3l(11111, country = "GB"))
  expect_no_message(.fixCountries("GB", "5L"))
})

test_that("a user-defined value set coded UK is not redirected to GB", {
  local_eq_env()
  rlang::reset_message_verbosity("eq5dsuite_uk_to_gb")

  # Someone who already has their own "UK" set keeps it: the alias only steps
  # in when "UK" matches nothing.
  suppressMessages(
    eqvs_add(dummy_vs("3L", "UK", seed = 5), version = "3L",
             country = "My UK set", countryCode = "UK", VSCode = "UK",
             saveOption = 1)
  )

  expect_identical(unname(.fixCountries("UK", "3L")), "UK")
  expect_no_message(.fixCountries("UK", "3L"))
  # ... and the built-in set is still reachable as GB.
  expect_identical(unname(.fixCountries("GB", "3L")), "GB")
  expect_equal(unname(eq5d3l(11111, country = "GB")), 1)
})

test_that("an unknown code is still NA and gives no deprecation message", {
  local_eq_env()
  rlang::reset_message_verbosity("eq5dsuite_uk_to_gb")

  expect_true(is.na(.fixCountries("NOWHERE", "3L")))
  expect_no_message(.fixCountries("NOWHERE", "3L"))
})

# ---------------------------------------------------------------------------
# Citations
#
# The citations are built from Crossref metadata, and six carried an R list
# printed into the volume/issue field:
#
#   "Value in Health Regional Issues. 2025;45(list(list(2025, 1))):101045"
#
# They are user-facing: eqvs_display(show_citation = TRUE) prints them, and
# they are the references users are asked to reproduce in publications.
# Corrected in the registry in R/sysdata.rda.
# ---------------------------------------------------------------------------

test_that("no citation carries an import artefact", {
  pkgenv <- local_eq_env()
  cc <- do.call(rbind, pkgenv$country_codes)

  expect_false(any(grepl("list(", cc$citation, fixed = TRUE)))
  expect_false(any(grepl("\\bNA\\b", cc$citation)))
  expect_false(anyNA(cc$citation))
  expect_true(all(nzchar(trimws(cc$citation))))
})

test_that("citations are free of formatting damage", {
  pkgenv <- local_eq_env()
  cc <- do.call(rbind, pkgenv$country_codes)

  # HTML entities from the Crossref journal title.
  expect_false(any(grepl("&amp;", cc$citation, fixed = TRUE)))
  # An empty author slot, printed as a stray comma.
  expect_false(any(grepl(",\\s*,", cc$citation)))
  # Stray whitespace.
  expect_identical(cc$citation, trimws(cc$citation))
  expect_false(any(grepl("  ", cc$citation, fixed = TRUE)))
  # A DOI does not end in a full stop.
  expect_false(any(grepl("\\.$", cc$doi)))
  # Every citation ends with the DOI it is filed under.
  has_doi <- grepl("^10\\.", cc$doi)
  expect_true(all(mapply(function(a, b) grepl(b, a, fixed = TRUE),
                         cc$citation[has_doi], cc$doi[has_doi])))
})

test_that("the six corrected citations read as intended", {
  pkgenv <- local_eq_env()
  cc <- do.call(rbind, pkgenv$country_codes)
  cit <- function(v, code) cc$citation[cc$Version == v & cc$VS_code == code]

  expect_match(cit("3L", "NL_2026"), "2026;27(6):1351-1357.", fixed = TRUE)
  expect_match(cit("3L", "TT"),      "2016;11:60-67.",        fixed = TRUE)
  expect_match(cit("5L", "ET"),      "2020;22:7-14.",         fixed = TRUE)
  expect_match(cit("5L", "GH"),      "2025;45:101045.",       fixed = TRUE)
  expect_match(cit("5L", "IT"),      "2022;292:114519.",      fixed = TRUE)
  expect_match(cit("5L", "NZ"),      "2020;246:112707.",      fixed = TRUE)

  # Found in the same sweep.
  expect_match(cit("5L", "GB"), "Value in Health. 2026;29(5):858-869.",
               fixed = TRUE)
  expect_match(cit("5L", "IT"), "Social Science & Medicine", fixed = TRUE)
})
