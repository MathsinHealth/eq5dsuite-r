# eqvs_display() either prints the table (return_df = FALSE) or returns it
# (return_df = TRUE), never both: assigning the data frame used to print the
# whole table as a side effect, which is unwanted in scripts and reports.

test_that("return_df = FALSE prints the table and returns NULL invisibly", {
  local_eq_env()

  expect_output(suppressMessages(eqvs_display(version = "3L")), "United Kingdom")

  result <- withVisible(suppressMessages(
    invisible(capture.output(eqvs_display(version = "3L")))
  ))
  expect_false(result$visible)

  invisible(capture.output(
    out <- suppressMessages(eqvs_display(version = "3L"))
  ))
  expect_null(out)
})

test_that("return_df = FALSE still labels the sections", {
  local_eq_env()

  msgs <- printed_messages(eqvs_display(version = "3L"))

  expect_true(any(grepl("Available national value sets", msgs, fixed = TRUE)))
  expect_true(any(grepl("No user-defined value sets available", msgs,
                        fixed = TRUE)))
})

test_that("return_df = TRUE prints nothing", {
  local_eq_env()

  expect_silent(eqvs_display(version = "3L", return_df = TRUE))
})

test_that("return_df = TRUE returns a data frame with the expected columns", {
  pkgenv <- local_eq_env()

  out <- eqvs_display(version = "3L", return_df = TRUE)

  expect_s3_class(out, "data.frame")
  expect_identical(colnames(out),
                   c("Type", colnames(pkgenv$country_codes[["3L"]])))
  expect_identical(nrow(out), nrow(pkgenv$country_codes[["3L"]]))
  expect_true(all(out$Type == "Value set"))
  expect_true("citation" %in% colnames(out))
})

test_that("return_df = TRUE returns the data frame visibly", {
  local_eq_env()

  # So that typing the call at the console still shows the table, while
  # `vs <- eqvs_display(...)` stays quiet.
  expect_true(withVisible(eqvs_display(version = "3L", return_df = TRUE))$visible)
})

test_that("show_citation is ignored when return_df = TRUE", {
  local_eq_env()

  expect_silent(
    eqvs_display(version = "3L", return_df = TRUE, show_citation = TRUE))

  # The returned frame carries the citations either way, so the two calls agree.
  expect_identical(
    eqvs_display(version = "3L", return_df = TRUE, show_citation = TRUE),
    eqvs_display(version = "3L", return_df = TRUE)
  )
})

test_that("show_citation still prints citations when return_df = FALSE", {
  local_eq_env()

  msgs <- printed_messages(eqvs_display(version = "3L", show_citation = TRUE))

  expect_true(any(grepl("Citations:", msgs, fixed = TRUE)))
  # One line per value set that has a citation.
  expect_true(any(grepl("[GB] Dolan P.", msgs, fixed = TRUE)))
})

test_that("the behaviour is the same for every instrument version", {
  pkgenv <- local_eq_env()

  for (version in c("3L", "5L", "Y3L")) {
    expect_silent(eqvs_display(version = version, return_df = TRUE))

    out <- eqvs_display(version = version, return_df = TRUE)
    expect_s3_class(out, "data.frame")
    expect_identical(nrow(out), nrow(pkgenv$country_codes[[version]]),
                     info = version)
    expect_true(withVisible(
      eqvs_display(version = version, return_df = TRUE))$visible, info = version)

    expect_output(suppressMessages(eqvs_display(version = version)),
                  "Version", info = version)
  }
})

test_that("a user-defined value set does not reintroduce printing", {
  local_eq_env()

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 1)
  )

  expect_silent(eqvs_display(version = "3L", return_df = TRUE))
  expect_silent(
    eqvs_display(version = "3L", return_df = TRUE, show_citation = TRUE))

  out <- eqvs_display(version = "3L", return_df = TRUE)
  expect_true("FAN" %in% out$VS_code)
  expect_identical(out$Type[out$VS_code == "FAN"], "User-defined")
  expect_true(all(out$Type[out$VS_code != "FAN"] == "Value set"))

  # ... and the printing path still shows both sections.
  msgs <- printed_messages(eqvs_display(version = "3L"))
  expect_true(any(grepl("User-defined value sets", msgs, fixed = TRUE)))
})

test_that("lower-case versions are accepted", {
  local_eq_env()

  expect_silent(eqvs_display(version = "3l", return_df = TRUE))
  expect_identical(eqvs_display(version = "3l", return_df = TRUE),
                   eqvs_display(version = "3L", return_df = TRUE))
})

test_that("an unknown version is an informative error", {
  local_eq_env()

  # Previously this printed a bare "NULL", and with return_df = TRUE returned a
  # 1x1 matrix holding nothing.
  expect_error(eqvs_display(version = "XX"),
               "Unknown EQ-5D version 'XX'", fixed = TRUE)
  expect_error(eqvs_display(version = "XX", return_df = TRUE),
               "Expected one of: 3L, 5L, Y3L", fixed = TRUE)
  expect_error(eqvs_display(version = c("3L", "5L")), "single string")
  expect_error(eqvs_display(version = 5), "single string")
})
