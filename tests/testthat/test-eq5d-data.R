# The data-handling functions the Shiny app and the script it generates share.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")

base_mapping <- function(...) {
  m <- list(eq5d_version = "3L", names_eq5d = DIMS, name_fu = "fu",
            name_id = "id", name_vas = "vas")
  utils::modifyList(m, list(...))
}

# ── eq5d_read_data ────────────────────────────────────────────────────────────

test_that("a CSV round trip keeps the data and the column names", {
  f <- withr::local_tempfile(fileext = ".csv")
  d <- head(example_data, 20)
  utils::write.csv(d, f, row.names = FALSE)

  got <- eq5d_read_data(f)
  expect_s3_class(got, "data.frame")
  expect_equal(nrow(got), 20L)
  expect_identical(names(got), names(d))
  expect_identical(got$mo, d$mo)
  # Characters stay characters.
  expect_type(got$time, "character")
})

test_that("a semicolon-separated file is read when told so", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv2(head(example_data, 10), f, row.names = FALSE)

  expect_equal(nrow(eq5d_read_data(f, sep = ";", dec = ",")), 10L)
  # Read with the wrong separator it is one column, not ten rows of data.
  expect_lt(ncol(eq5d_read_data(f)), ncol(example_data))
})

test_that("names with spaces and punctuation survive", {
  f <- withr::local_tempfile(fileext = ".csv")
  d <- data.frame(a = 1:2, b = 3:4)
  names(d) <- c("my column", "p (%)")
  utils::write.csv(d, f, row.names = FALSE)
  expect_identical(names(eq5d_read_data(f)), c("my column", "p (%)"))
})

test_that("RDS is read, and the type can be given explicitly", {
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(head(example_data, 5), f)
  expect_equal(nrow(eq5d_read_data(f)), 5L)

  g <- withr::local_tempfile(fileext = ".dat")
  utils::write.csv(head(example_data, 5), g, row.names = FALSE)
  expect_equal(nrow(eq5d_read_data(g, type = "csv")), 5L)
})

test_that("an unknown type is refused by name", {
  expect_error(eq5d_read_data("x.docx"), "Unsupported file type: docx")
  expect_error(eq5d_read_data(""), "must be the path to a file")
})

# ── eq5d_apply_mapping ────────────────────────────────────────────────────────

test_that("columns are renamed and coerced", {
  d <- data.frame(MOB = "1", SELFCARE = "2", USUAL = "1", PAIN = "3", ANX = "1",
                  visit = "Baseline", eqvas = "70", stringsAsFactors = FALSE)
  got <- eq5d_apply_mapping(d, list(
    names_eq5d = c("MOB", "SELFCARE", "USUAL", "PAIN", "ANX"),
    name_fu = "visit", name_vas = "eqvas"))

  expect_true(all(c(DIMS, "fu", "vas") %in% names(got)))
  expect_type(got$mo, "integer")
  expect_type(got$vas, "double")
  expect_equal(got$vas, 70)
})

test_that("columns not in the mapping are left alone", {
  d <- example_data
  got <- eq5d_apply_mapping(d, list(names_eq5d = DIMS, name_fu = "time"))
  expect_true(all(c("procedure", "year", "ageband", "gender") %in% names(got)))
  expect_identical(got$procedure, d$procedure)
  expect_false("time" %in% names(got))
  expect_true("fu" %in% names(got))
})

test_that("a NULL or absent column is skipped, not an error", {
  d <- example_data
  expect_silent(eq5d_apply_mapping(d, list(names_eq5d = DIMS,
                                           name_fu = NULL, name_id = "id")))
  expect_silent(eq5d_apply_mapping(d, list(names_eq5d = DIMS,
                                           name_groupvar = "not_a_column")))
  expect_error(eq5d_apply_mapping(1:5, list()), "must be a data frame")
})

# ── eq5d_validate ─────────────────────────────────────────────────────────────

test_that("the checks are returned as plain text", {
  d <- eq5d_apply_mapping(example_data, base_mapping(name_fu = "time"))
  v <- eq5d_validate(d, base_mapping(), quiet = TRUE)

  expect_s3_class(v, "data.frame")
  expect_identical(names(v), c("type", "message"))
  expect_true(all(v$type %in% c("ok", "warning", "error")))
  # No markup: the app adds its own.
  expect_false(any(grepl("<", v$message, fixed = TRUE)))
  expect_match(v$message[1], "^10,000 rows loaded\\.$")
})

test_that("a missing dimension column is an error, and can stop", {
  d <- eq5d_apply_mapping(example_data, base_mapping(name_fu = "time"))
  d$mo <- NULL

  v <- eq5d_validate(d, base_mapping(), quiet = TRUE)
  expect_true("error" %in% v$type)
  expect_match(v$message[v$type == "error"], "EQ-5D columns not found")

  expect_error(eq5d_validate(d, base_mapping(), stop_on_error = TRUE),
               "EQ-5D columns not found")
  # Without stop_on_error it warns and carries on, reporting the other
  # findings as it goes.
  w <- testthat::capture_warnings(eq5d_validate(d, base_mapping()))
  expect_true(any(grepl("EQ-5D columns not found", w)))
  expect_gte(length(w), 1L)
})

test_that("out-of-range levels and missing values are reported", {
  d <- eq5d_apply_mapping(example_data, base_mapping(name_fu = "time"))
  v <- eq5d_validate(d, base_mapping(), quiet = TRUE)
  # example_data codes missing dimensions as 9.
  expect_true(any(grepl("outside the expected range", v$message)))

  # A clean 3L frame passes both.
  clean <- data.frame(mo = 1L, sc = 1L, ua = 2L, pd = 1L, ad = 1L)
  v2 <- eq5d_validate(clean, list(eq5d_version = "3L", names_eq5d = DIMS),
                      quiet = TRUE)
  expect_true(any(grepl("within expected range", v2$message)))
  expect_true(any(grepl("No missing EQ-5D values", v2$message)))
  expect_false("error" %in% v2$type)
})

test_that("5L levels are judged against five, not three", {
  d <- data.frame(mo = 5L, sc = 4L, ua = 1L, pd = 1L, ad = 1L)
  v3 <- eq5d_validate(d, list(eq5d_version = "3L", names_eq5d = DIMS), quiet = TRUE)
  v5 <- eq5d_validate(d, list(eq5d_version = "5L", names_eq5d = DIMS), quiet = TRUE)
  expect_true(any(grepl("not a level the instrument allows", v3$message)))
  expect_false(any(grepl("not a level the instrument allows", v5$message)))
})

test_that("repeated IDs are read against the timepoint column", {
  d <- eq5d_apply_mapping(example_data, base_mapping(name_fu = "time"))
  v <- eq5d_validate(d, base_mapping(), quiet = TRUE)
  expect_true(any(grepl("expected for longitudinal data", v$message)))

  # The same IDs with no timepoint mapped is a warning.
  v2 <- eq5d_validate(d, base_mapping(name_fu = NULL), quiet = TRUE)
  expect_true(any(grepl("repeated patient IDs", v2$message)))
})

test_that("timepoint values outside the stated order are reported", {
  d <- eq5d_apply_mapping(example_data, base_mapping(name_fu = "time"))
  v <- eq5d_validate(d, base_mapping(levels_fu = "Pre-op"), quiet = TRUE)
  expect_true(any(grepl("not in the timepoint order given", v$message)))
  expect_true(any(grepl("Post-op", v$message)))
})

test_that("the EQ VAS range is checked", {
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L, vas = c(50, 120))
  v <- eq5d_validate(d, list(eq5d_version = "3L", names_eq5d = DIMS,
                             name_vas = "vas"), quiet = TRUE)
  expect_true(any(grepl("outside the expected range \\(0", v$message)))
})

# ── eq5d_age_band_midpoint ────────────────────────────────────────────────────

test_that("a band's midpoint is taken in completed years", {
  got <- eq5d_age_band_midpoint(c("20 to 29", "30 to 39", "60 to 69",
                                  "80 to 89", NA))
  # "30 to 39" covers [30, 40), so 35 - not 34.5.
  expect_equal(as.numeric(got), c(25, 35, 65, 85, NA))
  expect_true(attr(got, "banded"))
  expect_equal(attr(got, "straddles"), c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("other band spellings are understood", {
  # "65+" has no upper bound, so no midpoint is invented for it: the lower
  # bound is returned, which is honest and lands in the same DSU category as
  # any other age in the band. It used to return 70, i.e. 65 + 9 halved.
  expect_equal(as.numeric(eq5d_age_band_midpoint(c("30-39", "65+"))), c(35, 65))
  # And the value mapped is the same either way, which is the point.
  expect_equal(q(eqxwr_UK(11111, age = 65, male = 1)),
               q(eqxwr_UK(11111, age = 70, male = 1)))
})

test_that("exact ages are returned untouched", {
  got <- eq5d_age_band_midpoint(c(34, 58, 72))
  expect_equal(as.numeric(got), c(34, 58, 72))
  expect_false(attr(got, "banded"))
  expect_false(any(attr(got, "straddles")))
  # A character column of numbers is numbers, not bands.
  expect_false(attr(eq5d_age_band_midpoint(c("34", "58")), "banded"))
})

test_that("the midpoints map, and land in the upper band when straddling", {
  d <- head(example_data[!is.na(example_data$ageband), ], 20)
  age <- eq5d_age_band_midpoint(d$ageband)
  got <- q(eqxwr_UK(d[, DIMS], age = as.numeric(age),
                    male = as.integer(d$gender == "Male")))
  expect_length(got, 20L)
  expect_true(any(!is.na(got)))
})

# ── eq5d_format_table ─────────────────────────────────────────────────────────

test_that("proportions become percentages and counts lose their decimals", {
  top <- q(eq5d_profile_top_states(example_data, names_eq5d = DIMS,
                                   eq5d_version = "3L", n = 3))
  got <- eq5d_format_table(top)

  expect_equal(got$Percentage[1], "22.5%")
  expect_equal(got$`Cumulative percentage`[2], "36.5%")
  expect_equal(got$Frequency[1], "2,142")
  expect_true(all(vapply(got, is.character, logical(1L))))
  # The table passed in is untouched.
  expect_type(top$Percentage, "double")
})

test_that("headings are tidied unless asked otherwise", {
  tbl <- q(eq5d_profile_level_summary(example_data, names_eq5d = DIMS,
                                      eq5d_version = "3L"))
  expect_true("mo All n" %in% names(eq5d_format_table(tbl)))
  expect_true("n_All_mo" %in% names(eq5d_format_table(tbl, tidy_names = FALSE)))
})

test_that("the number of places can be set", {
  d <- data.frame(freq_a_mo = 0.23456, value = 1.23456)
  expect_equal(eq5d_format_table(d)$`mo a %`, "23.5%")
  expect_equal(eq5d_format_table(d, percent_digits = 2L)$`mo a %`, "23.46%")
  expect_equal(eq5d_format_table(d)$value, "1.23")
  expect_equal(eq5d_format_table(d, digits = 4L)$value, "1.2346")
})

test_that("a _p column counts only when its _n partner is present", {
  # Alone it is just a number; the heading is tidied either way.
  expect_equal(eq5d_format_table(data.frame(group_p = 0.5))$`group p`, "0.50")
  got <- eq5d_format_table(data.frame(group_p = 0.5, group_n = 10))
  expect_equal(got$`group p`, "50.0%")
  expect_equal(got$`group n`, "10")
})

test_that("a table with nothing to format comes back as characters", {
  d <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  expect_identical(eq5d_format_table(d)$name, c("a", "b"))
  expect_error(eq5d_format_table(1:3), "must be a data frame")
})
