# What lies behind each data check, for the Validation page's "See details".
#
# eq5d_validate() keeps its short messages and attaches, per finding, the
# values found, the records affected and what a percentage is of. The
# details describe the data and change nothing.

DIMS <- c("mo", "sc", "ua", "pd", "ad")
m3 <- function(...) c(list(eq5d_version = "3L", names_eq5d = DIMS), list(...))

details_for <- function(v, pattern) {
  i <- grep(pattern, v$message)
  expect_length(i, 1L)
  attr(v, "details")[[i]]
}

test_that("the findings are still a plain two-column table", {
  v <- eq5d_validate(example_data, m3(), quiet = TRUE)
  expect_identical(names(v), c("type", "message"))
  expect_length(attr(v, "details"), nrow(v))
})

test_that("invalid levels: variables, distinct values with counts, and records", {
  d <- data.frame(id = c("a", "b", "c", "d", "e"),
                  mo = c(1, 9, 9, 4, 2), sc = c(1, 1, 2.5, 1, 1),
                  ua = 1, pd = 1, ad = 1)
  v <- eq5d_validate(d, m3(name_id = "id"), quiet = TRUE)
  x <- details_for(v, "not a level the instrument allows")
  s <- x$summary
  expect_identical(s$Count[s$Variable == "mo" & s$Value == "9"], 2L)
  expect_identical(s$Count[s$Variable == "mo" & s$Value == "4"], 1L)
  expect_identical(s$Reason[s$Variable == "sc"], "fractional")
  expect_identical(x$records$Row, c(2L, 3L, 3L, 4L))
  expect_identical(x$records$ID, c("b", "c", "c", "d"))
  expect_match(x$denominator, "4 values in 3 of 5 rows", fixed = TRUE)
})

test_that("4 or 5 in EQ-5D-3L data asks, in bold, which instrument it is", {
  d <- data.frame(mo = c(1, 5), sc = 1, ua = 1, pd = 1, ad = 1)
  x <- details_for(eq5d_validate(d, m3(), quiet = TRUE), "not a level")
  expect_identical(x$emphasis,
                   "Please check whether your data use EQ-5D-3L or EQ-5D-5L.")
  # Not for other invalid values, and not for EQ-5D-5L data.
  d9 <- data.frame(mo = c(1, 9), sc = 1, ua = 1, pd = 1, ad = 1)
  expect_null(details_for(eq5d_validate(d9, m3(), quiet = TRUE),
                          "not a level")$emphasis)
  d6 <- data.frame(mo = c(1, 6), sc = 1, ua = 1, pd = 1, ad = 1)
  m5 <- list(eq5d_version = "5L", names_eq5d = DIMS)
  expect_null(details_for(eq5d_validate(d6, m5, quiet = TRUE),
                          "not a level")$emphasis)
})

test_that("a 9 is explained as a possible missing code, not assumed to be one", {
  d <- data.frame(mo = c(1, 9), sc = 1, ua = 1, pd = 1, ad = 1)
  x <- details_for(eq5d_validate(d, m3(), quiet = TRUE), "not a level")
  nine <- grep("9", x$notes, value = TRUE)
  expect_length(nine, 1L)
  expect_match(nine, "missing response", fixed = TRUE)
  expect_match(nine, "does not assume", fixed = TRUE)
  # No 9, no note about it.
  d4 <- data.frame(mo = c(1, 4), sc = 1, ua = 1, pd = 1, ad = 1)
  x4 <- details_for(eq5d_validate(d4, m3(), quiet = TRUE), "not a level")
  expect_false(any(grepl("A value of 9", x4$notes, fixed = TRUE)))
})

test_that("missing values: which dimensions, which records, and why", {
  d <- data.frame(id = 1:6,
                  mo = c("1", NA, "", "n/a", "9", "1"),
                  sc = c("1", "1", "1", "1", "1", NA),
                  ua = "1", pd = "1", ad = "1", stringsAsFactors = FALSE)
  v <- eq5d_validate(d, m3(name_id = "id"), quiet = TRUE)
  expect_true(any(grepl("^5 of 6 rows \\(83%\\) have missing EQ-5D values\\.$",
                        v$message)))
  x <- details_for(v, "have missing EQ-5D values")
  mo <- x$summary[x$summary$Variable == "mo", ]
  expect_identical(mo$`Missing in the data`, 2L)    # NA and ""
  expect_identical(mo$`Not a number`, 1L)           # "n/a"
  expect_identical(mo$`Invalid level (set to NA)`, 1L)  # 9
  expect_identical(x$summary$`Missing in the data`[x$summary$Variable == "sc"], 1L)
  expect_identical(x$records$Row, 2:6)
  expect_identical(x$records$`Missing in the data`, c("mo", "mo", "", "", "sc"))
  expect_identical(x$records$`Not a number`, c("", "", "mo", "", ""))
  expect_identical(x$records$`Invalid level`, c("", "", "", "mo", ""))
  expect_match(x$denominator, "of all 6 rows", fixed = TRUE)
})

test_that("the records listed are exactly the rows the analyses cannot use", {
  v <- eq5d_validate(example_data, m3(), quiet = TRUE)
  x <- details_for(v, "have missing EQ-5D values")
  cleaned <- suppressWarnings(
    .clean_dim_matrix(example_data[DIMS], max_level = 3L))
  expect_identical(x$records$Row, which(!stats::complete.cases(cleaned)))
  expect_identical(nrow(x$records), 497L)
})

test_that("validating changes nothing", {
  d <- example_data
  before <- d
  eq5d_validate(d, m3(name_id = "id", name_fu = "time", name_vas = "vas"),
                quiet = TRUE)
  expect_identical(d, before)
})

test_that("EQ VAS: values outside 0-100 with counts and records; 999 explained", {
  d <- data.frame(id = 1:7, mo = 1, sc = 1, ua = 1, pd = 1, ad = 1,
                  vas = c("50", "150", "999", "999", "-1", "abc", NA),
                  stringsAsFactors = FALSE)
  v <- eq5d_validate(d, m3(name_id = "id", name_vas = "vas"), quiet = TRUE)
  expect_true(any(grepl(
    "^4 of 5 recorded VAS values \\(80%\\) are outside the expected range",
    v$message)))
  x <- details_for(v, "VAS values")
  expect_identical(x$summary$Count[x$summary$Value == "999"], 2L)
  expect_identical(x$summary$Problem[x$summary$Value == "abc"], "not a number")
  expect_identical(x$records$Row, 2:6)
  expect_true(any(grepl("999", x$notes) & grepl("does not assume", x$notes)))
  expect_match(x$denominator, "5 recorded numeric VAS values", fixed = TRUE)
  expect_match(x$denominator, "1 rows have no VAS value", fixed = TRUE)
})

test_that("duplicates and unlisted timepoints show which records", {
  d <- data.frame(id = c(1, 1, 2, 3), fu = c("pre", "pre", "pre", "x"),
                  mo = 1, sc = 1, ua = 1, pd = 1, ad = 1)
  v <- eq5d_validate(d, m3(name_id = "id", name_fu = "fu", levels_fu = "pre"),
                     quiet = TRUE)
  dup <- details_for(v, "combinations are")
  expect_identical(dup$summary$ID, "1")
  expect_identical(dup$summary$Rows, "1, 2")
  tp <- details_for(v, "not in the timepoint order")
  expect_identical(tp$summary$`Timepoint value`, "x")
  expect_identical(tp$records$Row, 4L)
})

test_that("clean data have no details to show", {
  d <- data.frame(id = 1:2, mo = 1, sc = 2, ua = 3, pd = 1, ad = 1, vas = 50)
  v <- eq5d_validate(d, m3(name_id = "id", name_vas = "vas"), quiet = TRUE)
  expect_true(all(vapply(attr(v, "details"), is.null, NA)))
})

# ── The page ──────────────────────────────────────────────────────────────────

test_that("the Validation page offers See details, collapsed, where there are any", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(
    raw_data = example_data,
    mapping = list(eq5d_version = "3L", names_eq5d = DIMS, name_fu = "time",
                   name_id = "id", name_vas = "vas",
                   levels_fu = c("Pre-op", "Post-op")),
    processed_data = NULL, results = list(), value_cols = character(0),
    steps = list())
  shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    session$flushReact()
    html <- as.character(output$validation_msgs$html)
    # Three warnings with details; the "rows loaded" line has none.
    expect_identical(lengths(regmatches(html, gregexpr("See details", html))), 3L)
    expect_match(html, "<details class=\"finding-details\">", fixed = TRUE)
    expect_false(grepl("<details[^>]*open", html))
    expect_match(html, "Records affected (1,323)", fixed = TRUE)
    expect_match(html, "Percentages are of all 10,000 rows", fixed = TRUE)
    # The records are a paged table.
    recs <- output$det_2_records
    expect_false(is.null(recs))
  })
})

test_that("the bold instrument question reaches the page", {
  skip_unless_app()
  e <- app_env()
  d <- data.frame(mo = c(1, 4), sc = 1, ua = 1, pd = 1, ad = 1)
  rv <- shiny::reactiveValues(raw_data = d,
                              mapping = list(eq5d_version = "3L",
                                             names_eq5d = DIMS),
                              processed_data = NULL, results = list(),
                              value_cols = character(0), steps = list())
  shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    html <- as.character(output$validation_msgs$html)
    expect_match(html, paste0("<strong>Please check whether your data use ",
                              "EQ-5D-3L or EQ-5D-5L.</strong>"), fixed = TRUE)
  })
})
