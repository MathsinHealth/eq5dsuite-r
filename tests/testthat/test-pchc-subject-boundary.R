# The Paretian Classification of Health Change is computed by .pchc(), which
# differences each row against the row above it. That is only valid inside one
# respondent's records.
#
# Before the fix, a respondent whose first record was a follow-up (no baseline
# recorded) was differenced against the *previous respondent's* last record,
# which invented a health change for someone who cannot have one. Loss to
# follow-up and late enrolment make that a routine shape for real data.
#
# The seven exported functions that depend on .pchc() all rename the id column
# to "id" and sort by id, groupvar and follow-up before calling it.

pchc_input <- function(id, time, mo, sc = mo, ua = mo, pd = mo, ad = mo) {
  d <- data.frame(id = id, time = time,
                  mo = mo, sc = sc, ua = ua, pd = pd, ad = ad,
                  stringsAsFactors = FALSE)
  d <- suppressMessages(suppressWarnings(
    .prep_eq5d(d, names = c("mo", "sc", "ua", "pd", "ad"))))
  d <- .prep_fu(d, name = "time", levels = c("Pre-op", "Post-op"))
  d[order(d$id, d$fu), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# The bug itself
# ---------------------------------------------------------------------------

test_that(".pchc() does not compare one respondent against another", {
  # id 1 complete and unchanged; id 2 has only a follow-up record.
  d <- pchc_input(id   = c(1, 1, 2),
                  time = c("Pre-op", "Post-op", "Post-op"),
                  mo   = c(1, 1, 3))

  got <- .pchc(d, level_fu_1 = "Pre-op")

  # id 1's baseline is NA, its follow-up is "No change".
  expect_true(is.na(got$state[got$id == 1 & got$fu == "Pre-op"]))
  expect_identical(got$state[got$id == 1 & got$fu == "Post-op"], "No change")

  # id 2 has nothing to compare against and must be NA, not "Worsen".
  expect_true(is.na(got$state[got$id == 2]))

  # The per-dimension differences must be NA too, since the dimension plots
  # count them.
  for (dom in c("mo", "sc", "ua", "pd", "ad"))
    expect_true(is.na(got[[paste0(dom, "_diff")]][got$id == 2]))
})

test_that("eq5d_profile_pchc_table() excludes respondents with no baseline", {
  d <- data.frame(id   = c(1, 1, 2),
                  time = c("Pre-op", "Post-op", "Post-op"),
                  mo = c(1, 1, 3), sc = c(1, 1, 3), ua = c(1, 1, 3),
                  pd = c(1, 1, 3), ad = c(1, 1, 3))

  r <- suppressWarnings(suppressMessages(eq5d_profile_pchc_table(
    d, name_id = "id", names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
    name_fu = "time", levels_fu = c("Pre-op", "Post-op"))))

  # Only id 1 contributes: one respondent, "No change", and no "Worsen" row.
  expect_false("Worsen" %in% r$state)
  expect_true("No change" %in% r$state)
  total <- r[r$state == "Grand Total", grep("_n$", names(r)), drop = TRUE]
  expect_equal(unname(unlist(total)), 1)
})

test_that("a respondent missing the follow-up is unaffected", {
  # The reverse shape was always handled: a baseline-only respondent is
  # blanked by the existing `fu == level_fu_1` rule.
  d <- pchc_input(id   = c(1, 1, 2),
                  time = c("Pre-op", "Post-op", "Pre-op"),
                  mo   = c(1, 2, 3))
  got <- .pchc(d, level_fu_1 = "Pre-op")

  expect_identical(got$state[got$id == 1 & got$fu == "Post-op"], "Worsen")
  expect_true(is.na(got$state[got$id == 2]))
})

# ---------------------------------------------------------------------------
# Complete data is unchanged by the fix
# ---------------------------------------------------------------------------

test_that("complete pairs classify as before", {
  d <- pchc_input(
    id   = c(1, 1, 2, 2, 3, 3, 4, 4),
    time = rep(c("Pre-op", "Post-op"), 4),
    mo   = c(1, 1,   2, 1,   1, 2,   1, 2),
    sc   = c(1, 1,   2, 1,   1, 2,   2, 1))

  got <- .pchc(d, level_fu_1 = "Pre-op")
  fu  <- got[got$fu == "Post-op", ]
  fu  <- fu[order(fu$id), ]

  expect_identical(fu$state,
                   c("No change", "Improve", "Worsen", "Mixed change"))
  # Every baseline row stays NA.
  expect_true(all(is.na(got$state[got$fu == "Pre-op"])))
})

test_that("the whole example dataset is classified identically", {
  # Every one of the 5,000 respondents in example_data has both a Pre-op and a
  # Post-op record, so no row starts a new respondent at the wrong follow-up
  # and the fix cannot move a single classification. These counts therefore
  # lock in the pre-fix output.
  tb <- table(example_data$id, example_data$time)
  expect_true(all(tb > 0))

  r <- suppressWarnings(suppressMessages(eq5d_profile_pchc_table(
    example_data, name_id = "id",
    names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
    name_fu = "time", levels_fu = c("Pre-op", "Post-op"))))

  n_col <- grep("_n$", names(r), value = TRUE)[1]
  counts <- setNames(r[[n_col]], r$state)
  expect_identical(
    counts,
    c("No change" = 622, "Improve" = 3195, "Worsen" = 383,
      "Mixed change" = 326, "Grand Total" = 4526))
})

# ---------------------------------------------------------------------------
# Guard
# ---------------------------------------------------------------------------

test_that(".pchc() requires an id column", {
  d <- pchc_input(id = c(1, 1), time = c("Pre-op", "Post-op"), mo = c(1, 2))
  d$id <- NULL
  expect_error(.pchc(d, level_fu_1 = "Pre-op"), "must contain an `id` column")
})

test_that(".pchctab() has been removed", {
  # It had no callers anywhere in the package, and carried a latent
  # factor(labels=) bug that errored whenever every respondent fell on the
  # same side of the baseline "any problems" split.
  expect_false(exists(".pchctab", envir = asNamespace("eq5dsuite"),
                      inherits = FALSE))
})
