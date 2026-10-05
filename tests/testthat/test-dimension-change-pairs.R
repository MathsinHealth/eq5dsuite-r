# eq5d_profile_dimension_change_table() compares each respondent with
# themselves.
#
# The long table was sorted by dimension and respondent and then lagged as a
# whole, and only rows at the first timepoint were blanked, so a respondent
# whose first record was a later visit was compared with the previous
# respondent's last record. A pre 3, post 2 and B post 1 gave 3-2 and 2-1 at
# 50% each; only A has a change.
#
# Policy, the same as the PCHC analyses: a change is between consecutive
# records of one respondent, in timepoint order. A respondent's first record,
# and any record at the first timepoint, has no change. A missing visit is
# skipped over, so with three timepoints a respondent seen at the first and
# third is compared across that gap. A record with no ID cannot be attributed
# to anyone and is never paired.

DIMS <- c("mo", "sc", "ua", "pd", "ad")

dct <- function(df, levels_fu = c("pre", "post"))
  suppressMessages(eq5d_profile_dimension_change_table(
    df, name_id = "id", names_eq5d = DIMS, name_fu = "fu",
    levels_fu = levels_fu))

pchc <- function(df, levels_fu = c("pre", "post"))
  suppressMessages(eq5d_profile_pchc_table(
    df, name_id = "id", names_eq5d = DIMS, name_fu = "fu",
    levels_fu = levels_fu))

rec <- function(id, fu, mo, sc = 1, ua = 1, pd = 1, ad = 1)
  data.frame(id = id, fu = fu, mo = mo, sc = sc, ua = ua, pd = pd, ad = ad,
             stringsAsFactors = FALSE)

# The share of a dimension's changes that are `change`, e.g. "3-2".
share <- function(tab, change, dim = "mo") {
  x <- tab[[paste0(dim, "_% Total")]][tab$level_change == change]
  if (length(x)) x else 0
}

test_that("the review's three rows give one change, from A", {
  d <- rbind(rec("A", "pre", 3), rec("A", "post", 2), rec("B", "post", 1))
  tab <- dct(d)
  expect_identical(tab$level_change[tab$`mo_% Total` > 0], "3-2")
  expect_equal(share(tab, "3-2"), 1)
  expect_false("2-1" %in% tab$level_change)
})

test_that("a respondent with no baseline contributes no change", {
  d <- rbind(rec("A", "pre", 2), rec("A", "post", 2),
             rec("B", "post", 3), rec("C", "post", 1))
  tab <- dct(d)
  # Only A pairs; its other dimensions are 1-1 throughout.
  expect_identical(tab$level_change[tab$`mo_% Total` > 0], "2-2")
  expect_equal(share(tab, "2-2"), 1)
  expect_setequal(tab$level_change, c("1-1", "2-2"))
})

test_that("a missing intermediate visit is skipped over", {
  lv <- c("t1", "t2", "t3")
  d <- rbind(rec("A", "t1", 3), rec("A", "t3", 1),
             rec("B", "t1", 3), rec("B", "t2", 2), rec("B", "t3", 2))
  tab <- dct(d, lv)
  # A: 3-1 across the gap; B: 3-2 and 2-2. Three changes in all.
  expect_equal(share(tab, "3-1"), 1 / 3)
  expect_equal(share(tab, "3-2"), 1 / 3)
  expect_equal(share(tab, "2-2"), 1 / 3)
  # Nothing pairs A's last record with B's first.
  expect_false("1-3" %in% tab$level_change)
})

test_that("each dimension is paired on its own", {
  d <- rbind(rec("A", "pre", 1, sc = 3), rec("A", "post", 1, sc = 1),
             rec("B", "post", 2, sc = 2))
  tab <- dct(d)
  expect_equal(share(tab, "1-1", "mo"), 1)
  expect_equal(share(tab, "3-1", "sc"), 1)
  expect_false(any(c("1-2", "3-2", "1-3") %in% tab$level_change))
})

test_that("shuffling the rows changes nothing", {
  d <- rbind(rec("A", "pre", 3), rec("A", "post", 2), rec("B", "post", 1),
             rec("C", "pre", 1), rec("C", "post", 3), rec("D", "pre", 2))
  ref <- dct(d)
  for (seed in 1:5) {
    sh <- withr::with_seed(seed, d[sample(nrow(d)), ])
    expect_identical(dct(sh), ref, info = seed)
  }
})

test_that("a record with no ID is never paired", {
  d <- rbind(rec("A", "pre", 1), rec("A", "post", 1), rec(NA, "post", 2))
  tab <- dct(d)
  expect_identical(tab$level_change, "1-1")
  # PCHC applies the same rule.
  p <- pchc(d)
  expect_identical(p$state, c("No change", "Grand Total"))
  expect_equal(p[[2]][1], 1)
})

test_that("the pairs match PCHC and a direct pairing on example_data", {
  d <- example_data
  names(d)[names(d) == "time"] <- "fu"
  # Make some respondents incomplete: drop the baseline for some, the
  # follow-up for others, so pairing across respondents would show.
  set <- withr::with_seed(1, sample(unique(d$id), 600))
  drop <- (d$id %in% set[1:300] & d$fu == "Pre-op") |
          (d$id %in% set[301:600] & d$fu == "Post-op")
  d <- d[!drop, ]
  d <- withr::with_seed(2, d[sample(nrow(d)), ])
  lv <- c("Pre-op", "Post-op")

  tab <- suppressWarnings(dct(d, lv))

  # Direct pairing: one row per respondent with both visits.
  pre  <- d[d$fu == "Pre-op",  c("id", DIMS)]
  post <- d[d$fu == "Post-op", c("id", DIMS)]
  w <- merge(pre, post, by = "id", suffixes = c("_pre", "_post"))
  for (dim in DIMS) {
    a <- w[[paste0(dim, "_pre")]]; b <- w[[paste0(dim, "_post")]]
    ok <- a %in% 1:3 & b %in% 1:3
    expected <- table(paste0(a[ok], "-", b[ok])) / sum(ok)
    got <- stats::setNames(tab[[paste0(dim, "_% Total")]], tab$level_change)
    got <- got[got > 0]
    expect_equal(got[names(expected)], c(expected), ignore_attr = TRUE,
                 info = dim)
    expect_setequal(names(got), names(expected))
  }

  # PCHC classifies exactly the respondents with both visits and all five
  # dimensions valid at each.
  p <- suppressWarnings(pchc(d, lv))
  complete <- Reduce(`&`, lapply(DIMS, function(dim)
    w[[paste0(dim, "_pre")]] %in% 1:3 & w[[paste0(dim, "_post")]] %in% 1:3))
  expect_identical(as.integer(p[[2]][p$state == "Grand Total"]),
                   sum(complete))
})

test_that("a record at a timepoint outside levels_fu is excluded, not paired", {
  d <- rbind(rec("1", "t1", 1), rec("1", "t2", 1), rec("1", "t3", 3),
             rec("2", "t1", 2), rec("2", "t2", 2), rec("2", "t3", 2))
  tab <- suppressWarnings(dct(d, c("t1", "t2")))
  expect_false("1-3" %in% tab$level_change)
  expect_equal(share(tab, "1-1"), 0.5)
  expect_equal(share(tab, "2-2"), 0.5)
  # The rows are dropped with the warning .prep_fu() gives.
  expect_warning(dct(d, c("t1", "t2")), "not one of the levels")
  p <- suppressWarnings(pchc(d, c("t1", "t2")))
  expect_equal(p[[2]][p$state == "Grand Total"], 2)
})
