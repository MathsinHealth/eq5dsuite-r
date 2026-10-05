# eq5d_profile_change_summary() reports a count of nobody as 0, not NA.
#
# Counts came from aggregate(), which only returns the cells that occur, so a
# level nobody reported, or a dimension where nobody reported a problem, was
# NA after widening -- and the change in the number reporting problems was NA
# with it. One respondent at mo = 2 then mo = 1 gave a post-op problem count
# of NA and a change of NA, where it is 0 and -1.
#
# Rule: the denominator is the number of non-missing responses on that
# dimension at that visit. Where it is positive, every level of the
# instrument, and the problem count, is a number, 0 when nobody reported it.
# Where it is zero -- no records at that visit, or all of them missing on
# that dimension -- the counts are NA: the data cannot say how many reported a
# problem. The relative change is NA when the previous count is 0.

DIMS <- c("mo", "sc", "ua", "pd", "ad")

cs <- function(df, levels_fu = c("pre", "post"), version = "3L")
  suppressWarnings(suppressMessages(eq5d_profile_change_summary(
    df, names_eq5d = DIMS, eq5d_version = version, name_fu = "fu",
    levels_fu = levels_fu)))

rec <- function(fu, mo = 1, sc = 1, ua = 1, pd = 1, ad = 1)
  data.frame(fu = fu, mo = mo, sc = sc, ua = ua, pd = pd, ad = ad,
             stringsAsFactors = FALSE)

cell <- function(tab, level, col) {
  row <- if (level %in% c("problems", "change"))
    grep(if (level == "problems") "^Number reporting any problems"
         else "^Change in numbers", tab$level)
  else which(tab$level == level)
  expect_length(row, 1L)
  tab[[col]][row]
}

test_that("the review's respondent: post-op count 0, change -1", {
  tab <- cs(rbind(rec("pre", mo = 2), rec("post", mo = 1)))
  expect_identical(cell(tab, "problems", "n_pre_mo"), 1)
  expect_identical(cell(tab, "problems", "n_post_mo"), 0)
  expect_identical(cell(tab, "problems", "freq_post_mo"), 0)
  expect_identical(cell(tab, "change", "n_post_mo"), -1)
  expect_identical(cell(tab, "change", "freq_post_mo"), -1)
  # The level cells nobody reported are 0 too.
  expect_identical(cell(tab, "2", "n_post_mo"), 0)
  expect_identical(cell(tab, "1", "n_pre_mo"), 0)
  expect_identical(cell(tab, "1", "freq_pre_mo"), 0)
})

test_that("dimensions with no problems beside one with problems are 0", {
  tab <- cs(rbind(rec("pre", mo = 2), rec("post", mo = 1)))
  for (d in DIMS[-1]) {
    expect_identical(cell(tab, "problems", paste0("n_pre_", d)), 0, info = d)
    expect_identical(cell(tab, "problems", paste0("n_post_", d)), 0, info = d)
    expect_identical(cell(tab, "change", paste0("n_post_", d)), 0, info = d)
    # 0 to 0 is no change in number, and no relative change can be computed.
    expect_true(is.na(cell(tab, "change", paste0("freq_post_", d))), info = d)
  }
})

test_that("every level of the instrument has a row", {
  tab3 <- cs(rbind(rec("pre", mo = 2), rec("post", mo = 1)))
  expect_true(all(c("1", "2", "3") %in% tab3$level))
  expect_identical(cell(tab3, "3", "n_pre_mo"), 0)

  tab5 <- cs(rbind(rec("pre", mo = 4), rec("post", mo = 1)), version = "5L")
  expect_true(all(as.character(1:5) %in% tab5$level))
  expect_identical(cell(tab5, "5", "n_post_mo"), 0)
  expect_identical(cell(tab5, "4", "n_pre_mo"), 1)
})

test_that("per-dimension counts sum to the non-missing denominator", {
  set <- withr::with_seed(3, data.frame(
    fu = rep(c("pre", "post"), each = 40),
    mo = sample(c(1:3, NA), 80, TRUE), sc = sample(1:3, 80, TRUE),
    ua = sample(c(1, 1, 2), 80, TRUE), pd = 1, ad = sample(c(1:3, 9), 80, TRUE)))
  tab <- cs(set)
  for (d in DIMS) for (f in c("pre", "post")) {
    col <- paste0("n_", f, "_", d)
    v <- set[[d]][set$fu == f]
    denom <- sum(v %in% 1:3)
    expect_identical(sum(tab[[col]][tab$level %in% c("1", "2", "3")]),
                     as.numeric(denom), info = col)
    expect_identical(cell(tab, "Total", col), as.numeric(denom), info = col)
    expect_identical(cell(tab, "problems", col), as.numeric(sum(v %in% 2:3)),
                     info = col)
  }
})

test_that("all respondents in full health: zeros, and no relative change", {
  tab <- cs(rbind(rec("pre"), rec("pre"), rec("post")))
  for (d in DIMS) {
    expect_identical(cell(tab, "problems", paste0("n_pre_", d)), 0)
    expect_identical(cell(tab, "problems", paste0("freq_post_", d)), 0)
  }
})

test_that("a visit with no records has no counts, and no change", {
  lv <- c("pre", "mid", "post")
  tab <- cs(rbind(rec("pre", mo = 2), rec("post", mo = 2)), levels_fu = lv)
  expect_true(is.na(cell(tab, "problems", "n_mid_mo")))
  expect_true(is.na(cell(tab, "2", "n_mid_mo")))
  expect_equal(cell(tab, "Total", "n_mid_mo"), 0)
  # Neither the change into the empty visit nor out of it can be computed.
  expect_true(is.na(cell(tab, "change", "n_mid_mo")))
  expect_true(is.na(cell(tab, "change", "n_post_mo")))
})

test_that("a dimension missing for everyone at a visit has no counts", {
  tab <- cs(rbind(rec("pre", mo = 2), rec("post", mo = NA)))
  expect_true(is.na(cell(tab, "problems", "n_post_mo")))
  expect_true(is.na(cell(tab, "1", "n_post_mo")))
  expect_equal(cell(tab, "Total", "n_post_mo"), 0)
  expect_identical(cell(tab, "Missing data", "n_post_mo"), 1)
  expect_true(is.na(cell(tab, "change", "n_post_mo")))
  # The other dimensions are unaffected.
  expect_identical(cell(tab, "problems", "n_post_sc"), 0)
})

test_that("the problems row names the levels of the instrument", {
  y <- cs(rbind(rec("pre", mo = 2), rec("post")), version = "Y3L")
  expect_true(any(y$level == "Number reporting any problems (levels 2+3)"))
  f <- cs(rbind(rec("pre", mo = 2), rec("post")), version = "5L")
  expect_true(any(f$level == "Number reporting any problems (levels 2+3+4+5)"))
})

test_that("ordinary data are unchanged where every cell was observed", {
  d <- example_data
  names(d)[names(d) == "time"] <- "fu"
  tab <- cs(d, levels_fu = c("Pre-op", "Post-op"))
  # Hand-computed from the data.
  pre <- d$mo[d$fu == "Pre-op"]
  expect_identical(cell(tab, "2", "n_Pre-op_mo"), as.numeric(sum(pre == 2)))
  expect_equal(cell(tab, "2", "freq_Pre-op_mo"),
               sum(pre == 2) / sum(pre %in% 1:3))
  post <- d$mo[d$fu == "Post-op"]
  expect_identical(cell(tab, "change", "n_Post-op_mo"),
                   as.numeric(sum(post %in% 2:3) - sum(pre %in% 2:3)))
})
