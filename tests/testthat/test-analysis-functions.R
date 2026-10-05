# A worked example for each of the 31 exported analysis functions, on a
# fixture small enough to check by hand.
#
# Before this file, 15 of the 31 were never called by any test and the file
# holding them sat at 39% line coverage -- the headline functionality of the
# package, and where H-1 through H-6 were found.
#
# The fixture: four respondents, two time points, EQ-5D-3L, valued with GB.
#
#   id time      state   GB value   group  VAS  age
#    1 Pre-op    11111    1.000     A      100   30
#    1 Post-op   11111    1.000     A       90   30
#    2 Pre-op    22222    0.516     A       60   50
#    2 Post-op   11111    1.000     A       80   50
#    3 Pre-op    33333   -0.594     B       20   70
#    3 Post-op   33333   -0.594     B       30   70
#    4 Pre-op    11111    1.000     B       70   40
#    4 Post-op   21311    0.487     B       50   40
#
# By hand, the Paretian classification of each respondent's change is:
#   id 1  11111 -> 11111  No change
#   id 2  22222 -> 11111  Improve      (better on all five)
#   id 3  33333 -> 33333  No change
#   id 4  11111 -> 21311  Worsen       (worse on mo and ua, no dimension better)

dims <- c("mo", "sc", "ua", "pd", "ad")
lv   <- c("Pre-op", "Post-op")

fixture <- function() data.frame(
  id   = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
  time = rep(lv, 4),
  mo   = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 2L),
  sc   = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 1L),
  ua   = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 3L),
  pd   = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 1L),
  ad   = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 1L),
  vas  = c(100L, 90L, 60L, 80L, 20L, 30L, 70L, 50L),
  grp  = c("A", "A", "A", "A", "B", "B", "B", "B"),
  age  = c(30, 30, 50, 50, 70, 70, 40, 40),
  # The GB values of the states above, as tabulated in the header. The
  # eq5d_utility_* functions analyse a value column rather than calculating
  # one, so the fixture carries it.
  value = c(1, 1, 0.516, 1, -0.594, -0.594, 1, 0.487),
  stringsAsFactors = FALSE)

q <- function(expr) suppressWarnings(suppressMessages(expr))

# Plot functions return list(plot_data, p); tables return a data.frame.
expect_plot <- function(r, info = "") {
  expect_true(is.list(r), info = info)
  expect_named(r, c("plot_data", "p"), ignore.order = TRUE, info = info)
  expect_s3_class(r$p, "ggplot")
  expect_s3_class(r$plot_data, "data.frame")
  expect_gt(nrow(r$plot_data), 0L)
  # It builds, which is where a bad aes() or scale shows up.
  expect_no_error(ggplot2::ggplot_build(r$p))
  invisible(r$plot_data)
}

# ---------------------------------------------------------------------------
# Profile: levels and states
# ---------------------------------------------------------------------------

test_that("eq5d_profile_level_summary() counts levels by dimension", {
  r <- q(eq5d_profile_level_summary(fixture(), names_eq5d = dims,
                                    eq5d_version = "3L"))
  n <- function(level, dim) r[[paste0("n_All_", dim)]][r$level == level]

  # mobility over the eight rows: 1,1,2,1,3,3,1,2 -> four 1s, two 2s, two 3s.
  expect_equal(n("1", "mo"), 4)
  expect_equal(n("2", "mo"), 2)
  expect_equal(n("3", "mo"), 2)
  expect_equal(n("Total", "mo"), 8)
  expect_equal(n("Number reporting any problems (levels 2+3)", "mo"), 4)
  # self-care: 1,1,2,1,3,3,1,1 -> five 1s, one 2, two 3s.
  expect_equal(n("1", "sc"), 5)
  expect_equal(n("2", "sc"), 1)
  expect_equal(n("3", "sc"), 2)
})

test_that("eq5d_profile_level_summary_by_group() splits by the category", {
  r <- q(eq5d_profile_level_summary_by_group(
    fixture(), names_eq5d = dims, name_cat = "grp",
    levels_cat = c("A", "B"), eq5d_version = "3L"))

  expect_s3_class(r, "data.frame")
  expect_true(all(c("n_A_mo", "n_B_mo") %in% names(r)))
  # Group A mobility: 1,1,2,1 -> three 1s and one 2. Group B: 3,3,1,2.
  expect_equal(r$n_A_mo[r$level == "1"], 3)
  expect_equal(r$n_A_mo[r$level == "2"], 1)
  expect_equal(r$n_B_mo[r$level == "3"], 2)
})

test_that("eq5d_profile_change_summary() splits by follow-up", {
  # This one forwards match.call() to do.call(), so its arguments are
  # re-evaluated in the package namespace: `fixture()` would not be found.
  r <- q(eq5d_profile_change_summary(
    data.frame(
      mo = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 2L),
      sc = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 1L),
      ua = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 3L),
      pd = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 1L),
      ad = c(1L, 1L, 2L, 1L, 3L, 3L, 1L, 1L),
      time = rep(c("Pre-op", "Post-op"), 4)),
    names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
    name_fu = "time", levels_fu = c("Pre-op", "Post-op"),
    eq5d_version = "3L"))

  # Columns in the requested order (H-3), Pre-op first.
  mo_cols <- grep("_mo$", names(r), value = TRUE)
  expect_identical(mo_cols[1], "n_Pre-op_mo")
  # Pre-op mobility: 1,2,3,1 -> two 1s. Post-op: 1,1,3,2 -> two 1s.
  expect_equal(r[["n_Pre-op_mo"]][r$level == "1"], 2)
  expect_equal(r[["n_Post-op_mo"]][r$level == "1"], 2)
})

test_that("eq5d_profile_top_states() ranks the observed states", {
  r <- q(eq5d_profile_top_states(fixture(), names_eq5d = dims,
                                 eq5d_version = "3L", n = 3))
  expect_s3_class(r, "data.frame")
  # 11111 appears four times, 33333 twice, 22222 and 21311 once each.
  expect_identical(r[[1]][1], "11111")
  expect_equal(r$Frequency[1], 4)
  expect_equal(r$Percentage[1], 0.5)   # a proportion, not a percentage
  # The worst state is always shown, even outside the top n.
  expect_true("33333" %in% r[[1]])
})

test_that("eq5d_profile_dimension_change_table() reports level transitions", {
  r <- q(eq5d_profile_dimension_change_table(
    fixture(), name_id = "id", names_eq5d = dims,
    name_fu = "time", levels_fu = lv))

  expect_s3_class(r, "data.frame")
  expect_true(all(c("diff", "level_change") %in% names(r)))
  expect_true(all(c("No change", "Better", "Worse") %in% r$diff))
  # Mobility: id 1 stays 1-1, id 2 improves 2->1, id 3 stays 3-3, id 4
  # worsens 1->2. So exactly one "Worse" 1-2 transition, at 25% of the four.
  worse_12 <- r[r$diff == "Worse" & r$level_change == "1-2", "mo_% Total"]
  expect_equal(worse_12, 0.25)
})

# ---------------------------------------------------------------------------
# Profile: PCHC
# ---------------------------------------------------------------------------

test_that("eq5d_profile_pchc_table() classifies each respondent's change", {
  r <- q(eq5d_profile_pchc_table(fixture(), name_id = "id", names_eq5d = dims,
                                 name_fu = "time", levels_fu = lv))
  n <- function(state) r[[grep("_n$", names(r), value = TRUE)[1]]][r$state == state]

  expect_equal(n("No change"), 2)     # ids 1 and 3
  expect_equal(n("Improve"), 1)       # id 2
  expect_equal(n("Worsen"), 1)        # id 4
  # Only the categories that occur are listed; nobody changed in both
  # directions.
  expect_false("Mixed change" %in% r$state)
  expect_equal(n("Grand Total"), 4)
})

test_that("eq5d_profile_pchc_with_no_problems_table() separates full health", {
  r <- q(eq5d_profile_pchc_with_no_problems_table(
    fixture(), name_id = "id", name_groupvar = "grp", names_eq5d = dims,
    name_fu = "time", levels_fu = lv))

  expect_s3_class(r, "data.frame")
  # id 1 is at 11111 at both time points, so its "No change" is reported
  # separately from id 3's, which is at 33333 throughout.
  expect_true("No problems" %in% r$state)
  expect_true("No change" %in% r$state)
})

test_that("the PCHC and dimension plots run over groups", {
  f <- fixture()
  args <- list(name_id = "id", name_groupvar = "grp", names_eq5d = dims,
               name_fu = "time", levels_fu = lv)
  expect_plot(q(do.call(eq5d_profile_pchc_by_group_plot, c(list(f), args))))
  expect_plot(q(do.call(eq5d_profile_better_dimensions_by_group_plot,
                        c(list(f), args))))
  expect_plot(q(do.call(eq5d_profile_worse_dimensions_by_group_plot,
                        c(list(f), args))))

  # Nobody in the fixture changed in both directions, so the mixed-change plot
  # has nothing to draw. That used to fail inside data.frame() with "arguments
  # imply differing number of rows: 0, 1"; it now says so.
  expect_error(
    q(do.call(eq5d_profile_mixed_dimensions_by_group_plot, c(list(f), args))),
    "No respondent was classified as a mixed change", fixed = TRUE)

  # With a respondent who improves on one dimension and worsens on another, it
  # draws.
  g <- f
  g[8, c("mo", "sc")] <- list(2L, 1L)
  g[7, c("mo", "sc")] <- list(1L, 2L)
  expect_plot(q(do.call(eq5d_profile_mixed_dimensions_by_group_plot,
                        c(list(g), args))))
})

test_that("eq5d_profile_health_profile_grid() ranks every possible state", {
  r <- expect_plot(q(eq5d_profile_health_profile_grid(
    fixture(), names_eq5d = dims, name_fu = "time", levels_fu = lv,
    name_id = "id", eq5d_version = "3L", country = "GB")))
  expect_true(all(c("rank_t1", "rank_t2") %in% names(r)))

  # The ranking is over all 243 EQ-5D-3L states, not the four the fixture
  # holds, so a rank means the same thing in every sample valued with GB.
  # 11111 is the best state there is, and 33333 the worst.
  rk <- function(profile, col) unique(r[[col]][r[[sub("rank", "profile", col)]] == profile])
  expect_equal(rk(11111, "rank_t2"), 1)
  expect_equal(rk(33333, "rank_t2"), 243)
  # 22222 and 21311 sit where the GB value set puts them among all 243 (33rd
  # and 34th), not at 2nd and 3rd as they did when only the fixture's four
  # observed states were ranked. 21311 appears only as a follow-up state.
  expect_gt(rk(22222, "rank_t1"), 2)
  expect_lt(rk(22222, "rank_t1"), rk(21311, "rank_t2"))
  # id 2 improved 22222 -> 11111, so it is better (lower) at t2 than t1.
  expect_lt(r$rank_t2[r$id == 2], r$rank_t1[r$id == 2])
})

test_that("the HPG axes span the whole classification system", {
  r <- q(eq5d_profile_health_profile_grid(
    fixture(), names_eq5d = dims, name_fu = "time", levels_fu = lv,
    name_id = "id", eq5d_version = "3L", country = "GB"))
  expect_equal(r$p$scales$scales[[1L]]$limits, c(1, 243))
  expect_equal(r$p$scales$scales[[2L]]$limits, c(1, 243))
  # The labels name the axes the mapping actually uses: x is the second
  # follow-up level, y the first.
  expect_equal(r$p$labels$x, paste(lv[2L], "rank"))
  expect_equal(r$p$labels$y, paste(lv[1L], "rank"))
})

# ---------------------------------------------------------------------------
# Profile: LSS, LFS and the density curve
# ---------------------------------------------------------------------------

test_that("eq5d_profile_lss_utility_summary() summarises by level sum score", {
  r <- q(eq5d_profile_lss_utility_summary(fixture(), names_eq5d = dims,
                                          name_utility = "value",
                                          eq5d_version = "3L"))
  expect_s3_class(r, "data.frame")
  # LSS is the sum of the five levels: 11111 -> 5, 22222 -> 10, 33333 -> 15,
  # 21311 -> 8.
  expect_setequal(as.character(r$LSS), c("5", "8", "10", "15", "Missing"))
  expect_equal(r$Number[r$LSS == "5"], 4)     # four rows at 11111
  expect_equal(r$Mean[r$LSS == "5"], 1)
  expect_equal(r$Mean[r$LSS == "15"], -0.594, tolerance = 1e-6)
  expect_equal(r$Number[r$LSS == "Missing"], 0)
})

test_that("eq5d_profile_lfs_distribution() counts level frequency scores", {
  r <- q(eq5d_profile_lfs_distribution(fixture(), names_eq5d = dims,
                                       eq5d_version = "3L"))
  expect_s3_class(r, "data.frame")
  # LFS counts how many dimensions are at each level: 11111 -> "500",
  # 22222 -> "050", 33333 -> "005", 21311 -> "311".
  expect_true(all(c("500", "050", "005", "311") %in% as.character(r[[1]])))
})

test_that("the LFS utility functions agree with the values supplied", {
  f <- fixture()
  # One row per distinct value, one column per level frequency score.
  mean_u <- q(eq5d_profile_lfs_mean_utility(f, names_eq5d = dims,
                                            name_utility = "value",
                                            eq5d_version = "3L"))
  expect_identical(names(mean_u),
                   c("EQ-5D value", "005", "050", "311", "500", "Total"))
  # The four rows at 11111 have LFS "500" and value 1.
  expect_equal(mean_u[["500"]][mean_u[["EQ-5D value"]] == 1], 4)
  expect_equal(mean_u[["005"]][abs(mean_u[["EQ-5D value"]] + 0.594) < 1e-6], 2)

  summ <- q(eq5d_profile_lfs_utility_summary(f, names_eq5d = dims,
                                             name_utility = "value",
                                             eq5d_version = "3L"))
  expect_s3_class(summ, "data.frame")
  expect_equal(summ$Mean[as.character(summ[[1]]) == "005"], -0.594,
               tolerance = 1e-6)
})

test_that("the LSS and LFS plots run", {
  f <- fixture()
  expect_plot(q(eq5d_profile_lss_utility_plot(f, names_eq5d = dims,
                                              name_utility = "value",
                                              eq5d_version = "3L")))
  expect_plot(q(eq5d_profile_lfs_utility_plot(f, names_eq5d = dims,
                                              name_utility = "value",
                                              eq5d_version = "3L")))
})

test_that("eq5d_profile_density_curve() returns the curve and the index", {
  r <- q(eq5d_profile_density_curve(fixture(), names_eq5d = dims,
                                    eq5d_version = "3L"))
  expect_named(r, c("plot_data", "hsdi", "p"))
  # Four distinct states over eight observations: 11111 four times, 33333
  # twice, 22222 and 21311 once each.
  expect_identical(nrow(r$plot_data), 4L)
  expect_equal(r$plot_data$Frequency, c(4, 2, 1, 1))
  expect_equal(r$plot_data$CumPropObservations, c(0.5, 0.75, 0.875, 1))
  expect_equal(r$plot_data$CumPropStates, c(0.25, 0.5, 0.75, 1))
})

# ---------------------------------------------------------------------------
# Utility
# ---------------------------------------------------------------------------

test_that("eq5d_utility_summary() reports hand-checked statistics", {
  r <- q(eq5d_utility_summary(fixture(), name_utility = "value",
                              name_fu = "time", levels_fu = lv))
  pre  <- c(1, 0.516, -0.594, 1)       # ids 1-4 at Pre-op
  post <- c(1, 1, -0.594, 0.487)       # ids 1-4 at Post-op

  get <- function(col, stat) r[[col]][r$name == stat]
  expect_equal(get("Pre-op", "Mean"),  mean(pre),  tolerance = 1e-6)
  expect_equal(get("Post-op", "Mean"), mean(post), tolerance = 1e-6)
  expect_equal(get("Pre-op", "Median"), median(pre), tolerance = 1e-6)
  expect_equal(get("Pre-op", "Minimum"), -0.594, tolerance = 1e-6)
  expect_equal(get("Pre-op", "Maximum"), 1)
  expect_equal(get("Pre-op", "Range"), 1 - (-0.594), tolerance = 1e-6)
  expect_equal(get("Pre-op", "Observations"), 4)
  expect_equal(get("Pre-op", "Total sample"), 4)
  expect_equal(get("Pre-op", "Missing (n)"), 0)
})

test_that("eq5d_utility_summary_by_group() splits by group", {
  r <- q(eq5d_utility_summary_by_group(fixture(), name_utility = "value",
                                       name_groupvar = "grp"))
  expect_s3_class(r, "data.frame")
  # Group A holds 11111, 11111, 22222, 11111; group B 33333, 33333, 11111,
  # 21311.
  expect_identical(names(r), c("name", "A", "B", "All groups"))
  # Group A: 1, 1, 0.516, 1. Group B: -0.594, -0.594, 1, 0.487.
  expect_equal(r[["A"]][r$name == "Mean"], mean(c(1, 1, 0.516, 1)),
               tolerance = 1e-6)
  expect_equal(r[["A"]][r$name == "Median"], 1)
  expect_equal(r[["B"]][r$name == "Median"],
               median(c(-0.594, -0.594, 1, 0.487)), tolerance = 1e-6)
  expect_equal(r[["All groups"]][r$name == "N"], 8)
})

test_that("eq5d_utility_norms_comparison() lays values out by age band", {
  r <- q(eq5d_utility_norms_comparison(
    fixture(), name_utility = "value", name_fu = "time", levels_fu = lv,
    name_groupvar = "grp", name_age = "age"))
  pd <- if (is.data.frame(r)) r else r$plot_data
  expect_s3_class(pd, "data.frame")
  # The fixture's ages 30, 40, 50 and 70 fall in four different bands.
  expect_true(all(c("25-34", "35-44", "45-54", "65-74") %in% names(pd)))
})

test_that("the utility plots run", {
  f <- fixture()
  expect_plot(q(eq5d_utility_distribution_plot(f, name_utility = "value")))
  expect_plot(q(eq5d_utility_by_group_plot(f, name_utility = "value",
                                           name_groupvar = "grp")))
  expect_plot(q(eq5d_utility_over_time_plot(f, name_utility = "value",
                                            name_fu = "time", levels_fu = lv)))
  expect_plot(q(eq5d_utility_change_by_group_plot(
    f, name_utility = "value", name_fu = "time", levels_fu = lv,
    name_groupvar = "grp")))
  expect_plot(q(eq5d_utility_vas_scatter_plot(f, name_utility = "value",
                                              name_vas = "vas")))
})

# ---------------------------------------------------------------------------
# EQ VAS
# ---------------------------------------------------------------------------

test_that("eq5d_vas_summary() reports hand-checked statistics", {
  r <- q(eq5d_vas_summary(fixture(), name_vas = "vas", name_fu = "time",
                          levels_fu = lv))
  pre <- c(100, 60, 20, 70)
  m <- mean(pre)
  get <- function(stat) r[["Pre-op"]][r$name == stat]

  expect_equal(get("Mean"), 62.5)
  expect_equal(get("Median"), 65)
  expect_equal(get("Standard deviation"), sd(pre))
  expect_equal(get("Standard error"), sd(pre) / 2)
  expect_equal(get("Minimum"), 20)
  expect_equal(get("Maximum"), 100)
  expect_equal(get("Range"), 80)
  expect_equal(get("Observations"), 4)
  # Population estimators, kurtosis non-excess (M-15).
  expect_equal(get("Skewness"), mean((pre - m)^3) / mean((pre - m)^2)^1.5)
  expect_equal(get("Kurtosis (non-excess)"),
               mean((pre - m)^4) / mean((pre - m)^2)^2)
})

test_that("eq5d_vas_distribution_table() bins the VAS", {
  r <- q(eq5d_vas_distribution_table(fixture(), name_vas = "vas"))
  expect_s3_class(r, "data.frame")
  expect_identical(names(r), c("Range", "Midpoint", "Frequency"))
  # 25 bands, then three summary rows.
  expect_identical(utils::tail(r$Range, 3),
                   c("Total observed", "Missing", "Total sample"))
  expect_equal(sum(r$Frequency[!is.na(r$Midpoint)]), 8)
  expect_equal(r$Frequency[r$Range == "Total observed"], 8)
  expect_equal(r$Frequency[r$Range == "Missing"], 0)
  # The eight VAS values fall in eight different bands.
  expect_equal(sum(r$Frequency[!is.na(r$Midpoint)] > 0), 8)
  expect_equal(r$Frequency[r$Range == "100"], 1)
  expect_equal(r$Frequency[r$Range == "18-22"], 1)
})

test_that("the VAS plots run", {
  f <- fixture()
  expect_plot(q(eq5d_vas_histogram(f, name_vas = "vas")))
  expect_plot(q(eq5d_vas_grouped_distribution_plot(f, name_vas = "vas")))
})

# ---------------------------------------------------------------------------
# Coverage of the exported surface
# ---------------------------------------------------------------------------

test_that("every exported analysis function is exercised somewhere", {
  # The analysis families only: eq5d_read_data() and the other data-handling
  # functions are exported too, and have their own tests.
  exported <- grep("^eq5d_(profile|utility|vas)_",
                   getNamespaceExports("eq5dsuite"), value = TRUE)
  expect_identical(length(exported), 32L)

  files <- list.files(testthat::test_path("."), pattern = "^test.*[.]R$",
                      full.names = TRUE)
  txt <- unlist(lapply(files, readLines, warn = FALSE))
  # Called directly, or passed by name to do.call().
  called <- vapply(exported,
                   function(f) any(grepl(paste0("\\b", f, "\\b"), txt)),
                   logical(1))

  expect_identical(names(called)[!called], character(0))
})
