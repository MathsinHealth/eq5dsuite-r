# eq5d_profile_health_profile_grid() honours the arguments it documents.
#
# Custom dimension names failed with "replacement has 0 rows": the columns
# were renamed to mo..ad and then looked up by the caller's names. EQ-5D-Y-3L
# failed with "Unknown EQ-5D version 'Y3L'", because the 243 states to rank
# were enumerated by a function that accepted only "3L" and "5L". The grid
# compares two timepoints, but nothing said so when given more or fewer.

DIMS <- c("mo", "sc", "ua", "pd", "ad")

hpg <- function(df, version = "3L", country = "GB", names_eq5d = DIMS,
                levels_fu = c("pre", "post"))
  suppressMessages(eq5d_profile_health_profile_grid(
    df, names_eq5d = names_eq5d, name_fu = "fu", levels_fu = levels_fu,
    name_id = "id", eq5d_version = version, country = country))

two_visits <- function(n = 30, max_level = 3, seed = 1) {
  withr::with_seed(seed, {
    d <- data.frame(id = rep(seq_len(n), 2),
                    fu = rep(c("pre", "post"), each = n))
    for (k in DIMS) d[[k]] <- sample(seq_len(max_level), 2 * n, TRUE)
  })
  d
}

test_that("custom dimension names give the same grid as the standard ones", {
  d <- two_visits()
  ref <- hpg(d)
  custom <- d
  names(custom)[match(DIMS, names(custom))] <- c("Mob", "self care", "UA",
                                                 "pain", "anx")
  got <- hpg(custom, names_eq5d = c("Mob", "self care", "UA", "pain", "anx"))
  keep <- c("id", "fu", "profile_t1", "profile_t2", "rank_t1", "rank_t2",
            "state")
  expect_identical(got$plot_data[, keep], ref$plot_data[, keep])
  expect_s3_class(got$p, "ggplot")
})

test_that("3L, 5L and Y3L each rank every state of the instrument", {
  for (v in list(c("3L", "GB", 3), c("5L", "GB", 5), c("Y3L", "DE", 3))) {
    d <- two_visits(max_level = as.integer(v[3]))
    out <- hpg(d, version = v[1], country = v[2])
    n_states <- as.integer(v[3])^5
    expect_true(all(out$plot_data$rank_t1 %in% seq_len(n_states)), info = v[1])
    expect_false(anyNA(out$plot_data$rank_t2), info = v[1])
    # The axes span the whole classification system.
    b <- ggplot2::ggplot_build(out$p)
    expect_equal(b$layout$panel_params[[1]]$x.range, c(1, n_states),
                 info = v[1])
  }
})

test_that("Y3L is ranked by the Y3L value set, not the 3L one", {
  d <- two_visits()
  y <- hpg(d, version = "Y3L", country = "DE")
  vs <- eq5d(make_all_EQ_indexes("Y3L"), country = "DE", version = "Y3L")
  best <- make_all_EQ_indexes("Y3L")[order(-vs)][1]
  expect_identical(best, 11111L)
  r <- y$plot_data
  expect_identical(
    r$rank_t1[r$profile_t1 == r$profile_t1[1]][1],
    match(r$profile_t1[1], make_all_EQ_indexes("Y3L")[order(-vs)]))
})

test_that("make_all_EQ_states() and make_all_EQ_indexes() accept Y3L", {
  expect_identical(make_all_EQ_indexes("Y3L"), make_all_EQ_indexes("3L"))
  expect_identical(make_all_EQ_indexes("y3l"), make_all_EQ_indexes("3L"))
  expect_identical(make_all_EQ_states("Y3L"), make_all_EQ_states("3L"))
})

test_that("a respondent without a baseline is left out", {
  d <- two_visits()
  d <- d[!(d$id == 1 & d$fu == "pre"), ]
  out <- hpg(d)
  expect_false(1 %in% out$plot_data$id)
  expect_identical(nrow(out$plot_data), 29L)
})

test_that("an invalid level leaves that respondent out, with a warning", {
  d <- two_visits()
  d$mo[d$id == 2 & d$fu == "post"] <- 4   # not a 3L level
  expect_warning(out <- hpg(d), "NA")
  expect_false(2 %in% out$plot_data$id)
  expect_identical(nrow(out$plot_data), 29L)
})

test_that("it takes exactly two timepoints", {
  d <- two_visits()
  expect_error(hpg(d, levels_fu = "pre"), "exactly two")
  d3 <- rbind(d, transform(d[d$fu == "post", ], fu = "late"))
  expect_error(hpg(d3, levels_fu = c("pre", "post", "late")), "exactly two")
  # Two of three is fine; the third is excluded, not paired.
  out <- suppressWarnings(hpg(d3, levels_fu = c("pre", "post")))
  expect_identical(nrow(out$plot_data), 30L)
  expect_true(all(out$plot_data$fu == "post"))
})

test_that("no respondent with both timepoints is a clear error", {
  d <- two_visits()
  d <- d[d$fu == "post", ]
  expect_error(hpg(d), "both")
})
