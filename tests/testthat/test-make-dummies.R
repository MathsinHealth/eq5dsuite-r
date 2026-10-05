# make_dummies() builds each dimension's dummies by indexing rows out of a
# level design matrix. Without drop = FALSE, a single row of input came back as
# a plain vector, cbind() laid the five dimensions out as five columns instead
# of one row of dummies, and naming the result failed with
# "length of 'dimnames' [2] not equal to array extent".

dims <- c("mo", "sc", "ua", "pd", "ad")

test_that("make_dummies() works on a single row", {
  one <- data.frame(mo = 5L, sc = 4L, ua = 3L, pd = 2L, ad = 1L)
  got <- make_dummies(one, version = "5L")

  expect_identical(dim(got), c(1L, 20L))
  expect_identical(names(got),
                   as.vector(t(outer(dims, 2:5, paste0))))
  # One dummy set per level 5, 4, 3, 2 and none for ad, which is at level 1.
  expect_identical(which(unlist(got) == 1), c(mo5 = 4L, sc4 = 7L, ua3 = 10L,
                                              pd2 = 13L))
  expect_false(anyNA(got))
})

test_that("a single row matches the same row inside a longer call", {
  df <- data.frame(mo = c(5L, 1L, 3L), sc = c(4L, 1L, 2L), ua = c(3L, 1L, 5L),
                   pd = c(2L, 1L, 4L), ad = c(1L, 1L, 1L))
  many <- make_dummies(df, version = "5L")

  for (i in seq_len(nrow(df))) {
    one <- make_dummies(df[i, , drop = FALSE], version = "5L")
    expect_equal(unname(as.matrix(one)),
                 unname(as.matrix(many[i, , drop = FALSE])),
                 info = paste("row", i))
  }
})

test_that("a single row works for every combination of the options", {
  one3 <- data.frame(mo = 3L, sc = 2L, ua = 1L, pd = 3L, ad = 1L)
  one5 <- data.frame(mo = 5L, sc = 4L, ua = 3L, pd = 2L, ad = 1L)

  grid <- expand.grid(version = c("3L", "5L", "Y3L"), incremental = c(FALSE, TRUE),
                      drop_level_1 = c(TRUE, FALSE), add_intercept = c(FALSE, TRUE),
                      return_df = c(TRUE, FALSE), stringsAsFactors = FALSE)

  for (k in seq_len(nrow(grid))) {
    a <- as.list(grid[k, ])
    df <- if (a$version == "5L") one5 else one3
    got <- do.call(make_dummies, c(list(df), a))
    n_levels <- if (a$version == "5L") 5L else 3L
    n_cols <- 5L * (if (a$drop_level_1) n_levels - 1L else n_levels) +
      as.integer(a$add_intercept)
    expect_identical(dim(got), c(1L, n_cols), info = paste(unlist(a), collapse = "/"))
    expect_false(anyNA(got), info = paste(unlist(a), collapse = "/"))
  }
})

test_that("incremental dummies are cumulative for a single row", {
  one <- data.frame(mo = 3L, sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  got <- make_dummies(one, version = "3L", incremental = TRUE)
  # mo at level 3 sets both mo2 and mo3; every other dimension is at level 1.
  expect_identical(unlist(got),
                   c(mo2 = 1, mo3 = 1, sc2 = 0, sc3 = 0, ua2 = 0, ua3 = 0,
                     pd2 = 0, pd3 = 0, ad2 = 0, ad3 = 0))
})

test_that("zero rows still return a correctly shaped result", {
  empty <- data.frame(mo = integer(0), sc = integer(0), ua = integer(0),
                      pd = integer(0), ad = integer(0))
  got <- make_dummies(empty, version = "5L")
  expect_identical(dim(got), c(0L, 20L))
})
