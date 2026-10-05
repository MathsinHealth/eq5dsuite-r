# eq5d_profile_lss_utility_plot() and eq5d_profile_lfs_utility_plot() show the
# median, lowest and highest EQ-5D value in each score group. Both drew all
# three as identical horizontal segments at the same x, so where the three
# coincide they were painted on top of one another and only the last drawn
# (Highest) survived.
#
# They coincide exactly at the score groups reachable by a single health state,
# which are the most interpretable points on either plot: LSS 5 (11111) and 15
# (33333) for the EQ-5D-3L, LSS 5 and 25 for the 5L; LFS 500, 050 and 005 for
# the 3L. The median vanished at precisely those points.
#
# The range is now a vertical line with a short horizontal tick at each end,
# and the median a point drawn last, so it is never hidden. Shape as well as
# colour separates the median from the two range marks.

dims <- c("mo", "sc", "ua", "pd", "ad")

# Both plots take the EQ-5D values from a column; the score groups still come
# from the dimensions.
valued <- function(df) {
  df$value <- suppressWarnings(suppressMessages(
    eq5d3l(df[, dims], country = "GB")))
  df
}

lss_plot <- function(df = example_data, ...) {
  suppressWarnings(suppressMessages(eq5d_profile_lss_utility_plot(
    valued(df), names_eq5d = dims, name_utility = "value",
    eq5d_version = "3L", ...)))
}
lfs_plot <- function(df = example_data, ...) {
  suppressWarnings(suppressMessages(eq5d_profile_lfs_utility_plot(
    valued(df), names_eq5d = dims, name_utility = "value",
    eq5d_version = "3L", ...)))
}

geoms_of <- function(p)
  unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))

# ---------------------------------------------------------------------------
# The groups where the three coincide
# ---------------------------------------------------------------------------

test_that("the LSS plot has groups where median, lowest and highest coincide", {
  # The premise: without them there is nothing to fix.
  pd <- lss_plot()$plot_data
  zero <- pd$Maximum - pd$Minimum == 0
  expect_true(any(zero))
  expect_setequal(pd$LSS[zero], c(5, 15))
  expect_equal(pd$Median[pd$LSS == 5], 1)
  expect_equal(pd$Median[pd$LSS == 15], -0.594, tolerance = 1e-6)
})

test_that("the LFS plot has groups where median, lowest and highest coincide", {
  pd <- lfs_plot()$plot_data
  zero <- pd$Maximum - pd$Minimum == 0
  expect_setequal(pd$LFS[zero], c("500", "050", "005"))
})

# ---------------------------------------------------------------------------
# The median is drawn last
# ---------------------------------------------------------------------------

test_that("both plots draw the range first and the median last", {
  for (p in list(lss_plot()$p, lfs_plot()$p)) {
    expect_identical(geoms_of(p),
                     c("GeomSegment", "GeomSegment", "GeomPoint"))
    # A point layer last is what keeps the median visible; it is the whole fix.
    expect_identical(utils::tail(geoms_of(p), 1L), "GeomPoint")
  }
})

test_that("the median point is rendered at the coinciding scores", {
  p <- lss_plot()$p
  built <- ggplot2::ggplot_build(p)
  # The last layer's rendered data is the median, one point per LSS group.
  med <- built$data[[length(built$data)]]
  pd  <- lss_plot()$plot_data

  expect_identical(nrow(med), nrow(pd))
  expect_equal(sort(med$y), sort(pd$Median))
  # Including LSS 5 and 15, where nothing used to be visible but the range.
  expect_true(any(abs(med$y - 1) < 1e-8))
  expect_true(any(abs(med$y - (-0.594)) < 1e-6))
})

# ---------------------------------------------------------------------------
# The legend
# ---------------------------------------------------------------------------

test_that("the legend keeps all three series, by shape as well as colour", {
  for (r in list(lss_plot(), lfs_plot())) {
    built <- ggplot2::ggplot_build(r$p)
    lvl <- c("Median", "Lowest", "Highest")

    col <- built$plot$scales$get_scales("colour")
    shp <- built$plot$scales$get_scales("shape")
    expect_false(is.null(col))
    expect_false(is.null(shp))

    # Every series appears in both scales, in the same order, so the two
    # legends merge into one.
    expect_identical(col$get_limits(), lvl)
    expect_identical(shp$get_limits(), lvl)

    # The median is a filled circle; the range marks are horizontal dashes,
    # matching the ticks drawn in the panel.
    expect_identical(unname(shp$palette.cache %||% shp$map(lvl)), c(16, 45, 45))
    # Distinct colours, so the three are told apart in colour too.
    expect_identical(length(unique(col$map(lvl))), 3L)
  }
})

test_that("the shape scale alone separates the median from the range marks", {
  # Shape is what carries the distinction in greyscale.
  shp <- ggplot2::ggplot_build(lss_plot()$p)$plot$scales$get_scales("shape")
  mapped <- shp$map(c("Median", "Lowest", "Highest"))
  expect_false(mapped[1] == mapped[2])
  expect_false(mapped[1] == mapped[3])
})

# ---------------------------------------------------------------------------
# Nothing else about the functions changed
# ---------------------------------------------------------------------------

test_that("plot_data is unchanged in shape and content", {
  r <- lss_plot()
  expect_identical(names(r$plot_data), c("Median", "Minimum", "Maximum", "LSS"))
  expect_identical(r$plot_data$LSS, 5:15)
  expect_true(all(r$plot_data$Minimum <= r$plot_data$Median))
  expect_true(all(r$plot_data$Median  <= r$plot_data$Maximum))

  expect_identical(names(lfs_plot()$plot_data),
                   c("Median", "Minimum", "Maximum", "LFS"))
})

test_that("the returned plot is still a ggplot that can be customised", {
  for (r in list(lss_plot(), lfs_plot())) {
    expect_s3_class(r$p, "ggplot")

    p2 <- r$p +
      ggplot2::labs(title = "Custom title", subtitle = "A subtitle") +
      ggplot2::theme(legend.position = "right") +
      ggplot2::coord_cartesian(ylim = c(-1, 1))

    expect_s3_class(p2, "ggplot")
    # It builds, which is the real test that the layers still agree.
    expect_no_error(ggplot2::ggplot_build(p2))
    expect_identical(geoms_of(p2), geoms_of(r$p))
  }
})

test_that("the EQ-5D-5L path works too", {
  set.seed(1)
  d5 <- as.data.frame(replicate(5, sample(1:5, 400, TRUE), simplify = FALSE))
  names(d5) <- dims

  d5$value <- suppressWarnings(suppressMessages(
    eq5d5l(d5[, dims], country = "GB")))
  r <- suppressWarnings(suppressMessages(eq5d_profile_lss_utility_plot(
    d5, names_eq5d = dims, name_utility = "value", eq5d_version = "5L")))

  expect_identical(utils::tail(geoms_of(r$p), 1L), "GeomPoint")
  expect_true(min(r$plot_data$LSS) >= 5)
  expect_true(max(r$plot_data$LSS) <= 25)
})
