# The bulk ZIP export keeps every saved result.
#
# Files were named from the result's label alone, and the app gives every run
# of an analysis the same label, so a second run overwrote the first in the
# staging folder: two saved tables with values 1 and 2 produced one CSV
# holding 2. A plot that failed to render was dropped without a word.

# Download the archive through the real handler and unpack it.
download_zip <- function(e, results) {
  rv <- shiny::reactiveValues(results = results, steps = list())
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    utils::unzip(output$download_all_zip, exdir = dir)
  })
  dir
}

manifest_of <- function(dir)
  utils::read.csv(file.path(dir, "manifest.csv"), stringsAsFactors = FALSE,
                  check.names = FALSE)

value_in <- function(dir, file) utils::read.csv(file.path(dir, file))$x

test_that("repeated runs of one analysis each keep their own file", {
  skip_unless_app()
  e <- app_env()
  for (n in 2:3) {
    res <- fake_results(rep("Utility summary stats (3.1)", n))
    dir <- download_zip(e, res)
    m <- manifest_of(dir)
    csvs <- m$file[grepl("\\.csv$", m$file)]
    expect_length(csvs, n)
    expect_false(anyDuplicated(csvs) > 0L)
    expect_true(all(file.exists(file.path(dir, csvs))))
    expect_identical(vapply(csvs, value_in, numeric(1L), dir = dir,
                            USE.NAMES = FALSE), as.numeric(seq_len(n)))
  }
})

test_that("labels that sanitise to the same name do not collide", {
  skip_unless_app()
  e <- app_env()
  dir <- download_zip(e, fake_results(c("A/B", "A?B", "A B")))
  m <- manifest_of(dir)
  expect_identical(m$label, c("A/B", "A?B", "A B"))
  expect_identical(vapply(m$file, value_in, numeric(1L), dir = dir,
                          USE.NAMES = FALSE), c(1, 2, 3))
})

test_that("files follow the order the user gave the results", {
  skip_unless_app()
  e <- app_env()
  res <- rev(fake_results(c("Same", "Same", "Same")))
  dir <- download_zip(e, res)
  m <- manifest_of(dir)
  # The first file is the first result in the user's order: the one that was
  # saved last, with value 3.
  expect_identical(m$position, 1:3)
  expect_identical(vapply(m$file, value_in, numeric(1L), dir = dir,
                          USE.NAMES = FALSE), c(3, 2, 1))
  expect_true(all(startsWith(m$file, sprintf("%02d_", 1:3))))
})

test_that("tables and plots are both exported, and the manifest links them to their calls", {
  skip_unless_app()
  e <- app_env()
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                       ggplot2::aes(.data$x, .data$y)) + ggplot2::geom_point()
  res <- c(fake_results("A table"),
           list(list(id = "rp", timestamp = Sys.time(), label = "A table",
                     fn_call = "eq5d_profile_lss_utility_plot(df = analysis_data)",
                     result_type = "plot", data = NULL, plot = p,
                     call = list(fn = "eq5d_profile_lss_utility_plot",
                                 type = "plot"))))
  dir <- download_zip(e, res)
  m <- manifest_of(dir)
  expect_identical(nrow(m), 2L)
  expect_identical(m$type, c("table", "plot"))
  expect_match(m$file[2], "^02_.*\\.png$")
  expect_gt(file.size(file.path(dir, m$file[2])), 0)
  expect_identical(m$status, c("ok", "ok"))
  expect_identical(m$call[2], "eq5d_profile_lss_utility_plot(df = analysis_data)")
  # The name the generated script gives the same result.
  expect_identical(m$script_object[2], "profile_lss_utility_plot")
})

test_that("a plot that cannot be drawn is reported, not silently dropped", {
  skip_unless_app()
  e <- app_env()
  bad <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(.data$nope, .data$x)) +
    ggplot2::geom_point()
  res <- c(fake_results("Fine"),
           list(list(id = "rb", timestamp = Sys.time(), label = "Broken",
                     fn_call = "f()", result_type = "plot", data = NULL,
                     plot = bad)))
  dir <- download_zip(e, res)
  m <- manifest_of(dir)
  expect_identical(nrow(m), 2L)
  expect_identical(m$status[1], "ok")
  expect_match(m$status[2], "^failed: ")
  expect_false(file.exists(file.path(dir, m$file[2])))
  expect_true(file.exists(file.path(dir, m$file[1])))
})

test_that("individual downloads are named as in the archive, so repeats differ", {
  skip_unless_app()
  e <- app_env()
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                       ggplot2::aes(.data$x, .data$y)) + ggplot2::geom_point()
  plot_result <- function(id) list(
    id = id, timestamp = Sys.time(), label = "Same plot", fn_call = "f()",
    result_type = "plot", data = NULL, plot = p)
  res <- c(fake_results(rep("Utility summary stats (3.1)", 2)),
           list(plot_result("p1"), plot_result("p2")))
  rv <- shiny::reactiveValues(results = res, steps = list())
  dir <- download_zip(e, res)
  in_zip <- manifest_of(dir)$file

  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    csv <- c(basename(output$dl_csv_r1), basename(output$dl_csv_r2))
    png <- c(basename(output$dl_png_p1), basename(output$dl_png_p2))
    pdf <- c(basename(output$dl_pdf_p1), basename(output$dl_pdf_p2))
    expect_identical(csv, c("01_Utility_summary_stats__3_1_.csv",
                            "02_Utility_summary_stats__3_1_.csv"))
    expect_identical(png, c("03_Same_plot.png", "04_Same_plot.png"))
    expect_identical(pdf, c("03_Same_plot.pdf", "04_Same_plot.pdf"))
    # The same names the bulk archive gives the same results.
    expect_true(all(c(csv, png) %in% in_zip))
    # And the content is each result's own.
    expect_identical(utils::read.csv(output$dl_csv_r2)$x, 2L)
  })
})

