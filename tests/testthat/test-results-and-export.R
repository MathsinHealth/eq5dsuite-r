# One "Results and export" page.
#
# Each saved result has move up / move down / remove on the right, its own
# downloads and "View" below its title, and expands inline. Every control is
# keyed by the result's id, so reordering or removing results keeps every
# button, preview and export with its own result, and the list order is the
# order of the Word report, the archive and the R script.

q <- function(expr) suppressWarnings(suppressMessages(expr))

with_plot <- function(labels) {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                       ggplot2::aes(.data$x, .data$y)) + ggplot2::geom_point()
  res <- fake_results(labels)
  res[[2]]$result_type <- "plot"; res[[2]]$data <- NULL; res[[2]]$plot <- p
  res
}

labels_in <- function(html, labels) {
  pos <- vapply(labels, function(l)
    regexpr(paste0("<strong>", l, "</strong>"), html, fixed = TRUE), 1L)
  names(sort(pos[pos > 0]))
}

test_that("Results and Export are one page", {
  skip_unless_app()
  ui <- paste(readLines(file.path(app_dir(), "ui.R"), warn = FALSE),
              collapse = "\n")
  expect_match(ui, '"Results and export"', fixed = TRUE)
  expect_false(grepl('"Export",', ui, fixed = TRUE))
  expect_false(grepl("mod_results_ui", ui, fixed = TRUE))
  expect_false(file.exists(file.path(app_dir(), "modules", "mod_results.R")))
})

test_that("each entry has its controls, downloads and View, keyed by its id", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = with_plot(c("A", "B", "C")),
                              steps = list())
  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    html <- as.character(output$export_list$html)
    for (id in c("r1", "r3")) {
      expect_match(html, paste0("up_", id), fixed = TRUE)
      expect_match(html, paste0("down_", id), fixed = TRUE)
      expect_match(html, paste0("rm_", id), fixed = TRUE)
      expect_match(html, paste0("view_", id), fixed = TRUE)
      expect_match(html, paste0("dl_csv_", id), fixed = TRUE)
    }
    # A figure offers PNG and PDF, not CSV.
    expect_match(html, "dl_png_r2", fixed = TRUE)
    expect_match(html, "dl_pdf_r2", fixed = TRUE)
    expect_false(grepl("dl_csv_r2", html, fixed = TRUE))
    # Downloads and View sit under the title; the controls on the right.
    row <- regmatches(html, regexpr("result-row-head.*?result-row-controls", html))
    expect_match(row, "result-row-actions", fixed = TRUE)
    # The first cannot move up, the last cannot move down.
    button <- function(id)
      regmatches(html, regexpr(paste0('<button[^>]*-', id, '"[^>]*>'), html))
    expect_match(button("up_r1"), "disabled", fixed = TRUE)
    expect_match(button("down_r3"), "disabled", fixed = TRUE)
    expect_false(grepl("disabled", button("up_r2"), fixed = TRUE))
    # Collapsed, nothing is previewed.
    expect_false(grepl("result-preview", html, fixed = TRUE))
  })
})

test_that("View shows the result inline, and hides it again", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = with_plot(c("A", "B", "C")),
                              steps = list())
  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    session$setInputs(view_r1 = 1)
    html <- as.character(output$export_list$html)
    expect_match(html, "result-preview", fixed = TRUE)
    expect_match(html, "tbl_r1", fixed = TRUE)
    expect_match(html, "call_r1", fixed = TRUE)
    expect_false(is.null(output$tbl_r1))
    expect_identical(output$call_r1, "f()")
    # A figure previews as a plot.
    session$setInputs(view_r2 = 1)
    html <- as.character(output$export_list$html)
    expect_match(html, "plot_r2", fixed = TRUE)
    expect_false(is.null(output$plot_r2))
    # Again: hidden.
    session$setInputs(view_r1 = 2)
    html <- as.character(output$export_list$html)
    expect_false(grepl("tbl_r1", html, fixed = TRUE))
    expect_match(html, "plot_r2", fixed = TRUE)
  })
})

test_that("moving and removing keep every control, file and export in step", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B", "C")),
                              steps = list())
  dir <- withr::local_tempdir()
  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    session$setInputs(view_r3 = 1)
    # C up, twice: C, A, B.
    session$setInputs(up_r3 = 1)
    session$setInputs(up_r3 = 2)
    expect_identical(vapply(rv$results, `[[`, "", "label"), c("C", "A", "B"))
    html <- as.character(output$export_list$html)
    expect_identical(labels_in(html, c("A", "B", "C")), c("C", "A", "B"))
    # C's preview stayed open, under C.
    expect_match(html, "tbl_r3", fixed = TRUE)
    # Each result's own file, named by its new position.
    expect_identical(basename(output$dl_csv_r3), "01_C.csv")
    expect_identical(basename(output$dl_csv_r1), "02_A.csv")
    expect_identical(utils::read.csv(output$dl_csv_r3)$x, 3L)
    # The archive and the script follow the same order.
    utils::unzip(output$download_all_zip, exdir = dir)
    m <- utils::read.csv(file.path(dir, "manifest.csv"))
    expect_identical(m$label, c("C", "A", "B"))
    # A down: C, B, A.
    session$setInputs(down_r1 = 1)
    expect_identical(vapply(rv$results, `[[`, "", "label"), c("C", "B", "A"))
    expect_identical(basename(output$dl_csv_r1), "03_A.csv")
    # Remove C: its preview goes with it, B and A move up.
    session$setInputs(rm_r3 = 1)
    html <- as.character(output$export_list$html)
    expect_identical(labels_in(html, c("A", "B", "C")), c("B", "A"))
    expect_false(grepl("tbl_r3", html, fixed = TRUE))
    expect_identical(basename(output$dl_csv_r2), "01_B.csv")
  })
})

test_that("the R script follows the list after moves and removals", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  for (o in c("111", "141")) {
    rv[["t_o"]] <- o
    q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
      session$setInputs(component = "profile")
      session$setInputs(output = shiny::isolate(rv$t_o))
      session$setInputs(run = 1)
    }))
  }
  ids <- vapply(shiny::isolate(rv$results), `[[`, "", "id")
  rv[["t_ids"]] <- ids
  rv$steps <- list()
  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    ids <- shiny::isolate(rv$t_ids)
    do.call(session$setInputs, stats::setNames(list(1), paste0("up_", ids[2])))
    lines <- strsplit(output$script_preview, "\n")[[1]]
    a <- grep("eq5d_profile_shannon\\(", lines)[1]
    b <- grep("eq5d_profile_level_summary\\(", lines)[1]
    expect_lt(a, b)
    do.call(session$setInputs, stats::setNames(list(1), paste0("rm_", ids[2])))
    lines <- strsplit(output$script_preview, "\n")[[1]]
    expect_false(any(grepl("eq5d_profile_shannon\\(", lines)))
  })
})

test_that("results saved together still get different ids", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = list())
  shiny::isolate({
    for (i in 1:50) e$save_result(rv, "Same", "f()", "table",
                                  data = data.frame(x = i))
  })
  ids <- vapply(shiny::isolate(rv$results), `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0L)
  # Usable in an input id.
  expect_true(all(grepl("^[A-Za-z0-9_]+$", ids)))
})
