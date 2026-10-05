# The Word report the Export page offers: one section per saved result, in the
# order the Results page put them, rendered through the bundled template.

skip_unless_pandoc <- function() {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

q <- function(expr) suppressWarnings(suppressMessages(expr))
dims <- c("mo", "sc", "ua", "pd", "ad")

a_table <- function(label) {
  list(id = label, timestamp = Sys.time(), label = label, fn_call = "f()",
       result_type = "table",
       data = data.frame(group = c("x", "y"), value = c(0.123456, 0.987654)),
       plot = NULL)
}

a_plot <- function(label) {
  d <- head(example_data, 200L)
  d$value <- q(eq5d3l(d[, dims], country = "GB"))
  list(id = label, timestamp = Sys.time(), label = label, fn_call = "f()",
       result_type = "plot", data = NULL,
       plot = q(eq5d_utility_distribution_plot(d, name_utility = "value"))$p)
}

# Read back the document: its headings in order, and how many tables and
# figures it holds.
read_docx_parts <- function(path) {
  dir <- tempfile("unz")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = dir)
  xml <- paste(readLines(file.path(dir, "word", "document.xml"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "")

  paras <- regmatches(xml, gregexpr("<w:p[ >].*?</w:p>", xml))[[1]]
  headings <- character(0)
  for (p in paras) {
    if (!grepl('w:pStyle w:val="Heading2"', p)) next
    txt <- regmatches(p, gregexpr("<w:t[^>]*>[^<]*</w:t>", p))[[1]]
    txt <- gsub("<[^>]*>", "", txt)
    headings <- c(headings, paste(txt, collapse = ""))
  }
  tbls <- regmatches(xml, gregexpr("<w:tbl>.*?</w:tbl>", xml))[[1]]
  sects <- regmatches(xml, gregexpr("<w:sectPr[ >].*?</w:sectPr>", xml))[[1]]
  list(headings = headings,
       tables = tbls,
       sections = sects,
       n_tables = lengths(regmatches(xml, gregexpr("<w:tbl>", xml))),
       n_figures = lengths(regmatches(xml, gregexpr("<w:drawing>", xml))),
       styles = unique(unlist(regmatches(
         xml, gregexpr('(?<=w:pStyle w:val=")[^"]+', xml, perl = TRUE)))),
       files = utils::unzip(path, list = TRUE)$Name)
}

test_that("the report has one section per result, in the order given", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")

  eq5dsuite:::write_results_docx(
    list(a_table("Third thing"), a_plot("First thing"), a_table("Second thing")),
    f)

  expect_true(file.exists(f))
  parts <- read_docx_parts(f)
  expect_identical(parts$headings,
                   c("Third thing", "First thing", "Second thing"))
  expect_equal(parts$n_tables, 2L)
  expect_equal(parts$n_figures, 1L)
})

test_that("reordering the results reorders the report", {
  skip_unless_pandoc()
  res <- list(a_table("A"), a_table("B"), a_table("C"))

  f1 <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(res, f1)
  expect_identical(read_docx_parts(f1)$headings, c("A", "B", "C"))

  # Exactly what move_result() does to rv$results.
  res[c(1, 3)] <- res[c(3, 1)]
  f2 <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(res, f2)
  expect_identical(read_docx_parts(f2)$headings, c("C", "B", "A"))
})

test_that("a removed result is absent from the report", {
  skip_unless_pandoc()
  res <- list(a_table("Keep one"), a_table("Drop me"), a_table("Keep two"))
  f <- withr::local_tempfile(fileext = ".docx")

  eq5dsuite:::write_results_docx(res[-2L], f)

  parts <- read_docx_parts(f)
  expect_identical(parts$headings, c("Keep one", "Keep two"))
  expect_false(any(grepl("Drop me", parts$headings)))
  expect_equal(parts$n_tables, 2L)
})

test_that("the report takes the bundled template's styles and furniture", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("Only")), f)

  parts <- read_docx_parts(f)
  # Styles that exist only because the template defines them.
  expect_true(all(c("Title", "Heading2", "Compact") %in% parts$styles))
  # The template's header and footer, and the footer's logo, come along.
  expect_true(any(grepl("word/header1.xml", parts$files, fixed = TRUE)))
  expect_true(any(grepl("word/footer1.xml", parts$files, fixed = TRUE)))
  expect_true(any(grepl("^word/media/", parts$files)))
  # The template's own text does not.
  xml <- paste(readLines(unz(f, "word/document.xml"), warn = FALSE), collapse = "")
  expect_false(grepl("tralokinumab", xml, fixed = TRUE))
})

test_that("the template ships with the package", {
  path <- system.file("shiny", "template.docx", package = "eq5dsuite")
  expect_true(nzchar(path))
  expect_gt(file.size(path), 1000)
  expect_true(any(grepl("word/styles.xml",
                        utils::unzip(path, list = TRUE)$Name, fixed = TRUE)))
})

test_that("an empty result list is refused rather than making an empty file", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(eq5dsuite:::write_results_docx(list(), f), "no results")
  expect_false(file.exists(f))
})

test_that("a result carrying neither table nor figure is noted, not dropped", {
  skip_unless_pandoc()
  empty <- list(id = "e", timestamp = Sys.time(), label = "Nothing here",
                fn_call = "f()", result_type = "table", data = NULL, plot = NULL)
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("Real"), empty), f)
  expect_identical(read_docx_parts(f)$headings, c("Real", "Nothing here"))
})


# ---------------------------------------------------------------------------
# How the tables are formatted
# ---------------------------------------------------------------------------

wide_table <- function(n_cols = 21L) {
  d <- as.data.frame(matrix(seq_len(3L * n_cols), nrow = 3L))
  names(d) <- c("level", paste0(rep(c("n", "freq"), length.out = n_cols - 1L),
                                "_Pre-op_", seq_len(n_cols - 1L)))
  list(id = "w", timestamp = Sys.time(), label = "Wide", fn_call = "f()",
       result_type = "table", data = d, plot = NULL)
}

test_that("headings lose the underscores Word cannot break", {
  expect_equal(
    eq5dsuite:::tidy_table_headings(
      c("level", "n_All_mo", "freq_Post-op_mo", "Knee Replacement_Post-op_n",
        "Groin Hernia_Post-op_p", "mo_% Total", "Cumulative percentage")),
    c("level", "mo All n", "mo Post-op %", "Knee Replacement Post-op n",
      "Groin Hernia Post-op %", "mo % Total", "Cumulative percentage"))
  # Nothing is left with an underscore in it.
  expect_false(any(grepl("_", eq5dsuite:::tidy_table_headings(
    c("n_All_mo", "a_b_c_d", "x")), fixed = TRUE)))
})

test_that("the document's tables are bordered and sized to their contents", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("Small")), f)

  parts <- read_docx_parts(f)
  expect_length(parts$tables, 1L)
  # "Table", the style pandoc reaches for, has no borders in the template.
  expect_match(parts$tables[[1]], 'w:tblStyle w:val="TableGrid"')
  expect_false(grepl('w:tblStyle w:val="Table"', parts$tables[[1]], fixed = TRUE))
  # Pandoc's fixed widths, taken from the Markdown source, are gone.
  expect_false(grepl('<w:gridCol w:w="', parts$tables[[1]], fixed = TRUE))
  expect_match(parts$tables[[1]], 'w:tblLayout w:type="autofit"')
})

test_that("a wide table is set smaller, a narrow one is not", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("Narrow"), wide_table(11L)), f)

  tbls <- read_docx_parts(f)$tables
  expect_length(tbls, 2L)
  expect_false(grepl("<w:sz ", tbls[[1]], fixed = TRUE))      # 3 columns
  expect_match(tbls[[2]], '<w:sz w:val="16"')                  # 11 columns, 8pt
})

test_that("a very wide table gets a landscape page of its own", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("Before"), wide_table(21L)), f)

  parts <- read_docx_parts(f)
  # Three sections: portrait up to the wide table, landscape for it, then the
  # document's own portrait section for the rest.
  expect_length(parts$sections, 3L)
  orient <- vapply(parts$sections, function(sp)
    if (grepl('w:orient="landscape"', sp, fixed = TRUE)) "landscape" else "portrait",
    character(1L))
  expect_equal(unname(orient), c("portrait", "landscape", "portrait"))

  # Every section keeps the template's header, footer and margins, or they
  # would be lost from the pages after the break.
  for (sp in parts$sections) {
    expect_match(sp, "<w:headerReference")
    expect_match(sp, "<w:footerReference")
    expect_match(sp, "<w:pgMar")
  }
})

test_that("a document of narrow tables stays portrait throughout", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("One"), a_table("Two")), f)

  parts <- read_docx_parts(f)
  expect_length(parts$sections, 1L)      # just the document's own
  expect_false(any(grepl('w:orient="landscape"', parts$sections, fixed = TRUE)))
})

test_that("restyling an already-styled document changes nothing further", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("One"), wide_table(21L)), f)
  before <- read_docx_parts(f)

  # Idempotent: no second landscape break, no doubled font properties.
  eq5dsuite:::style_docx_tables(f)
  after <- read_docx_parts(f)
  expect_length(after$sections, length(before$sections))
  expect_equal(after$headings, before$headings)
  expect_equal(lengths(regmatches(after$tables[[2]],
                                  gregexpr("<w:sz ", after$tables[[2]]))),
               lengths(regmatches(before$tables[[2]],
                                  gregexpr("<w:sz ", before$tables[[2]]))))
})

test_that("restyling leaves a valid archive with the template's furniture", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("One"), wide_table(21L),
                                      a_plot("A figure")), f)

  parts <- read_docx_parts(f)
  # The figure survives the rewrite, as do the styles and the furniture.
  expect_equal(parts$n_figures, 1L)
  expect_true(any(grepl("word/styles.xml", parts$files, fixed = TRUE)))
  expect_true(any(grepl("word/header1.xml", parts$files, fixed = TRUE)))
  expect_true(any(grepl("word/footer1.xml", parts$files, fixed = TRUE)))
  expect_true(any(grepl("^word/media/", parts$files)))
  # And the result is still a readable zip.
  expect_silent(utils::unzip(f, list = TRUE))
})

test_that("numbers are right-aligned and labels left", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("Aligned")), f)
  tbl <- read_docx_parts(f)$tables[[1]]
  expect_match(tbl, 'w:jc w:val="right"')
  expect_match(tbl, 'w:jc w:val="left"')
})

test_that("writing the report leaves the working directory alone", {
  skip_unless_pandoc()
  f <- withr::local_tempfile(fileext = ".docx")
  before <- getwd()

  # The process's working directory is shared by every Shiny session, so
  # changing it even briefly is not safe on a server.
  eq5dsuite:::write_results_docx(list(a_table("One"), wide_table(21L)), f)

  expect_identical(getwd(), before)
  expect_true(file.exists(f))
  expect_silent(utils::unzip(f, list = TRUE))
})

test_that("no package code changes the working directory", {
  # The sources, not the installed package: R/ there is a lazy-load database,
  # which parse() cannot read. Under R CMD check the sources are absent, so
  # this skips.
  src <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(src), "package sources not available")
  files <- list.files(src, pattern = "\\.R$", full.names = TRUE)
  skip_if(!length(files), "package sources not available")

  offenders <- character(0L)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    if (any(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "setwd"))
      offenders <- c(offenders, basename(f))
  }
  expect_identical(offenders, character(0L))
})

test_that("two reports written in the same second do not collide", {
  skip_unless_pandoc()
  # The directory name used to come from the clock in whole seconds, so two
  # reports begun together shared one and the first to finish deleted it.
  before <- list.files(tempdir())

  f1 <- withr::local_tempfile(fileext = ".docx")
  f2 <- withr::local_tempfile(fileext = ".docx")
  eq5dsuite:::write_results_docx(list(a_table("First")), f1)
  eq5dsuite:::write_results_docx(list(a_table("Second")), f2)

  expect_identical(read_docx_parts(f1)$headings, "First")
  expect_identical(read_docx_parts(f2)$headings, "Second")

  # And neither left its working directory behind.
  left <- setdiff(list.files(tempdir()), before)
  expect_false(any(grepl("^eq5ddocx", left)))
})
