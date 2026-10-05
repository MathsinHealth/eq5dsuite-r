# The catalogue of available analyses on the Analysis page.
#
# It lists only the analyses the confirmed data can support -- checked
# against the data, not just against which columns are selected -- with a
# plain title and description, and "Select this" selects one in the sidebar
# without running it.

q <- function(expr) suppressWarnings(suppressMessages(expr))

ids <- function(specs) vapply(specs, `[[`, "", "id")

# Outside a session, so isolate(); the app calls it from an observer.
avail <- function(e, rv) shiny::isolate(e$available_analyses(rv))

# A processed session on `d`, with the variables selected by `mapping`.
rv_for <- function(e, d, mapping) {
  shiny::reactiveValues(raw_data = d, mapping = mapping,
                        processed_data = eq5d_apply_mapping(d, mapping),
                        results = list(), value_cols = character(0),
                        steps = list())
}

map_of <- function(...) {
  m <- list(eq5d_version = "3L", names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
            name_fu = "time", levels_fu = c("Pre-op", "Post-op"),
            name_id = "id", name_vas = "vas", name_groupvar = "procedure")
  modifyList(m, list(...))
}

LONGITUDINAL <- c("121", "122", "123", "124", "121fig", "122fig", "123fig",
                  "124fig", "125fig")
PAIRED <- c("122", "123", "124", "121fig", "122fig", "123fig", "124fig",
            "125fig")

test_that("every analysis in the registry has a plain title and description", {
  skip_unless_app()
  e <- app_env()
  reg <- ids(e$ANALYSES)
  expect_setequal(names(e$CATALOGUE), reg)
  for (id in reg) {
    c <- e$CATALOGUE[[id]]
    expect_true(nzchar(c$title) && nzchar(c$about), info = id)
    # An abbreviation in a title comes with its expansion.
    for (ab in c("PCHC", "LSS", "LFS", "HPG", "HSDI", "CI", "VAS"))
      if (grepl(paste0("\\b", ab, "\\b"), c$title))
        expect_match(c$title, paste0("\\(", "[^)]*\\b", ab, "\\b"), info = id)
  }
})

test_that("everything is available on example_data with a value column", {
  skip_unless_app()
  e <- app_env()
  expect_setequal(ids(avail(e, processed_rv(e))), ids(e$ANALYSES))
})

test_that("nothing is available before the data are validated", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  rv$processed_data <- NULL
  expect_length(avail(e, rv), 0L)
})

test_that("no timepoint: no longitudinal analysis", {
  skip_unless_app()
  e <- app_env()
  m <- map_of(); m$name_fu <- NULL; m$levels_fu <- NULL
  got <- ids(avail(e, rv_for(e, example_data, m)))
  expect_false(any(LONGITUDINAL %in% got))
  expect_true(all(c("111", "112", "113", "141") %in% got))
})

test_that("a Timepoint column with one timepoint is not enough for change", {
  skip_unless_app()
  e <- app_env()
  pre <- example_data[example_data$time == "Pre-op", ]
  got <- ids(avail(e, rv_for(e, pre, map_of(levels_fu = "Pre-op"))))
  # The column is selected, so unmet_needs() alone would allow these.
  expect_false(any(LONGITUDINAL %in% got))
  expect_true("111" %in% got)
})

test_that("two timepoints but nobody seen at both: no paired analysis", {
  skip_unless_app()
  e <- app_env()
  d <- example_data
  d$id <- seq_len(nrow(d))          # every record a different respondent
  got <- ids(avail(e, rv_for(e, d, map_of())))
  expect_false(any(PAIRED %in% got))
  expect_true("121" %in% got)       # frequencies at each timepoint still work
})

test_that("EQ VAS analyses need EQ VAS scores in range", {
  skip_unless_app()
  e <- app_env()
  d <- example_data
  d$vas <- 999
  got <- ids(avail(e, rv_for(e, d, map_of())))
  expect_false(any(c("21", "22", "fig21", "fig22") %in% got))
})

test_that("EQ-5D value analyses need values, and appear once there are some", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e, with_value = FALSE)
  values <- c("131", "131fig", "134", "132fig", "31", "fig34", "32", "fig32",
              "fig31", "fig33", "fig35")
  expect_false(any(values %in% ids(avail(e, rv))))
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility_GB", add = 1)
  }))
  expect_true(all(values %in% ids(avail(e, rv))))
})

test_that("the catalogue shows exactly the available analyses", {
  skip_unless_app()
  e <- app_env()
  m <- map_of(); m$name_fu <- NULL; m$levels_fu <- NULL
  rv <- rv_for(e, example_data, m)
  shown <- NULL
  local_mocked_bindings(showModal = function(ui, ...) shown <<- ui,
                        .package = "shiny")
  shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(catalogue = 1)
  })
  html <- as.character(shown)
  specs <- avail(e, rv)
  expect_identical(lengths(regmatches(html, gregexpr(">Select this<", html))),
                   length(specs))
  for (s in specs) expect_match(html, e$CATALOGUE[[s$id]]$title, fixed = TRUE)
  expect_false(grepl("Paretian", html, fixed = TRUE))   # no timepoint
  expect_match(html, "Analyses available for your data", fixed = TRUE)
})

test_that("Select this selects the analysis, closes the catalogue, runs nothing", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  updates <- list(); closed <- 0L
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, label = NULL,
                                 choices = NULL, selected = NULL)
      updates[[length(updates) + 1L]] <<- list(id = inputId, selected = selected),
    removeModal = function(...) closed <<- closed + 1L,
    .package = "shiny")
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "profile", output = "111")
    updates <<- list()
    # Same component: the output is selected directly.
    session$setInputs(catalogue_pick = "122")
    expect_identical(updates[[1]], list(id = "output", selected = "122"))
    # Another component: switch it, then select the output once it is offered.
    updates <<- list()
    session$setInputs(catalogue_pick = "21")
    expect_identical(updates[[1]], list(id = "component", selected = "vas"))
    session$setInputs(component = "vas")
    out <- Filter(function(u) identical(u$id, "output"), updates)
    expect_identical(out[[length(out)]]$selected, "21")
  }))
  expect_identical(closed, 2L)
  expect_length(shiny::isolate(rv$results), 0L)
})

# ── Tabs and examples ─────────────────────────────────────────────────────────

# The catalogue modal for `rv`, as HTML, opened through the module.
catalogue_html <- function(e, rv) {
  shown <- NULL
  local_mocked_bindings(showModal = function(ui, ...) shown <<- ui,
                        .package = "shiny")
  shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(catalogue = 1)
  })
  as.character(shown)
}

# The HTML of one tab pane, by its component value.
pane <- function(html, comp) {
  starts <- gregexpr('<div class="tab-pane', html)[[1]]
  s <- regexpr(paste0('<div class="tab-pane[^>]*data-value="', comp, '"'), html)
  expect_gt(s, 0L)
  nxt <- starts[starts > s]
  substr(html, s, if (length(nxt)) min(nxt) - 1L else nchar(html))
}

test_that("every analysis has a shipped example, and the tables are current", {
  skip_unless_app()
  e <- app_env()
  dir <- e$catalogue_dir()
  expect_true(nzchar(dir))
  rv <- e$catalogue_example_rv()
  for (s in e$ANALYSES) {
    if (identical(s$type, "plot")) {
      f <- file.path(dir, paste0(s$id, ".png"))
      expect_true(file.exists(f), info = s$id)
      sig <- readBin(f, "raw", 8L)
      expect_identical(sig, as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
                       info = s$id)
    } else {
      f <- file.path(dir, paste0(s$id, ".rds"))
      expect_true(file.exists(f), info = s$id)
      # Rebuilt now from the registry and example_data: the same table.
      expect_equal(readRDS(f), e$catalogue_example_output(s, rv),
                   tolerance = 1e-12, info = s$id)
    }
  }
})

test_that("the catalogue has three tabs, each with only its own analyses", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  html <- catalogue_html(e, rv)
  for (lab in c("EQ-5D profiles", "EQ-5D values", "EQ VAS"))
    expect_match(html, paste0('data-bs-toggle="tab"[^>]*>\\s*', lab,
                              '\\s*<span class="catalogue-count">'))
  avail <- avail(e, rv)
  for (comp in c("profile", "values", "vas")) {
    p <- pane(html, comp)
    mine <- Filter(function(s) identical(s$component, comp), avail)
    expect_identical(lengths(regmatches(p, gregexpr(">Select this<", p))),
                     length(mine), info = comp)
    for (s in mine) expect_match(p, e$CATALOGUE[[s$id]]$title, fixed = TRUE)
  }
  # Without a timepoint the profile tab drops the change analyses.
  m <- map_of(); m$name_fu <- NULL; m$levels_fu <- NULL
  p <- pane(catalogue_html(e, rv_for(e, example_data, m)), "profile")
  expect_false(grepl("Paretian", p, fixed = TRUE))
  expect_match(p, "Level frequencies by dimension", fixed = TRUE)
})

test_that("each analysis shows an example output and a visible Select this", {
  skip_unless_app()
  e <- app_env()
  html <- catalogue_html(e, processed_rv(e))
  n_items <- lengths(regmatches(html, gregexpr('class="catalogue-item"', html)))
  expect_identical(n_items, 30L)
  expect_identical(lengths(regmatches(html, gregexpr(">\\s*Example output", html))), 30L)
  expect_identical(lengths(regmatches(html,
    gregexpr('class="btn btn-sm btn-primary catalogue-select"', html))), 30L)
})

test_that("a table example: a few rows, every column, and the full table on demand", {
  skip_unless_app()
  e <- app_env()
  html <- catalogue_html(e, processed_rv(e))
  item <- function(id) {
    title <- e$CATALOGUE[[id]]$title
    s <- regexpr(paste0("<h6>", title, "</h6>"), html, fixed = TRUE)
    nxt <- gregexpr('<div class="catalogue-item"', html)[[1]]
    nxt <- nxt[nxt > s]
    substr(html, s, if (length(nxt)) min(nxt) - 1L else nchar(html))
  }
  full <- e$catalogue_preview("121")$data         # 21 columns, 8 rows
  it <- item("121")
  expect_match(it, sprintf("Showing 5 of %d rows", nrow(full)), fixed = TRUE)
  expect_match(it, 'class="example-table-wrap"', fixed = TRUE)
  first <- regmatches(it, regexpr("<table.*?</table>", it))
  # Every column is kept: the wide table scrolls rather than losing any.
  expect_identical(lengths(regmatches(first, gregexpr("<th[ >]", first))), ncol(full))
  expect_identical(lengths(regmatches(first, gregexpr("<tr>", first))), 6L)  # header + 5
  expect_match(first, "mo Pre-op n", fixed = TRUE)                # readable headings
  expect_match(it, "<summary>View full example</summary>", fixed = TRUE)
  tabs <- regmatches(it, gregexpr("<table.*?</table>", it))[[1]]
  expect_identical(lengths(regmatches(tabs[2], gregexpr("<tr>", tabs[2]))),
                   nrow(full) + 1L)
  # A table of five rows or fewer has nothing more to show.
  short <- Filter(function(s) !identical(s$type, "plot") &&
                    nrow(e$catalogue_preview(s$id)$data) <= 5L, e$ANALYSES)
  for (s in short)
    expect_false(grepl("View full example", item(s$id), fixed = TRUE), info = s$id)
})

test_that("a figure example: a thumbnail with alt text, and Enlarge", {
  skip_unless_app()
  e <- app_env()
  html <- catalogue_html(e, processed_rv(e))
  expect_match(html, '<img src="eq5d-catalogue/125fig.png"', fixed = TRUE)
  expect_match(html, 'alt="Example output: Health Profile Grid (HPG)"', fixed = TRUE)
  expect_match(html, '<a href="eq5d-catalogue/125fig.png" target="_blank"',
               fixed = TRUE)
  expect_match(html, "Enlarge", fixed = TRUE)
  expect_true("eq5d-catalogue" %in% names(shiny::resourcePaths()))
})

test_that("opening the catalogue runs no analysis, and reads each example once", {
  skip_unless_app()
  e <- app_env()
  e$pkg_fn <- function(name) stop("an analysis ran: ", name)
  rv <- processed_rv(e)
  expect_no_error(catalogue_html(e, rv))
  reads <- e$CATALOGUE_CACHE$reads
  expect_gt(reads, 0L)
  catalogue_html(e, rv)
  expect_identical(e$CATALOGUE_CACHE$reads, reads)
  expect_length(shiny::isolate(rv$results), 0L)
})

test_that("the layout adapts to small screens", {
  skip_unless_app()
  css <- paste(readLines(file.path(app_dir(), "WWW", "styles.css"),
                         warn = FALSE), collapse = "\n")
  expect_match(css, "@media (max-width: 767.98px)", fixed = TRUE)
  expect_match(css, ".example-table-wrap { overflow-x: auto;", fixed = TRUE)
})
