# global.R — loaded once by Shiny before ui.R and server.R
# Loads packages, sources modules, and defines shared helpers.
#
# The app never attaches eq5dsuite. run_app() loads the package but does not
# put it on the search path, so every call into it is written eq5dsuite:: (or
# eq5dsuite::: for internal helpers). See tests/testthat/test-shiny-app.R.

library(shiny)
library(bslib)
library(DT)

# ── Mode ──────────────────────────────────────────────────────────────────────
# Local by default; online when run_app(online = TRUE) or EQ5DSUITE_ONLINE says
# so. Read once, here, and every online-only branch in the app is
# `if (ONLINE$enabled)`.
ONLINE <- eq5dsuite:::eq5d_online_config(
  online = getOption("eq5dsuite.online.config")$enabled
)
# What the file picker should offer. Locally that includes .rds, which is not
# accepted online.
ONLINE$allowed_types_ui <- if (ONLINE$enabled) {
  ONLINE$allowed_types
} else {
  c("csv", "xlsx", "xls", "rds")
}

# Canonical column names used inside the app once a mapping is confirmed.
DIMS_STD <- c("mo", "sc", "ua", "pd", "ad")

# Resolve an eq5dsuite function by name, without attaching the package.
pkg_fn <- function(name) getExportedValue("eq5dsuite", name)

# Null-coalescing operator, used throughout.
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b

# ── Expose internal package helpers in Shiny scope ────────────────────────────
# These are defined in R/shiny_helpers.R as non-exported functions.

DIM_LABELS          <- eq5dsuite:::DIM_LABELS
ensure_fu           <- eq5dsuite:::ensure_fu
compute_utility_col <- eq5dsuite:::compute_utility_col
write_results_docx  <- eq5dsuite:::write_results_docx

# ── Reading and mapping uploaded data ─────────────────────────────────────────

#' Suggest column mapping from a data frame's names
suggest_mapping <- function(col_names) {
  lower <- tolower(col_names)

  match_first <- function(patterns) {
    for (p in patterns) {
      idx <- grep(p, lower)
      if (length(idx) > 0L) return(col_names[idx[1L]])
    }
    NULL
  }

  list(
    mo      = match_first(c("^mo$", "mobil")),
    sc      = match_first(c("^sc$", "self.?care", "selfcare")),
    ua      = match_first(c("^ua$", "usual")),
    pd      = match_first(c("^pd$", "pain")),
    ad      = match_first(c("^ad$", "anxiet")),
    name_fu = match_first(c("^fu$", "^time$", "timepoint", "follow",
                            "^visit$", "^wave$", "^round$")),
    name_groupvar = match_first(c("^group$", "groupvar", "procedure", "category",
                                  "cohort", "^arm$", "^treatment$")),
    name_id = match_first(c("^id$", "patient.?id", "subject.?id", "person.?id",
                            "^patient$", "^subject$", "^person$")),
    name_vas = match_first(c("^vas$", "eq.?vas", "visual")),
    name_age = match_first(c("^age$", "age.?band", "ageband", "age.?group")),
    name_sex = match_first(c("^sex$", "^gender$", "^male$"))
  )
}

# Add a synthetic "groupvar" of "All" when no group column is mapped, so the
# by-group analyses still run on ungrouped data.
ensure_groupvar <- function(df) {
  if (!"groupvar" %in% names(df)) df[["groupvar"]] <- "All"
  df
}

# ── Dataset previews ──────────────────────────────────────────────────────────

# Rows a preview offers to show at once.
PREVIEW_ROWS <- c(10L, 20L, 50L, 100L)

#' A preview of the working dataset
#'
#' EQ-5D value columns are shown to three decimal places. That is display
#' only: `df` keeps full precision, which is what the downloads write.
preview_table <- function(df, value_cols = character(0L)) {
  dt <- DT::datatable(
    df,
    options  = list(pageLength = PREVIEW_ROWS[1L],
                    lengthMenu = PREVIEW_ROWS,
                    scrollX = TRUE,
                    dom = "ltip"),
    rownames = FALSE,
    class    = "table-sm table-striped")
  shown <- intersect(value_cols, names(df))
  if (length(shown)) dt <- DT::formatRound(dt, columns = shown, digits = 3L)
  dt
}

#' The EQ-5D value columns of the working dataset, in the order they appeared
#'
#' An existing column mapped on the Data page is renamed to "utility" by
#' apply_mapping(); columns calculated on the Calculate EQ-5D values page keep
#' the name they were given there.
value_columns <- function(rv) {
  cols <- rv$value_cols %||% character(0L)
  df <- rv$processed_data
  if (is.null(df)) return(cols)
  cols[cols %in% names(df)]
}

# ── Results ───────────────────────────────────────────────────────────────────

#' The R code for a call, as the script would write it
#'
#' One deparser for the "Show R code" panel and for the generated script, so
#' the two cannot disagree. It quotes safely: a column named `It's a group`
#' comes out as valid R, which the string-pasting it replaces did not.
format_call <- function(fn_name, args) {
  eq5dsuite:::.deparse_call(fn_name, args)
}

#' Save a result to rv$results
#' `call` is the structured record of the analysis -- its function, the
#' arguments it was given and any preparation of the data frame. The generated
#' script is built from these, never from assembled text, and because the
#' record travels on the result it follows the user's reordering and removal.
save_result <- function(rv, label, fn_call, result_type, data = NULL,
                        plot = NULL, call = NULL) {
  entry <- list(
    id          = paste0("r", format(Sys.time(), "%Y%m%d%H%M%S%OS3")),
    timestamp   = Sys.time(),
    label       = label,
    fn_call     = fn_call,
    result_type = result_type,   # "table", "plot", or "both"
    data        = data,
    plot        = plot,
    call        = call
  )
  rv$results <- c(rv$results, list(entry))
  invisible(entry$id)
}

#' Record a step of the session, for the generated script
#'
#' Steps are structured records, not text. A step of a given `kind` replaces an
#' earlier one where only the latest can apply (the data loaded, the mapping
#' confirmed); value calculations accumulate, in order.
record_step <- function(rv, kind, ..., replace = TRUE) {
  step <- c(list(kind = kind), list(...))
  steps <- rv$steps %||% list()
  if (isTRUE(replace))
    steps <- Filter(function(s) !identical(s$kind, kind), steps)
  rv$steps <- c(steps, list(step))
  invisible(step)
}

#' Position of a saved result, or NA
result_index <- function(results, id) {
  if (is.null(id) || !length(results)) return(NA_integer_)
  idx <- which(vapply(results, function(r) identical(r$id, id), logical(1L)))
  if (!length(idx)) NA_integer_ else idx[1L]
}

#' Move a saved result up (`by = -1`) or down (`by = 1`)
#'
#' The order lives in rv$results, which the Results page, the Export page and
#' the Word report all read, so moving a result here moves it everywhere.
move_result <- function(rv, id, by) {
  results <- rv$results
  i <- result_index(results, id)
  j <- i + by
  if (is.na(i) || j < 1L || j > length(results)) return(invisible(FALSE))
  results[c(i, j)] <- results[c(j, i)]
  rv$results <- results
  invisible(TRUE)
}

#' Remove a saved result
remove_result <- function(rv, id) {
  results <- rv$results
  i <- result_index(results, id)
  if (is.na(i)) return(invisible(FALSE))
  rv$results <- results[-i]
  invisible(TRUE)
}

#' A short label for the kind of a saved result
result_icon <- function(result_type) {
  switch(result_type, table = "table", plot = "chart-bar",
         both = "layer-group", "file")
}

# ── Value sets ────────────────────────────────────────────────────────────────

#' Get version-aware utility method choices for the UI
#'
#' The direction of every mapping follows the instrument version set on the
#' Data page, so each label states it.
get_utility_method_choices <- function(eq5d_version) {
  if (identical(eq5d_version, "3L")) {
    c(
      "Direct (3L value set)"              = "direct",
      "Reverse crosswalk 3L→5L (XWR)" = "xwr",
      "NICE DSU UK mapping 3L→5L"     = "uk"
    )
  } else {
    c(
      "Direct (5L value set)"          = "direct",
      "Crosswalk 5L→3L (XW)"      = "xw",
      "NICE DSU UK mapping 5L→3L" = "uk"
    )
  }
}

#' Whose value sets a method uses
#'
#' Not the instrument the data were collected with -- the instrument the method
#' produces values on. A direct value set is the data's own, but a crosswalk
#' values one instrument's responses with the *other* instrument's value set:
#' `eqxwr()` maps EQ-5D-3L responses onto EQ-5D-5L value sets, and `eqxw()`
#' maps EQ-5D-5L responses onto EQ-5D-3L ones.
#'
#' Getting this wrong is not cosmetic. About half the value sets exist for both
#' instruments, so offering the data's own version appears to work; the other
#' half are refused by `eq5d()` with "No valid countries listed".
value_set_version <- function(method, eq5d_version) {
  switch(method %||% "direct",
    xwr    = "5L",            # 3L responses, 5L values
    xw     = "3L",            # 5L responses, 3L values
    eq5d_version              # direct, and anything unrecognised
  )
}

#' Which way the NICE DSU UK mapping runs, given the instrument version
uk_direction <- function(eq5d_version) {
  if (identical(eq5d_version, "3L"))
    list(fn = "eqxwr_UK", from = "EQ-5D-3L", to = "EQ-5D-5L", col = "eq5d_uk_5L")
  else
    list(fn = "eqxw_UK", from = "EQ-5D-5L", to = "EQ-5D-3L", col = "eq5d_uk_3L")
}

#' Get available value-set country codes for a given EQ-5D version
get_country_choices <- function(eq5d_version) {
  tryCatch({
    vs_df <- eq5dsuite::eqvs_display(version = eq5d_version, return_df = TRUE)
    codes <- vs_df[["VS_code"]]
    names_col <- if ("Name" %in% names(vs_df)) vs_df[["Name"]] else codes
    stats::setNames(codes, paste0(names_col, " (", codes, ")"))
  }, error = function(e) character(0L))
}

# ── Shared page grammar ───────────────────────────────────────────────────────
# Every working page is a sidebar of inputs on the left and results on the
# right. Nothing scrolls inside a box; cards size to their content.

#' A page: sidebar of inputs, then the results area
#'
#' `fillable = FALSE` is the point of the layout: bslib would otherwise make
#' the content flex to the viewport and scroll inside its own box. Here cards
#' size to their content and only the page scrolls.
page_shell <- function(..., sidebar_title = NULL, sidebar) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300, title = sidebar_title, open = "desktop", sidebar
    ),
    fillable = FALSE,
    ...
  )
}

#' A results card. Sizes to its content; figures can still be opened full
#' screen with the control in the corner.
result_card <- function(title, ..., full_screen = FALSE) {
  bslib::card(
    full_screen = full_screen,
    fill = FALSE,
    if (is.null(title)) NULL else bslib::card_header(title),
    bslib::card_body(fillable = FALSE, ...)
  )
}

#' A short muted paragraph, used for page and control descriptions
hint <- function(...) shiny::p(..., class = "hint")

#' A collapsible disclosure for rarely-changed options
disclosure <- function(title, ...) {
  shiny::tags$details(
    class = "disclosure",
    shiny::tags$summary(title),
    shiny::div(class = "disclosure-body", ...)
  )
}

#' A coloured note. `type` is "info", "warning" or "error".
note <- function(type, ...) {
  cls <- switch(type, warning = "note note-warning",
                error = "note note-error", "note note-info")
  icon <- switch(type, warning = "triangle-exclamation",
                 error = "circle-exclamation", "circle-info")
  shiny::div(class = cls, shiny::icon(icon), " ", ...)
}

#' The "Show R code" disclosure shown under every result
call_display <- function(output_id) {
  shiny::tags$details(
    class = "disclosure",
    shiny::tags$summary("Show R code"),
    shiny::div(class = "disclosure-body",
               shiny::verbatimTextOutput(output_id, placeholder = TRUE))
  )
}

#' Run an analysis without its warnings reaching the log
#'
#' The analysis functions warn about the data they were given, and some of
#' those warnings quote it: the follow-up warning names the values it did not
#' recognise, which come from the uploaded file. Shiny writes warnings to
#' stderr, which on a server is the log.
#'
#' Warnings are collected and shown to the person who uploaded the data, where
#' they belong, and muffled so they go no further. Locally they are shown the
#' same way, so the behaviour is one thing rather than two.
run_quietly <- function(expr) {
  seen <- character(0L)
  out <- withCallingHandlers(
    expr,
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage"))

  if (length(seen))
    shiny::showNotification(
      shiny::tagList(shiny::strong("The analysis reported:"),
                     shiny::tags$ul(lapply(unique(seen), shiny::tags$li))),
      type = "warning", duration = 12)
  out
}

err_notify <- function(e) {
  # Shown to the user, who supplied the data in it; never written to the log,
  # which on a server is read by someone else.
  shiny::showNotification(
    paste("Analysis error:", conditionMessage(e)),
    type = "error", duration = 8
  )
}

# ── Result tables ─────────────────────────────────────────────────────────────

analysis_table_output <- function(output_id) DT::DTOutput(output_id)

#' A figure that sizes itself by aspect ratio rather than a pixel height, so
#' it fills the width it is given and grows with the full-screen control.
plot_frame <- function(output_id) {
  shiny::div(class = "plot-frame",
             shiny::plotOutput(output_id, height = "100%"))
}

# The level-frequency tables are named n_<fu>_<dim> / freq_<fu>_<dim>; give
# them a two-row header grouping the columns by dimension.
build_profile_container <- function(df) {
  col_names <- names(df)
  if (length(col_names) < 2L || col_names[1L] != "level") return(NULL)

  rest <- col_names[-1L]
  if (!all(grepl("^(n|freq)_", rest))) return(NULL)

  parsed <- lapply(rest, function(cn) {
    parts <- strsplit(cn, "_")[[1L]]
    if (length(parts) < 3L) return(NULL)
    list(
      metric = parts[1L],
      dim    = parts[length(parts)],
      fu     = paste(parts[2L:(length(parts) - 1L)], collapse = "_")
    )
  })
  if (any(vapply(parsed, is.null, logical(1L)))) return(NULL)

  dims <- vapply(parsed, `[[`, character(1L), "dim")
  if (!all(dims %in% names(DIM_LABELS))) return(NULL)

  fu_vals   <- vapply(parsed, `[[`, character(1L), "fu")
  multi_fu  <- length(unique(fu_vals)) > 1L
  dim_order <- unique(dims)

  th_level <- shiny::tags$th(rowspan = 2L, "Level")
  th_dims  <- lapply(dim_order, function(d) {
    shiny::tags$th(
      colspan = sum(dims == d),
      style   = "text-align: center; border-bottom: 0;",
      DIM_LABELS[[d]]
    )
  })
  th_sub <- lapply(seq_along(rest), function(i) {
    p      <- parsed[[i]]
    symbol <- if (p$metric == "freq") "%" else "n"
    label  <- if (multi_fu) paste0(p$fu, " ", symbol) else symbol
    shiny::tags$th(label, style = "text-align: right;")
  })

  shiny::tags$table(
    class = "table table-sm table-striped display",
    shiny::tags$thead(
      shiny::tags$tr(th_level, th_dims),
      shiny::tags$tr(th_sub)
    )
  )
}

render_analysis_table <- function(data_reactive) {
  DT::renderDT({
    df <- data_reactive()
    shiny::req(df)

    container  <- build_profile_container(df)
    pct_cols   <- eq5dsuite:::.proportion_columns(df)
    count_cols <- eq5dsuite:::.whole_number_columns(df, except = pct_cols)
    # Round the remaining double columns (e.g. mean, sd) to 2 decimal places.
    dbl_cols   <- setdiff(names(df)[vapply(df, is.double, logical(1L))],
                          c(pct_cols, count_cols))

    opts <- list(pageLength = 15L, scrollX = TRUE, dom = "tip")

    dt <- if (!is.null(container)) {
      DT::datatable(df, container = container, options = opts, rownames = FALSE)
    } else {
      DT::datatable(df, options = opts, rownames = FALSE,
                    class = "table-sm table-striped")
    }

    if (length(pct_cols) > 0L)
      dt <- DT::formatPercentage(dt, columns = pct_cols, digits = 1L)
    if (length(count_cols) > 0L)
      dt <- DT::formatRound(dt, columns = count_cols, digits = 0L)
    if (length(dbl_cols) > 0L)
      dt <- DT::formatRound(dt, columns = dbl_cols, digits = 2L)

    dt
  })
}

# ── Guard for pages that need validated data ─────────────────────────────────
# The analyses need rv$processed_data, which only exists once the user has
# mapped the columns on the Data page *and* pressed "Proceed to Analysis" on
# the Validation page. The message names what is missing and carries a button
# to the page that supplies it.
analysis_guard <- function(rv, ns) {
  if (!is.null(rv$processed_data)) return(NULL)

  mapped <- !is.null(rv$mapping)
  bslib::card(bslib::card_body(
    shiny::p(shiny::icon("circle-info"), " ",
             if (mapped)
               "Your columns are mapped. Review the data checks on the
                Validation page and press “Proceed” to continue."
             else
               "No data yet. Upload a file or load the example dataset, then
                map the EQ-5D columns.",
             class = "text-muted"),
    shiny::actionButton(
      ns(if (mapped) "goto_validation" else "goto_data"),
      if (mapped) "Go to Validation" else "Go to Data",
      class = "btn-primary", icon = shiny::icon("arrow-right"))
  ))
}

# Wire that button up. Called once per module server that shows the guard.
analysis_guard_server <- function(input, session) {
  root <- session$rootScope()
  shiny::observeEvent(input$goto_data,
    bslib::nav_select("main_nav", "data", session = root))
  shiny::observeEvent(input$goto_validation,
    bslib::nav_select("main_nav", "validation", session = root))
}

# Jump to another top-level page from anywhere.
goto_page <- function(session, value) {
  bslib::nav_select("main_nav", value, session = session$rootScope())
}

# ── Session files ─────────────────────────────────────────────────────────────

#' A folder of this session's own, for anything the app writes
#'
#' Created on first use and removed when the session ends, so nothing a user
#' uploads or generates outlives their visit or is visible to anyone else.
#' Shiny's own download handlers already write to paths it manages; this is for
#' the staging directories the app makes itself.
session_dir <- function(session) {
  d <- session$userData$eq5d_dir
  if (is.null(d)) {
    d <- tempfile("eq5dsession_")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    session$userData$eq5d_dir <- d
  }
  d
}

# A file inside the session's folder. The name is ours, never the uploaded
# file's: a name from a stranger has no business in a server-side path.
session_file <- function(session, prefix, ext) {
  tempfile(prefix, tmpdir = session_dir(session), fileext = ext)
}

# The file formats this app accepts, written out. Online that is a shorter
# list, so the help text and the file picker cannot disagree.
accepted_types_text <- function() {
  t <- paste0(".", ONLINE$allowed_types_ui)
  n <- length(t)
  parts <- list()
  for (i in seq_len(n)) {
    parts <- c(parts, list(shiny::code(t[i])))
    if (i < n - 1L)       parts <- c(parts, list(", "))
    else if (i == n - 1L) parts <- c(parts, list(" or "))
  }
  shiny::tagList(parts)
}

# ── Online notices ────────────────────────────────────────────────────────────

# Shown only online. The wording lives in eq5d_online_notice(), which a
# deployment can override without touching the app.
online_note <- function(which, type = "info") {
  if (!ONLINE$enabled) return(NULL)
  note(type, eq5dsuite:::eq5d_online_notice(which))
}

# ── Modules ───────────────────────────────────────────────────────────────────
# Sourced last, so that everything above is available while they are read: the
# analysis registry in mod_analysis.R is built at source time and uses
# DIMS_STD.
for (.f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE)
}
rm(.f)
