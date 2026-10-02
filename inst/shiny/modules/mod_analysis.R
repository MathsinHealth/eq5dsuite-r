# mod_analysis.R — one Analysis page for all three components
#
# The sidebar has two selects — Component, then Output — followed by whatever
# that output needs, then a single Run. The main area shows one card.
#
# Every analysis is one entry in ANALYSES below. The arguments each entry
# builds are exactly those the previous accordion-per-analysis version built,
# so the computation is unchanged; only the way the user reaches it differs.

COMPONENTS <- c("EQ-5D profiles" = "profile",
                "EQ-5D values"   = "values",
                "EQ VAS"         = "vas")

# Shorthands used by the registry's prep functions.
.ver  <- function(rv) rv$mapping$eq5d_version
.base <- function(rv) list(names_eq5d = DIMS_STD, eq5d_version = .ver(rv))
# The timepoint order set on the Data page. NULL when no timepoint is mapped,
# which leaves the analysis functions to their own default.
.fu_lv <- function(rv) rv$mapping$levels_fu
.pchc_args <- function(rv) list(name_id = "id", name_groupvar = "groupvar",
                                names_eq5d = DIMS_STD, name_fu = "fu",
                                levels_fu = .fu_lv(rv))

# needs codes: fu, id, groupvar, vas, utility
# opts codes:  utility_col, topn, two_fu, group_filter
#
# No analysis takes a value set any more: every one that uses EQ-5D values
# takes them from a column, which the Calculate EQ-5D values page produces.
a <- function(id, component, group, label, desc, type, fn, save_label,
              needs = character(0), opts = character(0), prep) {
  list(id = id, component = component, group = group, label = label,
       desc = desc, type = type, fn = fn, save_label = save_label,
       needs = needs, opts = opts, prep = prep)
}

ANALYSES <- list(

  # ── EQ-5D profiles: cross-sectional ────────────────────────────────────────
  a("111", "profile", "Cross-sectional",
    "Table 1.1.1 — Level frequencies by dimension",
    "Frequency (n and %) of each response level across all five EQ-5D dimensions.",
    "table", "eq5d_profile_level_summary", "Level frequencies (1.1.1)",
    prep = function(input, rv) list(df = rv$processed_data, args = .base(rv))),

  a("112", "profile", "Cross-sectional",
    "Table 1.1.2 — Level frequencies by group",
    "Frequency of response levels stratified by group.",
    "table", "eq5d_profile_level_summary_by_group", "Level freq. by group (1.1.2)",
    needs = "groupvar",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, name_cat = "groupvar",
                  eq5d_version = .ver(rv)))),

  a("113", "profile", "Cross-sectional",
    "Table 1.1.3 — Most common health states",
    "The most frequently observed EQ-5D health states in the dataset.",
    "table", "eq5d_profile_top_states", "Most common states (1.1.3)",
    opts = "topn",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, eq5d_version = .ver(rv),
                  n = as.integer(input$topn)))),

  # ── EQ-5D profiles: longitudinal ──────────────────────────────────────────
  a("121", "profile", "Longitudinal",
    "Table 1.2.1 — Level frequencies by timepoint",
    "Frequency of response levels at each timepoint.",
    "table", "eq5d_profile_change_summary", "Level freq. by timepoint (1.2.1)",
    needs = "fu",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, name_fu = "fu",
                  levels_fu = .fu_lv(rv), eq5d_version = .ver(rv)))),

  a("122", "profile", "Longitudinal",
    "Table 1.2.2 — PCHC health change",
    paste("Paretian Classification of Health Change: how each respondent's",
          "profile moved between two timepoints. Ungrouped data are treated",
          "as one group."),
    "table", "eq5d_profile_pchc_table", "PCHC (1.2.2)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("123", "profile", "Longitudinal",
    "Table 1.2.3 — PCHC accounting for no problems",
    "PCHC separating those who reported no problems at baseline.",
    "table", "eq5d_profile_pchc_with_no_problems_table", "PCHC no-problems (1.2.3)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("124", "profile", "Longitudinal",
    "Table 1.2.4 — Level change between timepoints",
    "Proportion improving, stable or worsening in each dimension.",
    "table", "eq5d_profile_dimension_change_table", "Level change (1.2.4)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_id = "id", names_eq5d = DIMS_STD, name_fu = "fu",
                  levels_fu = .fu_lv(rv)))),

  a("121fig", "profile", "Longitudinal",
    "Figure 1.2.1 — PCHC categories by group",
    "Bar chart of the PCHC categories, by group.",
    "plot", "eq5d_profile_pchc_by_group_plot", "PCHC bar chart (fig. 1.2.1)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("122fig", "profile", "Longitudinal",
    "Figure 1.2.2 — Improvements by group and dimension",
    "Proportion improving in each dimension, by group.",
    "plot", "eq5d_profile_better_dimensions_by_group_plot",
    "Improvements chart (fig. 1.2.2)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("123fig", "profile", "Longitudinal",
    "Figure 1.2.3 — Worsenings by group and dimension",
    "Proportion worsening in each dimension, by group.",
    "plot", "eq5d_profile_worse_dimensions_by_group_plot",
    "Worsenings chart (fig. 1.2.3)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("124fig", "profile", "Longitudinal",
    "Figure 1.2.4 — Mixed changes by group and dimension",
    "Proportion with mixed change in each dimension, by group.",
    "plot", "eq5d_profile_mixed_dimensions_by_group_plot",
    "Mixed change chart (fig. 1.2.4)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("125fig", "profile", "Longitudinal",
    "Figure 1.2.5 — Health Profile Grid (HPG)",
    "Scatter plot comparing health profiles between two timepoints.",
    "plot", "eq5d_profile_health_profile_grid", "HPG scatter (fig. 1.2.5)",
    needs = c("utility", "fu", "id"), opts = c("utility_col", "two_fu"),
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, name_utility = input$utility_col,
                  name_fu = "fu", levels_fu = input$fu_levels,
                  name_id = "id"))),

  # ── EQ-5D profiles: summarising severity ──────────────────────────────────
  a("131", "profile", "Summarising severity",
    "Table 1.3.1 — Summary statistics by Level Sum Score (LSS)",
    "Summary statistics for EQ-5D values at each Level Sum Score.",
    "table", "eq5d_profile_lss_utility_summary", "LSS summary stats (1.3.1)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  a("131fig", "profile", "Summarising severity",
    "Figure 1.3.1 — EQ-5D values vs Level Sum Score",
    "EQ-5D values plotted against the Level Sum Score.",
    "plot", "eq5d_profile_lss_utility_plot", "LSS scatter (fig. 1.3.1)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  a("132", "profile", "Summarising severity",
    "Table 1.3.2 — Distribution by Level Frequency Score (LFS)",
    "Distribution of EQ-5D health states by Level Frequency Score.",
    "table", "eq5d_profile_lfs_distribution", "LFS distribution (1.3.2)",
    prep = function(input, rv) list(df = rv$processed_data, args = .base(rv))),

  a("134", "profile", "Summarising severity",
    "Table 1.3.4 — Summary statistics by Level Frequency Score (LFS)",
    "Summary statistics for EQ-5D values by Level Frequency Score.",
    "table", "eq5d_profile_lfs_utility_summary", "LFS summary stats (1.3.4)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  a("132fig", "profile", "Summarising severity",
    "Figure 1.3.2 — EQ-5D values vs Level Frequency Score",
    "EQ-5D values plotted against the Level Frequency Score.",
    "plot", "eq5d_profile_lfs_utility_plot", "LFS scatter (fig. 1.3.2)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  # ── EQ-5D profiles: informativity ─────────────────────────────────────────
  a("141", "profile", "Informativity",
    "Table 1.4.1 — Shannon's informativity indices",
    paste("Shannon's index H' in bits, its maximum H'max, and Shannon's",
          "evenness index J' = H'/H'max, for each dimension and for the",
          "health state: how much of the classification system the sample",
          "uses. By timepoint when one is mapped."),
    "table", "eq5d_profile_shannon", "Shannon's indices (1.4.1)",
    prep = function(input, rv) list(
      df = rv$processed_data,
      # H'max depends on what the instrument allows, so eq5d_version matters
      # here as much as anywhere. name_fu is passed only when a timepoint is
      # mapped: without one the function reports the sample as a whole.
      args = c(.base(rv),
               if ("fu" %in% names(rv$processed_data))
                 list(name_fu = "fu", levels_fu = .fu_lv(rv))))),

  a("141fig", "profile", "Informativity",
    "Figure 1.4.1 — Health State Density Index (HSDI)",
    "Cumulative distribution of the observed health profiles.",
    "plot", "eq5d_profile_density_curve", "HSDI (fig. 1.4.1)",
    prep = function(input, rv) list(df = rv$processed_data, args = .base(rv))),

  # ── EQ-5D values ──────────────────────────────────────────────
  # These analyse an existing column of EQ-5D values rather than calculating
  # one, so they need a value column and no value set. apply_mapping() renames
  # whichever column the user mapped, or the Calculate EQ-5D values page
  # produced, to "utility".
  a("31", "values", "Summary",
    "Table 3.1 \u2014 EQ-5D value summary statistics",
    "Descriptive statistics for the EQ-5D values, by timepoint.",
    "table", "eq5d_utility_summary", "Utility summary stats (3.1)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) {
      fu <- ensure_fu(rv$processed_data, rv$mapping)
      list(df = fu$df,
           prep = if (identical(fu$name_fu, "fu")) NULL else "fu_all",
           args = list(name_utility = input$utility_col, name_fu = fu$name_fu,
                       levels_fu = if (identical(fu$name_fu, "fu")) .fu_lv(rv)),
           # name_fu is synthetic when no timepoint is mapped, so it is left
           # out of the code shown to the user, as it always has been.
           call_args = list(name_utility = input$utility_col))
    }),

  a("fig34", "values", "Summary",
    "Figure 3.4 \u2014 EQ-5D value distribution",
    "Bar chart of the distribution of EQ-5D values.",
    "plot", "eq5d_utility_distribution_plot", "Utility bar chart (fig. 3.4)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col))),

  a("32", "values", "By group",
    "Table 3.2 \u2014 Mean EQ-5D value by group",
    "Mean EQ-5D value in each group.",
    "table", "eq5d_utility_summary_by_group", "Utility by group (3.2)",
    needs = c("utility", "groupvar"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_groupvar = "groupvar"))),

  a("fig32", "values", "By group",
    "Figure 3.2 \u2014 Mean EQ-5D value and 95% CI by group",
    "Mean EQ-5D value with 95% confidence intervals, by group.",
    "plot", "eq5d_utility_by_group_plot", "Utility CI bar chart (fig. 3.2)",
    needs = c("utility", "groupvar"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_groupvar = "groupvar"))),

  a("fig31", "values", "Over time",
    "Figure 3.1 \u2014 EQ-5D values by timepoint",
    "Box plots of the EQ-5D values at each timepoint.",
    "plot", "eq5d_utility_over_time_plot", "Utility box plots (fig. 3.1)",
    needs = c("utility", "fu"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_fu = "fu",
                  levels_fu = .fu_lv(rv)))),

  a("fig33", "values", "Over time",
    "Figure 3.3 \u2014 EQ-5D values by timepoint and group",
    "Mean EQ-5D value with 95% CI by timepoint, coloured by group.",
    "plot", "eq5d_utility_change_by_group_plot",
    "Utility by timepoint & group (fig. 3.3)",
    needs = c("utility", "fu", "groupvar"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_fu = "fu",
                  levels_fu = .fu_lv(rv), name_groupvar = "groupvar"))),

  a("fig35", "values", "Against the EQ VAS",
    "Figure 3.5 \u2014 EQ-5D values vs EQ VAS",
    "Scatter plot of EQ-5D values against the EQ VAS score.",
    "plot", "eq5d_utility_vas_scatter_plot", "Utility vs VAS (fig. 3.5)",
    needs = c("utility", "vas"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_vas = "vas"))),

  # ── EQ VAS ────────────────────────────────────────────────────────────────
  a("21", "vas", "Summary",
    "Table 2.1 — EQ VAS summary statistics by timepoint",
    "Descriptive statistics for the EQ VAS score, grouped by timepoint.",
    "table", "eq5d_vas_summary", "VAS summary stats (2.1)",
    needs = "vas", opts = "group_filter",
    prep = function(input, rv) {
      df <- rv$processed_data
      sel <- input$group_filter
      if (!is.null(sel) && sel != "All" && "groupvar" %in% names(df)) {
        df <- df[df[["groupvar"]] == sel, , drop = FALSE]
      }
      fu <- ensure_fu(df, rv$mapping)
      list(df = fu$df,
           prep = if (identical(fu$name_fu, "fu")) NULL else "fu_all",
           args = list(name_vas = "vas", name_fu = fu$name_fu,
                       levels_fu = if (identical(fu$name_fu, "fu")) .fu_lv(rv)),
           call_args = list(name_vas = "vas"))
    }),

  a("22", "vas", "Summary",
    "Table 2.2 — EQ VAS frequency of mid-points",
    "Frequency distribution of EQ VAS scores by mid-point intervals.",
    "table", "eq5d_vas_distribution_table", "VAS mid-points (2.2)",
    needs = "vas",
    prep = function(input, rv) list(df = rv$processed_data,
                                    args = list(name_vas = "vas"))),

  a("fig21", "vas", "Distribution",
    "Figure 2.1 — EQ VAS distribution (histogram)",
    "Histogram of the EQ VAS distribution.",
    "plot", "eq5d_vas_histogram", "VAS histogram (fig. 2.1)",
    needs = "vas",
    prep = function(input, rv) list(df = rv$processed_data,
                                    args = list(name_vas = "vas"))),

  a("fig22", "vas", "Distribution",
    "Figure 2.2 — EQ VAS distribution by mid-points (box plot)",
    "Box plots of the EQ VAS distribution by mid-point interval.",
    "plot", "eq5d_vas_grouped_distribution_plot", "VAS box plot (fig. 2.2)",
    needs = "vas",
    prep = function(input, rv) list(df = rv$processed_data,
                                    args = list(name_vas = "vas")))
)
rm(a)

analysis_spec <- function(id) {
  for (s in ANALYSES) if (identical(s$id, id)) return(s)
  NULL
}

# Choices for the Output select, as an optgroup list.
analysis_choices <- function(component) {
  specs <- Filter(function(s) identical(s$component, component), ANALYSES)
  out <- list()
  for (g in unique(vapply(specs, `[[`, character(1L), "group"))) {
    inner <- Filter(function(s) identical(s$group, g), specs)
    out[[g]] <- stats::setNames(
      vapply(inner, `[[`, character(1L), "id"),
      vapply(inner, `[[`, character(1L), "label"))
  }
  out
}

# What a spec still needs, in words. Empty means it can run.
unmet_needs <- function(spec, rv) {
  m <- rv$mapping
  if (is.null(m)) return("data")
  lab <- c(fu = "a Timepoint column", id = "a Patient ID column",
           groupvar = "a Group column", vas = "an EQ VAS column",
           utility = "an EQ-5D value column")
  out <- character(0L)
  for (n in spec$needs) {
    if (identical(n, "utility")) {
      # Any of the value columns will do: one mapped on the Data page, or any
      # calculated on the Calculate EQ-5D values page.
      if (!length(value_columns(rv))) out <- c(out, lab[["utility"]])
    } else if (is.null(m[[paste0("name_", n)]]) ||
               !nzchar(m[[paste0("name_", n)]])) {
      out <- c(out, lab[[n]])
    }
  }
  out
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Analysis",
    sidebar = shiny::tagList(
      shiny::selectInput(ns("component"), "Component",
                         choices = COMPONENTS, selected = "profile"),
      shiny::selectInput(ns("output"), "Output",
                         choices = analysis_choices("profile")),
      shiny::uiOutput(ns("desc")),
      shiny::uiOutput(ns("opts")),
      shiny::uiOutput(ns("run_ui"))
    ),
    shiny::uiOutput(ns("guard")),
    shiny::uiOutput(ns("result"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_analysis_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$guard <- shiny::renderUI(analysis_guard(rv, ns))
    analysis_guard_server(input, session)

    shiny::observeEvent(input$component, {
      shiny::updateSelectInput(session, "output",
                               choices = analysis_choices(input$component))
    })

    spec <- shiny::reactive({
      shiny::req(input$output)
      analysis_spec(input$output)
    })

    missing <- shiny::reactive({
      s <- spec(); shiny::req(s)
      unmet_needs(s, rv)
    })

    output$desc <- shiny::renderUI({
      s <- spec(); shiny::req(s)
      hint(s$desc)
    })

    timepoints <- shiny::reactive({
      df <- rv$processed_data
      if (is.null(df) || !"fu" %in% names(df)) return(character(0L))
      sort(unique(as.character(df[["fu"]])))
    })

    groups <- shiny::reactive({
      df <- rv$processed_data
      if (is.null(df) || !"groupvar" %in% names(df)) return(character(0L))
      sort(unique(as.character(df[["groupvar"]])))
    })

    # ── The controls this output needs ───────────────────────────────────────
    output$opts <- shiny::renderUI({
      s <- spec(); shiny::req(s)
      if (length(missing()) > 0L) return(NULL)
      bits <- list()

      if ("utility_col" %in% s$opts) {
        cols <- value_columns(rv)
        bits <- c(bits, list(shiny::selectInput(
          ns("utility_col"), "Utility column",
          choices = cols,
          selected = shiny::isolate(input$utility_col) %||% cols[1L])))
      }
      if ("topn" %in% s$opts) {
        bits <- c(bits, list(shiny::numericInput(
          ns("topn"), "How many states", value = 10L,
          min = 1L, max = 50L, step = 1L)))
      }
      if ("two_fu" %in% s$opts) {
        tp <- timepoints()
        bits <- c(bits, list(
          if (length(tp) < 2L)
            note("warning", "At least two timepoints are needed.")
          else shiny::selectInput(
            ns("fu_levels"), "Compare which two timepoints",
            choices = tp, selected = tp[1:2], multiple = TRUE)))
      }
      if ("group_filter" %in% s$opts) {
        g <- groups()
        if (length(g) > 0L) {
          bits <- c(bits, list(shiny::selectInput(
            ns("group_filter"), "Restrict to group",
            choices = c("All" = "All", stats::setNames(g, g)),
            selected = "All")))
        }
      }
      if (length(bits) == 0L) return(NULL)
      shiny::tagList(bits)
    })

    output$run_ui <- shiny::renderUI({
      shiny::req(spec())
      miss <- missing()
      if (identical(miss, "data")) return(NULL)
      if (length(miss) > 0L) {
        # A value column is the one missing piece the user cannot supply on
        # the Data page unless their file already has one, so send them to the
        # page that produces it.
        wants_value <- "an EQ-5D value column" %in% miss
        # One string, not several children: Shiny puts whitespace between a
        # tag's children, which would show as "value column ." here.
        return(shiny::tagList(
          note("warning", paste0(
            "This analysis needs ", paste(miss, collapse = " and "),
            if (wants_value)
              ". Calculate one from the EQ-5D dimensions, or map a column you
               already have."
            else ". Map it on the Data page.")),
          if (wants_value)
            shiny::actionButton(ns("goto_values"), "Calculate EQ-5D values",
                                class = "btn-primary w-100",
                                icon = shiny::icon("calculator"))
        ))
      }
      shiny::actionButton(ns("run"), "Run", class = "btn-primary w-100",
                          icon = shiny::icon("play"))
    })

    shiny::observeEvent(input$goto_values, goto_page(session, "values"))

    # ── Run ──────────────────────────────────────────────────────────────────
    res <- shiny::reactiveValues(data = NULL, plot = NULL, call = NULL,
                                 spec = NULL)

    shiny::observeEvent(input$run, {
      s <- spec()
      shiny::req(s, rv$processed_data, length(missing()) == 0L)

      if ("utility_col" %in% s$opts &&
          !isTRUE(input$utility_col %in% value_columns(rv))) {
        shiny::showNotification("Choose a utility column first.",
                                type = "warning", duration = 4)
        return()
      }
      if ("two_fu" %in% s$opts && length(input$fu_levels %||% character(0L)) != 2L) {
        shiny::showNotification("Select exactly two timepoints.",
                                type = "warning", duration = 5)
        return()
      }

      p  <- s$prep(input, rv)
      # The code shown to the user, and the record the script is built from,
      # are the same arguments deparsed the same way.
      cs <- format_call(s$fn, c(list(df = quote(analysis_data)), p$args))
      rec <- list(fn = s$fn, args = p$args, prep = p$prep, type = s$type)

      tryCatch({
        out <- run_quietly(do.call(pkg_fn(s$fn), c(list(df = p$df), p$args)))
        if (identical(s$type, "plot")) {
          res$plot <- out$p
          res$data <- NULL
          save_result(rv, s$save_label, cs, "plot", plot = out$p, call = rec)
        } else {
          res$data <- out
          res$plot <- NULL
          save_result(rv, s$save_label, cs, "table", data = out, call = rec)
        }
        res$call <- cs
        res$spec <- s
      }, error = function(e) err_notify(e))
    })

    # Clear the shown result when the user picks a different output.
    shiny::observeEvent(input$output, {
      res$data <- NULL; res$plot <- NULL; res$call <- NULL; res$spec <- NULL
    })

    # ── The one result card ──────────────────────────────────────────────────
    output$result <- shiny::renderUI({
      if (is.null(rv$processed_data)) return(NULL)
      s <- res$spec
      if (is.null(s)) {
        cur <- spec()
        miss <- missing()
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, hint(
          if ("an EQ-5D value column" %in% miss)
            "This analysis needs a column of EQ-5D values. Calculate one on the
             Calculate EQ-5D values page, or map a column you already have on
             the Data page."
          else if (length(miss) > 0L)
            "Map the columns this analysis needs, then come back."
          else paste0("Press Run to produce ", cur$label, ".")))))
      }
      bslib::card(
        full_screen = TRUE, fill = FALSE,
        bslib::card_header(s$label),
        bslib::card_body(fillable = FALSE,
          if (identical(s$type, "plot")) plot_frame(ns("plot"))
          else analysis_table_output(ns("table")),
          call_display(ns("call"))
        )
      )
    })

    output$table <- render_analysis_table(shiny::reactive(res$data))
    output$plot  <- shiny::renderPlot(res$plot)
    output$call  <- shiny::renderText(res$call)
  })
}
