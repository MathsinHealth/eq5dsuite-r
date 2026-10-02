# mod_results.R — Results explorer module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Saved results",
    sidebar = shiny::uiOutput(ns("results_list")),
    shiny::uiOutput(ns("result_viewer"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_results_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Currently selected result id
    selected_id <- shiny::reactiveVal(NULL)

    # ── Results list ──────────────────────────────────────────────────────────
    # Shown in the order they will be exported, numbered so that order is
    # plain, with controls to move a result or drop it. The order lives in
    # rv$results, so it survives switching tabs and adding another result, and
    # the Export page and the Word report follow it.
    output$results_list <- shiny::renderUI({
      results <- rv$results
      if (length(results) == 0L) {
        return(hint("No results yet. Every analysis you run is saved here, ",
                    "in the order you run them."))
      }

      n <- length(results)
      rows <- lapply(seq_len(n), function(i) {
        r <- results[[i]]
        small <- function(id, icon, title, disabled = FALSE) {
          b <- shiny::actionButton(ns(id), label = NULL,
                                   icon = shiny::icon(icon),
                                   title = title,
                                   class = "btn btn-sm btn-outline-secondary")
          if (disabled) b$attribs$disabled <- "disabled"
          b
        }
        shiny::div(
          class = if (identical(selected_id(), r$id))
            "result-row result-row-selected" else "result-row",
          shiny::div(
            class = "result-row-main",
            shiny::actionLink(
              ns(paste0("view_", r$id)),
              shiny::tagList(
                shiny::span(class = "result-n", i), " ",
                shiny::icon(result_icon(r$result_type)), " ", r$label)),
            shiny::tags$small(class = "hint",
                              format(r$timestamp, "%H:%M:%S %d %b %Y"))
          ),
          shiny::div(
            class = "result-row-controls",
            small(paste0("up_", r$id), "arrow-up", "Move up", i == 1L),
            small(paste0("down_", r$id), "arrow-down", "Move down", i == n),
            small(paste0("rm_", r$id), "xmark", "Remove")
          )
        )
      })
      shiny::tagList(rows,
                     hint("Results are exported in this order. The Export page ",
                          "also offers an R script that repeats this session."))
    })

    # One set of observers per result id. observeEvent() is idempotent per id
    # here because the ids are unique and the list only grows or shrinks.
    wired <- shiny::reactiveVal(character(0L))
    shiny::observe({
      ids <- vapply(rv$results, function(r) r$id, character(1L))
      new_ids <- setdiff(ids, wired())
      for (the_id in new_ids) {
        local({
          rid <- the_id
          shiny::observeEvent(input[[paste0("view_", rid)]],
                              selected_id(rid), ignoreInit = TRUE)
          shiny::observeEvent(input[[paste0("up_", rid)]],
                              move_result(rv, rid, -1L), ignoreInit = TRUE)
          shiny::observeEvent(input[[paste0("down_", rid)]],
                              move_result(rv, rid, 1L), ignoreInit = TRUE)
          shiny::observeEvent(input[[paste0("rm_", rid)]], {
            if (identical(shiny::isolate(selected_id()), rid))
              selected_id(NULL)
            remove_result(rv, rid)
          }, ignoreInit = TRUE)
        })
      }
      if (length(new_ids)) wired(c(wired(), new_ids))
    })

    # ── Viewer title ──────────────────────────────────────────────────────────
    output$viewer_title <- shiny::renderUI({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      if (is.null(res)) return("Result Viewer")
      shiny::tagList(res$label,
                     shiny::tags$small(
                       class = "text-muted ms-2",
                       format(res$timestamp, "%d %b %Y %H:%M")
                     ))
    })

    # ── Result viewer ─────────────────────────────────────────────────────────
    output$result_viewer <- shiny::renderUI({
      id  <- selected_id()
      res <- find_result(rv$results, id)

      if (is.null(res)) {
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, 
          hint("Select a result from the list on the left."))))
      }

      content <- list()

      # Show table
      if (res$result_type %in% c("table", "both") && !is.null(res$data)) {
        content <- c(content, list(
          DT::DTOutput(ns("viewer_table"))
        ))
      }

      # Show plot
      if (res$result_type %in% c("plot", "both") && !is.null(res$plot)) {
        content <- c(content, list(
          plot_frame(ns("viewer_plot"))
        ))
      }

      # Show R call
      content <- c(content, list(call_display(ns("viewer_call"))))

      i <- result_index(rv$results, id)
      bslib::card(
        full_screen = TRUE, fill = FALSE,
        bslib::card_header(
          res$label,
          shiny::tags$small(class = "hint ms-2",
                            sprintf("%d of %d", i, length(rv$results)))),
        bslib::card_body(fillable = FALSE, content)
      )
    })

    # Render selected result table
    output$viewer_table <- DT::renderDT({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      shiny::req(res, !is.null(res$data))
      DT::datatable(res$data,
                    options  = list(pageLength = 15L, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE,
                    class    = "table-sm table-striped")
    })

    # Render selected result plot
    output$viewer_plot <- shiny::renderPlot({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      shiny::req(res, !is.null(res$plot))
      res$plot
    })

    # Render selected result R call
    output$viewer_call <- shiny::renderText({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      shiny::req(res)
      res$fn_call
    })
  })
}

# ── Helper ────────────────────────────────────────────────────────────────────

find_result <- function(results, id) {
  if (is.null(id) || length(results) == 0L) return(NULL)
  idx <- which(vapply(results, function(r) identical(r$id, id), logical(1L)))
  if (length(idx) == 0L) return(NULL)
  results[[idx[1L]]]
}
