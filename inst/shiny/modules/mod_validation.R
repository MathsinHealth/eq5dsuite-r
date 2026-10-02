# mod_validation.R — Data validation and preprocessing module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Validation",
    sidebar = shiny::uiOutput(ns("sidebar")),
    shiny::uiOutput(ns("main"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_validation_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive: run validation when mapping is confirmed ───────────────────
    validation <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      # The checks come back as plain text; the markup is the app's.
      eq5dsuite:::eq5d_validate(rv$raw_data, rv$mapping, quiet = TRUE)
    })

    # ── Render: main UI (shown only when mapping exists) ─────────────────────
    output$sidebar <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(shiny::tagList(
          hint("Upload data and confirm the column mapping first."),
          shiny::actionButton(ns("goto_data"), "Go to Data",
                              class = "btn-primary w-100",
                              icon = shiny::icon("arrow-right"))
        ))
      }
      shiny::tagList(
        shiny::tags$p("Mapping", class = "sidebar-label"),
        shiny::uiOutput(ns("mapping_summary")),
        shiny::hr(),
        shiny::actionButton(ns("proceed"), "Proceed",
                            class = "btn-primary w-100",
                            icon = shiny::icon("arrow-right"))
      )
    })

    shiny::observeEvent(input$goto_data, goto_page(session, "data"))

    output$main <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, hint(
          "Nothing to check yet. Map your EQ-5D columns on the Data page and ",
          "the checks will appear here."))))
      }
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          bslib::card_header("Data checks"),
          bslib::card_body(fillable = FALSE, shiny::uiOutput(ns("validation_msgs")))
        ),
        bslib::card(
          full_screen = TRUE, fill = FALSE,
          bslib::card_header("Processed data"),
          bslib::card_body(fillable = FALSE,
            shiny::uiOutput(ns("processed_info")),
            DT::DTOutput(ns("processed_table"))
          )
        )
      )
    })

    # ── Render: validation messages ───────────────────────────────────────────
    output$validation_msgs <- shiny::renderUI({
      v <- validation()
      msgs <- lapply(seq_len(nrow(v)), function(i)
        list(type = v$type[i], text = v$message[i]))
      shiny::tagList(
        lapply(msgs, function(msg) {
          cls <- switch(msg$type,
            ok      = "note note-ok",
            warning = "note note-warning",
            error   = "note note-error",
            "note note-info"
          )
          icon <- switch(msg$type,
            ok = "circle-check", warning = "triangle-exclamation",
            error = "circle-exclamation", "circle-info"
          )
          shiny::div(class = cls, role = "alert",
                     shiny::icon(icon), " ", msg$text)
        })
      )
    })

    # ── Render: mapping summary ───────────────────────────────────────────────
    output$mapping_summary <- shiny::renderUI({
      m <- rv$mapping
      shiny::req(m)
      items <- list(
        list("EQ-5D version", m$eq5d_version),
        list("Dimensions",    paste(m$names_eq5d, collapse = ", ")),
        list("Timepoint",     m$name_fu       %||% "(not mapped)"),
        list("Group",         m$name_groupvar %||% "(not mapped)"),
        list("Patient ID",    m$name_id       %||% "(not mapped)"),
        list("EQ VAS",        m$name_vas      %||% "(not mapped)"),
        list("Age",           m$name_age      %||% "(not mapped)"),
        list("Sex",           m$name_sex      %||% "(not mapped)"),
        list("EQ-5D value",   m$name_utility  %||% "(not mapped)")
      )
      shiny::tags$dl(
        class = "map-summary",
        lapply(items, function(x) {
          shiny::tagList(
            shiny::tags$dt(x[[1]]),
            shiny::tags$dd(x[[2]])
          )
        })
      )
    })

    # ── Reactive: apply mapping to produce processed_data preview ─────────────
    preview_data <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      eq5dsuite:::eq5d_apply_mapping(rv$raw_data, rv$mapping)
    })

    # Use rv$processed_data when available (preserves utility columns added on
    # the Data page), otherwise fall back to the freshly-mapped preview.
    current_data <- shiny::reactive({
      if (!is.null(rv$processed_data)) rv$processed_data else preview_data()
    })

    output$processed_info <- shiny::renderUI({
      df <- current_data()
      hint(shiny::strong(format(nrow(df), big.mark = ",")), " rows \u00d7 ",
           shiny::strong(ncol(df)), " columns",
           if (nrow(df) > 200L) " \u2014 first 200 shown" else "")
    })

    output$processed_table <- DT::renderDT({
      df <- current_data()
      shiny::req(df)
      DT::datatable(
        head(df, 200L),
        options  = list(pageLength = 10L, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Proceed: finalise processed_data and store in rv ──────────────────────
    shiny::observeEvent(input$proceed, {
      v <- validation()
      has_error <- any(v$type == "error")
      if (has_error) {
        shiny::showNotification(
          "Please resolve data errors before proceeding.",
          type = "error", duration = 5
        )
        return()
      }
      # Preserve any utility columns already added on the Data page.
      # Only set from raw mapping if processed_data not yet initialised.
      if (is.null(rv$processed_data)) {
        rv$processed_data <- preview_data()
      }
      shiny::showNotification(
        "Dataset validated.", type = "message", duration = 3
      )
      # Next in the workflow is calculating EQ-5D values. The profile and EQ
      # VAS analyses do not need them, so the page says so rather than this
      # being a required step.
      goto_page(session, "values")
    })
  })
}
