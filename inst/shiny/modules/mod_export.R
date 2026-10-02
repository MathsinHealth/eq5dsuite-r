# mod_export.R — Export module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Export",
    sidebar = shiny::tagList(
      hint("Every saved result, in the order set on the Results page."),
      shiny::downloadButton(ns("download_docx"), "Word report (.docx)",
                            class = "btn-primary w-100 mb-2"),
      shiny::downloadButton(ns("download_all_zip"), "Tables and figures (.zip)",
                            class = "btn-outline-secondary w-100 mb-2"),
      shiny::downloadButton(ns("download_script"), "R script (.R)",
                            class = "btn-outline-secondary w-100"),
      disclosure(
        "About the R script",
        shiny::p("An R script that repeats this session: loading the data, ",
                 "the checks, the EQ-5D values, and every saved result in the ",
                 "order below."),
        shiny::p("It calls eq5dsuite's analysis functions and writes ",
                 "everything else out in full, so it runs on its own ",
                 "wherever the package is installed, and you can read and ",
                 "change every step.")),
      disclosure(
        "About the Word report",
        shiny::p("One section per result, in order, each with its title and ",
                 "its table or figure."),
        shiny::p("The document uses the template bundled with the package, ",
                 "so it carries the same styles, header and footer as other ",
                 "Maths in Health reports."))
    ),
    shiny::tagList(
      bslib::card(
        fill = FALSE,
        bslib::card_header("Results"),
        bslib::card_body(fillable = FALSE, shiny::uiOutput(ns("export_list")))
      ),
      bslib::card(
        fill = FALSE,
        bslib::card_header("R script"),
        bslib::card_body(fillable = FALSE,
          hint("What the download contains."),
          shiny::verbatimTextOutput(ns("script_preview")))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_export_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Export list ───────────────────────────────────────────────────────────
    output$export_list <- shiny::renderUI({
      results <- rv$results
      if (length(results) == 0L) {
        return(hint("Nothing to export yet. Run an analysis first."))
      }
      
      rows <- lapply(seq_along(results), function(i) {
        r    <- results[[i]]
        dl_buttons <- list()
        
        if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
          dl_buttons <- c(dl_buttons, list(
            shiny::downloadButton(
              ns(paste0("dl_csv_", i)),
              label = "CSV",
              class = "btn-sm btn-outline-secondary me-1"
            )
          ))
        }
        
        if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
          dl_buttons <- c(dl_buttons, list(
            shiny::downloadButton(
              ns(paste0("dl_png_", i)),
              label = "PNG",
              class = "btn-sm btn-outline-secondary me-1"
            ),
            shiny::downloadButton(
              ns(paste0("dl_pdf_", i)),
              label = "PDF",
              class = "btn-sm btn-outline-secondary"
            )
          ))
        }
        
        shiny::div(
          class = "export-row",
          shiny::div(
            shiny::span(class = "result-n", i), " ",
            shiny::strong(r$label),
            shiny::tags$br(),
            shiny::tags$small(
              class = "hint",
              format(r$timestamp, "%d %b %Y %H:%M"),
              " \u2014 ", r$result_type
            )
          ),
          shiny::div(dl_buttons)
        )
      })
      
      shiny::tagList(rows)
    })
    
    # ── Dynamic download handlers ─────────────────────────────────────────────
    # We observe rv$results and register handlers each time it changes.
    shiny::observe({
      results <- rv$results
      lapply(seq_along(results), function(i) {
        r <- results[[i]]
        
        # CSV download
        if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
          local({
            local_r <- r
            local_i <- i
            output[[paste0("dl_csv_", local_i)]] <- shiny::downloadHandler(
              filename = function() {
                paste0(safe_filename(local_r$label), "_",
                       format(local_r$timestamp, "%Y%m%d"), ".csv")
              },
              content = function(file) {
                utils::write.csv(local_r$data, file, row.names = FALSE)
              }
            )
          })
        }
        
        # PNG download
        if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
          local({
            local_r <- r
            local_i <- i
            output[[paste0("dl_png_", local_i)]] <- shiny::downloadHandler(
              filename = function() {
                paste0(safe_filename(local_r$label), "_",
                       format(local_r$timestamp, "%Y%m%d"), ".png")
              },
              content = function(file) {
                ggplot2::ggsave(file, plot = local_r$plot,
                                width = 8, height = 5, dpi = 150)
              }
            )
          })
          
          # PDF download
          local({
            local_r <- r
            local_i <- i
            output[[paste0("dl_pdf_", local_i)]] <- shiny::downloadHandler(
              filename = function() {
                paste0(safe_filename(local_r$label), "_",
                       format(local_r$timestamp, "%Y%m%d"), ".pdf")
              },
              content = function(file) {
                ggplot2::ggsave(file, plot = local_r$plot,
                                width = 8, height = 5, device = "pdf")
              }
            )
          })
        }
      })
    })
    
    # ── R script ──────────────────────────────────────────────────────────────
    script_lines <- shiny::reactive({
      eq5dsuite:::script_from_session(rv$steps %||% list(), rv$results)
    })

    output$script_preview <- shiny::renderText(
      paste(script_lines(), collapse = "\n"))

    output$download_script <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5d_analysis_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) writeLines(script_lines(), file)
    )

    # ── Word report ───────────────────────────────────────────────────────────
    output$download_docx <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      },
      content = function(file) {
        results <- rv$results
        if (length(results) == 0L)
          stop("There are no saved results to put in the report.", call. = FALSE)
        # The same formatting the Analysis page shows on screen, so the
        # document and the app do not disagree about what a number means.
        write_results_docx(results, file,
                           format_table = eq5dsuite:::eq5d_format_table)
      }
    )

    # ── Bulk ZIP download (tables + plots) ────────────────────────────────────
    output$download_all_zip <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        results  <- rv$results
        # A name no other session can collide with, removed once the archive
        # is built rather than left in tempdir() for the life of the process.
        # Inside this session's own folder, which goes when the session does.
        tmp_dir  <- session_file(session, "eq5dzip_", "")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        out_files <- character(0L)
        
        # Tables → CSV
        for (r in results) {
          if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
            fname <- file.path(tmp_dir, paste0(safe_filename(r$label), ".csv"))
            utils::write.csv(r$data, fname, row.names = FALSE)
            out_files <- c(out_files, fname)
          }
        }
        
        # Plots → PNG
        for (r in results) {
          if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
            fname <- file.path(tmp_dir, paste0(safe_filename(r$label), ".png"))
            tryCatch(
              ggplot2::ggsave(fname, plot = r$plot, width = 8, height = 5, dpi = 150),
              error = function(e) NULL
            )
            if (file.exists(fname) && file.size(fname) > 0L) {
              out_files <- c(out_files, fname)
            }
          }
        }
        
        if (length(out_files) == 0L) {
          placeholder <- file.path(tmp_dir, "no_results.txt")
          writeLines("No results to export.", placeholder)
          out_files <- placeholder
        }
        
        utils::zip(file, files = out_files, flags = "-j")
      }
    )
    
  })
}

# ── Helper ────────────────────────────────────────────────────────────────────

safe_filename <- function(label) {
  gsub("[^A-Za-z0-9_-]", "_", label)
}
