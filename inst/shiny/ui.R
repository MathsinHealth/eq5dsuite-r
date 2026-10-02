# ui.R — main UI definition

navbar_brand <- shiny::tags$span(
  class = "brand",
  shiny::tags$img(src = "logo.png", height = "24px"),
  shiny::tags$strong("eq5dsuite")
)

nav_item <- function(icon, label, value, content) {
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon(icon), " ", label),
    value = value,
    content
  )
}

ui <- bslib::page_navbar(
  id    = "main_nav",
  title = navbar_brand,
  # The house style: a warm off-white ground, terracotta for the things you
  # act on, a serif for headings. Colours sampled from the reference app.
  theme = bslib::bs_theme(
    version      = 5,
    bg           = "#FAF8F5",
    fg           = "#3A3330",
    primary      = "#A87968",
    secondary    = "#7A9BB5",
    base_font    = c("Nunito Sans", "Source Sans Pro", "Segoe UI",
                     "system-ui", "-apple-system", "Helvetica Neue",
                     "Arial", "sans-serif"),
    heading_font = c("Georgia", "Iowan Old Style", "Palatino Linotype",
                     "Book Antiqua", "Times New Roman", "serif")
  ),
  # A light bar, not the dark one bslib gives a dark primary, and the page you
  # are on marked by a filled pill rather than an underline. bg/inverse are
  # deprecated in bslib 0.9; navbar_options() is the way now.
  navbar_options = bslib::navbar_options(
    bg = "#F5F0EA", theme = "light", underline = FALSE
  ),
  window_title = "eq5dsuite",
  # Pages scroll normally; nothing scrolls inside a box.
  fillable = FALSE,
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # The idle watch is loaded only when it is going to be used.
    if (ONLINE$enabled)
      shiny::tags$script(src = "idle.js")
  ),

  # In the order the work is done.
  nav_item("house",      "Home",       "home",       mod_home_ui("home")),
  nav_item("upload",     "Data",       "data",       mod_data_ui("data")),
  nav_item("list-check", "Validation", "validation", mod_validation_ui("validation")),
  nav_item("calculator", "Calculate EQ-5D values", "values", mod_values_ui("values")),
  nav_item("chart-bar",  "Analysis",   "analysis",   mod_analysis_ui("analysis")),
  nav_item("table-list", "Results",    "results",    mod_results_ui("results")),
  nav_item("download",   "Export",     "export",     mod_export_ui("export")),

  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::tags$a(class = "nav-ext",
                  href = "https://github.com/MathsInHealth/eq5dsuite",
                  target = "_blank", "eq5dsuite package")
  )
)
