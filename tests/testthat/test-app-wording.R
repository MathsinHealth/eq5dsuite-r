# Choosing columns is "selecting variables", not "mapping".
#
# In EuroQol research a mapping is a method that predicts one instrument's
# values from another's (the NICE DSU's UK mapping is one), so the app no
# longer calls choosing columns "mapping". That scientific sense keeps the
# word.

q <- function(expr) suppressWarnings(suppressMessages(expr))

app_text <- function() {
  files <- c(file.path(app_dir(), c("ui.R", "server.R", "global.R")),
             list.files(file.path(app_dir(), "modules"), "\\.R$",
                        full.names = TRUE))
  paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
}

test_that("the Data page confirms variables, and says so", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  said <- character(0)
  local_mocked_bindings(
    showNotification = function(ui, ...) said <<- c(said, as.character(ui)),
    .package = "shiny")
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    html <- as.character(output$confirm_ui$html)
    expect_match(html, "Confirm variables", fixed = TRUE)
    expect_false(grepl("Confirm mapping", html, fixed = TRUE))
    session$setInputs(version = "3L", col_mo = "mo", col_sc = "sc",
                      col_ua = "ua", col_pd = "pd", col_ad = "ad",
                      col_fu = "time", col_groupvar = "", col_id = "id",
                      col_vas = "", col_age = "", col_sex = "",
                      col_utility = "", fu_order = c("Pre-op", "Post-op"),
                      confirm = 1)
  }))
  expect_true(any(grepl("^Variables confirmed\\.", said)))
  expect_false(any(grepl("Mapping confirmed", said, fixed = TRUE)))
})

test_that("the Validation page heads its summary 'Variables'", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    html <- as.character(output$sidebar$html)
    expect_match(html, '>Variables</p>', fixed = TRUE)
    expect_false(grepl(">Mapping<", html, fixed = TRUE))
    expect_false(grepl("not mapped", as.character(output$mapping_summary$html),
                       fixed = TRUE))
  })
})

test_that("no user-facing text calls choosing columns 'mapping'", {
  skip_unless_app()
  txt <- app_text()
  for (old in c("Confirm mapping", "Mapping confirmed", "map its columns",
                "Map your EQ-5D columns", "Map it on the Data page",
                "\"Mapping\"", "(not mapped)", "column mapping first",
                "Your columns are mapped"))
    expect_false(grepl(old, txt, fixed = TRUE), info = old)
  # The scientific mapping keeps its name.
  expect_match(txt, "NICE DSU UK mapping", fixed = TRUE)
})
