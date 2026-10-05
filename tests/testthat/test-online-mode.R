# Online mode: what changes when the app is deployed on a public server, and
# that nothing changes when it is not.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")

# An app environment in one mode or the other.
online_env <- function(enabled = TRUE, ...) {
  withr::local_options(c(
    list(eq5dsuite.online.config = list(enabled = enabled)), list(...)),
    .local_envir = parent.frame())
  app_env()
}

# ---------------------------------------------------------------------------
# The setting itself
# ---------------------------------------------------------------------------

test_that("online mode is off unless asked for", {
  withr::local_envvar(c(EQ5DSUITE_ONLINE = NA))
  expect_false(eq5d_online_config()$enabled)
  expect_true(eq5d_online_config(online = TRUE)$enabled)
})

test_that("the environment variable turns it on, and the argument wins", {
  withr::local_envvar(c(EQ5DSUITE_ONLINE = "true"))
  expect_true(eq5d_online_config()$enabled)
  expect_false(eq5d_online_config(online = FALSE)$enabled)

  for (v in c("TRUE", "yes", "1", "on")) {
    withr::local_envvar(c(EQ5DSUITE_ONLINE = v))
    expect_true(eq5d_online_config()$enabled, info = v)
  }
  withr::local_envvar(c(EQ5DSUITE_ONLINE = "no"))
  expect_false(eq5d_online_config()$enabled)
})

test_that("the limits have the documented defaults", {
  withr::local_envvar(c(EQ5DSUITE_MAX_ROWS = NA, EQ5DSUITE_MAX_COLS = NA,
                        EQ5DSUITE_MAX_UPLOAD_MB = NA,
                        EQ5DSUITE_IDLE_MINUTES = NA,
                        EQ5DSUITE_ALLOWED_TYPES = NA))
  cfg <- eq5d_online_config(online = TRUE)
  expect_equal(cfg$max_upload_mb, 10)
  expect_equal(cfg$max_rows, 50000L)
  expect_equal(cfg$max_cols, 200L)
  expect_equal(cfg$idle_minutes, 30L)
  expect_equal(cfg$allowed_types, c("csv", "xlsx", "xls"))
  expect_false("rds" %in% cfg$allowed_types)
})

test_that("options beat environment variables, which beat the defaults", {
  withr::local_envvar(c(EQ5DSUITE_MAX_ROWS = "1234"))
  expect_equal(eq5d_online_config(online = TRUE)$max_rows, 1234L)

  withr::local_options(list(eq5dsuite.max_rows = 99))
  expect_equal(eq5d_online_config(online = TRUE)$max_rows, 99L)
})

test_that("the accepted types can be given as a list in one string", {
  withr::local_envvar(c(EQ5DSUITE_ALLOWED_TYPES = "csv, xlsx"))
  expect_equal(eq5d_online_config(online = TRUE)$allowed_types, c("csv", "xlsx"))
})

# ---------------------------------------------------------------------------
# The limits
# ---------------------------------------------------------------------------

test_that("a dataset within the limits is accepted", {
  expect_null(eq5d_check_upload(example_data, "csv",
                                eq5d_online_config(online = TRUE)))
})

test_that("too many rows is refused, saying the limit and what to do", {
  cfg <- within(eq5d_online_config(online = TRUE), max_rows <- 100L)
  msg <- eq5d_check_upload(example_data, "csv", cfg)

  expect_type(msg, "character")
  expect_match(msg, "10,000 rows", fixed = TRUE)   # what they gave
  expect_match(msg, "up to 100", fixed = TRUE)     # the limit
  expect_match(msg, "eq5dsuite::run_app()", fixed = TRUE)  # and the way round it
})

test_that("too many columns is refused", {
  cfg <- within(eq5d_online_config(online = TRUE), max_cols <- 3L)
  msg <- eq5d_check_upload(example_data, "csv", cfg)
  expect_match(msg, "12 columns", fixed = TRUE)
  expect_match(msg, "up to 3", fixed = TRUE)
})

test_that("a type that is not accepted is refused, .rds by name", {
  cfg <- eq5d_online_config(online = TRUE)
  expect_match(eq5d_check_upload(NULL, "rds", cfg), "not safe on a shared server")
  expect_match(eq5d_check_upload(NULL, "docx", cfg), "accepts CSV, XLSX, XLS")
  expect_match(eq5d_check_upload(NULL, "", cfg), "accepts CSV")
  expect_null(eq5d_check_upload(NULL, "csv", cfg))
  expect_null(eq5d_check_upload(NULL, "XLSX", cfg))   # case does not matter
})

test_that("nothing is checked locally", {
  cfg <- eq5d_online_config()   # off
  expect_null(eq5d_check_upload(example_data, "rds", cfg))
  expect_null(eq5d_check_upload(example_data, "anything", cfg))
  big <- example_data[rep(1, 10), ]
  expect_null(eq5d_check_upload(big, "csv", within(cfg, max_rows <- 1L)))
})

test_that("the app refuses an oversized upload after reading it", {
  skip_unless_app()
  e <- online_env(TRUE, eq5dsuite.max_rows = 100)

  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(example_data, path, row.names = FALSE)

  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(file = list(name = basename(path), datapath = path))
    # Refused: nothing was loaded and nothing was recorded.
    expect_null(uploaded())
    expect_length(rv$steps, 0L)
  }))
})

test_that("the app refuses a type the browser let through", {
  skip_unless_app()
  e <- online_env(TRUE)

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(example_data, path)

  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(file = list(name = "data.rds", datapath = path))
    expect_null(uploaded())
  }))
})

test_that("the same upload is accepted locally", {
  skip_unless_app()
  e <- online_env(FALSE)

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(example_data, path)

  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(file = list(name = "data.rds", datapath = path))
    expect_equal(nrow(uploaded()), 10000L)
  }))
})

# ---------------------------------------------------------------------------
# What run_app() sets
# ---------------------------------------------------------------------------

# run_app() without actually starting a server: capture the options that are
# in force at the moment it would hand over to shiny::runApp().
run_app_options <- function(...) {
  seen <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      seen <<- list(
        max_request  = getOption("shiny.maxRequestSize"),
        sanitize     = getOption("shiny.sanitize.errors"),
        config       = getOption("eq5dsuite.online.config"),
        active       = getOption("eq5dsuite.online.active"))
      invisible(NULL)
    },
    .package = "shiny")
  suppressMessages(run_app(...))
  seen
}

test_that("online mode sets the upload limit and sanitises errors", {
  skip_if_not_installed("shiny")
  withr::local_envvar(c(EQ5DSUITE_ONLINE = NA, EQ5DSUITE_MAX_UPLOAD_MB = NA))

  got <- run_app_options(online = TRUE)
  expect_equal(got$max_request, 10 * 1024^2)
  expect_true(got$sanitize)
  expect_true(got$config$enabled)
  expect_true(got$active)
})

test_that("the upload limit follows the setting", {
  skip_if_not_installed("shiny")
  withr::local_options(list(eq5dsuite.max_upload_mb = 3))
  expect_equal(run_app_options(online = TRUE)$max_request, 3 * 1024^2)
})

test_that("locally run_app() sets none of it", {
  skip_if_not_installed("shiny")
  withr::local_envvar(c(EQ5DSUITE_ONLINE = NA))
  withr::local_options(list(shiny.maxRequestSize = NULL,
                            shiny.sanitize.errors = NULL))

  got <- run_app_options()
  expect_null(got$max_request)
  expect_null(got$sanitize)
  expect_false(got$config$enabled)
  expect_false(got$active)
})

test_that("the options do not outlive the call", {
  skip_if_not_installed("shiny")
  before <- getOption("eq5dsuite.online.active")
  run_app_options(online = TRUE)
  expect_identical(getOption("eq5dsuite.online.active"), before)
})

# ---------------------------------------------------------------------------
# Temporary files
# ---------------------------------------------------------------------------

test_that("the session's files live in a folder of its own, and go with it", {
  skip_unless_app()
  e <- online_env(TRUE)
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B")),
                              steps = list())

  captured <- NULL
  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    # Building the archive stages files in the session's folder.
    zp <- output$download_all_zip
    expect_true(file.exists(zp))
    captured <<- session$userData$eq5d_dir
    expect_false(is.null(captured))
    expect_true(dir.exists(captured))
    # It is under tempdir(), not the app directory.
    expect_true(startsWith(normalizePath(captured),
                           normalizePath(tempdir())))
    expect_false(startsWith(normalizePath(captured),
                            normalizePath(app_dir())))
  })
})

test_that("the folder is deleted when the session ends", {
  skip_unless_app()
  e <- online_env(TRUE)

  # session_dir() creates it; onSessionEnded() removes it. Drive both.
  session <- shiny::MockShinySession$new()
  d <- e$session_dir(session)
  expect_true(dir.exists(d))

  session$onSessionEnded(function() {
    if (dir.exists(d)) unlink(d, recursive = TRUE, force = TRUE)
  })
  session$close()
  expect_false(dir.exists(d))
})

test_that("a session file never takes its name from the uploaded file", {
  skip_unless_app()
  e <- online_env(TRUE)
  session <- shiny::MockShinySession$new()
  f <- e$session_file(session, "eq5dzip_", ".zip")
  expect_match(basename(f), "^eq5dzip_")
  expect_true(startsWith(normalizePath(dirname(f), mustWork = FALSE),
                         normalizePath(e$session_dir(session))))
})

# ---------------------------------------------------------------------------
# Value sets
# ---------------------------------------------------------------------------

test_that("the app never adds, drops or updates a value set", {
  skip_unless_app()
  # Value sets live in an environment shared by the whole R process, so one
  # session changing them would change them for everyone. The app must only
  # read them. See DEPLOY.md.
  forbidden <- c("eqvs_add", "eqvs_drop", "update_value_sets")
  files <- list.files(app_dir(), pattern = "\\.R$", recursive = TRUE,
                      full.names = TRUE)

  called <- character(0L)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    hit <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% forbidden]
    if (length(hit)) called <- c(called, paste0(basename(f), ": ", hit))
  }
  expect_identical(called, character(0L))
})

test_that("update_value_sets() refuses while the app is online", {
  withr::local_options(list(eq5dsuite.online.active = TRUE))
  expect_error(update_value_sets(), "disabled while the app is running online")
})

test_that("update_value_sets() is unaffected locally", {
  withr::local_options(list(eq5dsuite.online.active = FALSE))
  # Not run for real -- it reaches the network -- but it must get past the
  # guard, which is the first thing in the body.
  expect_false(eq5dsuite:::.online_now())
})

# ---------------------------------------------------------------------------
# What users are told
# ---------------------------------------------------------------------------

test_that("the notices say what happens to the data, and can be replaced", {
  for (w in c("data", "upload", "local")) {
    txt <- eq5d_online_notice(w)
    expect_type(txt, "character")
    expect_gt(nchar(txt), 40L)
  }
  # Accurate about the disk: uploads are temporary files, deleted at the end
  # of the session; nothing claims they stay in memory (review I05).
  for (w in c("data", "upload")) {
    expect_match(eq5d_online_notice(w), "temporary files", fixed = TRUE)
    expect_match(eq5d_online_notice(w), "deleted when the session ends",
                 fixed = TRUE)
    expect_false(grepl("in memory|not stored", eq5d_online_notice(w)))
  }
  expect_match(eq5d_online_notice("data"), "de-identified")
  expect_match(eq5d_online_notice("local"), "eq5dsuite::run_app()", fixed = TRUE)

  withr::local_options(list(eq5dsuite.online.notice.data = "Our own wording."))
  expect_equal(eq5d_online_notice("data"), "Our own wording.")
})

test_that("the notices appear online and not locally", {
  skip_unless_app()
  on_html <- function(e, ui) paste(as.character(ui), collapse = "")

  eo <- online_env(TRUE)
  home <- on_html(eo, eo$mod_home_ui("home"))
  data <- on_html(eo, eo$mod_data_ui("data"))
  expect_match(home, "processed only for this session", fixed = TRUE)
  expect_match(home, "run the app on your own machine", fixed = TRUE)
  expect_match(data, "de-identified", fixed = TRUE)
  # The version is on the home page either way.
  expect_match(home, "version", fixed = TRUE)

  el <- online_env(FALSE)
  expect_false(grepl("processed only for this session",
                     on_html(el, el$mod_home_ui("home")), fixed = TRUE))
  expect_false(grepl("de-identified",
                     on_html(el, el$mod_data_ui("data")), fixed = TRUE))
})

test_that("the file picker offers .rds locally and not online", {
  skip_unless_app()
  eo <- online_env(TRUE)
  el <- online_env(FALSE)
  expect_false(grepl(".rds", paste(as.character(eo$mod_data_ui("data")),
                                   collapse = ""), fixed = TRUE))
  expect_true(grepl(".rds", paste(as.character(el$mod_data_ui("data")),
                                  collapse = ""), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Warnings stay with the user
# ---------------------------------------------------------------------------

test_that("an analysis warning is shown, not written to the log", {
  skip_unless_app()
  e <- online_env(TRUE)
  rv <- processed_rv(e)

  # example_data codes missing dimensions as 9, which the analyses warn about.
  # The warning must not escape: on a server, stderr is the log.
  expect_silent(
    shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
      session$setInputs(component = "profile", output = "111")
      session$setInputs(run = 1)
      expect_s3_class(res$data, "data.frame")
    }))
})

test_that("run_quietly() returns the value and swallows the noise", {
  skip_unless_app()
  e <- app_env()
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    expect_silent(got <- e$run_quietly({
      warning("a value from the data: Month 12")
      message("chatter")
      42
    }))
    expect_equal(got, 42)
  })
})

# ---------------------------------------------------------------------------
# Local mode is untouched
# ---------------------------------------------------------------------------

test_that("locally the app has no online furniture at all", {
  skip_unless_app()
  e <- online_env(FALSE)
  expect_false(e$ONLINE$enabled)
  expect_null(e$online_note("data"))
  expect_setequal(e$ONLINE$allowed_types_ui, c("csv", "xlsx", "xls", "rds"))

  # The idle watch is loaded only online.
  ui_src <- paste(readLines(file.path(app_dir(), "ui.R"), warn = FALSE),
                  collapse = "\n")
  expect_match(ui_src, "if (ONLINE$enabled)", fixed = TRUE)
})

test_that("every online branch hangs off the one setting", {
  skip_unless_app()
  files <- list.files(app_dir(), pattern = "\\.R$", recursive = TRUE,
                      full.names = TRUE)
  # Nothing reads the environment variable or the option directly; everything
  # goes through ONLINE, resolved once in global.R. Asked of the parsed code,
  # so the comment in global.R that names them is not a hit.
  stray <- character(0L)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    lits <- pd$text[pd$token == "STR_CONST"]
    hit <- grep("EQ5DSUITE_ONLINE|eq5dsuite[.]online[.]active", lits, value = TRUE)
    if (length(hit)) stray <- c(stray, paste0(basename(f), ": ", hit))
  }
  expect_identical(stray, character(0L))
})

# ---------------------------------------------------------------------------
# The deployment guide
# ---------------------------------------------------------------------------

test_that("DEPLOY.md ships, and documents every setting", {
  path <- system.file("shiny", "DEPLOY.md", package = "eq5dsuite")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "shiny",
                                                 "DEPLOY.md")
  skip_if(!file.exists(path), "DEPLOY.md not available")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # Every environment variable the code reads is named in the guide.
  for (v in c("EQ5DSUITE_ONLINE", "EQ5DSUITE_MAX_UPLOAD_MB",
              "EQ5DSUITE_MAX_ROWS", "EQ5DSUITE_MAX_COLS",
              "EQ5DSUITE_IDLE_MINUTES", "EQ5DSUITE_ALLOWED_TYPES"))
    expect_match(txt, v, fixed = TRUE)

  # And the server-side matters the app cannot set for itself.
  for (topic in c("client_max_body_size", "shiny-server.conf",
                  "simple_scheduler", "app_idle_timeout", "preserve_logs",
                  "sanitize", "proxy_read_timeout"))
    expect_match(txt, topic, fixed = TRUE)

  # The warning that matters most.
  expect_match(txt, "must never add, change or remove a value set", fixed = TRUE)
  expect_match(txt, ".rds` is not accepted online", fixed = TRUE)
})

test_that("DEPLOY.md gives an app.R that a server can actually run", {
  path <- system.file("shiny", "DEPLOY.md", package = "eq5dsuite")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "shiny",
                                                 "DEPLOY.md")
  skip_if(!file.exists(path), "DEPLOY.md not available")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # eq5d_app(), not run_app(): the server runs the directory itself, so an
  # app.R that called run_app() would be a runApp() inside a runApp().
  #
  # Three colons, because eq5d_app() is internal: it configures and returns
  # the app rather than analysing EQ-5D data, so it stays out of eq5dsuite::.
  # Two colons in app.R would not find it, which is the one way this guide
  # could leave a deployment dead on arrival.
  expect_match(txt, "eq5dsuite:::eq5d_app(online = TRUE)", fixed = TRUE)
  # And nothing a reader would copy says two. The prose does, once, to warn
  # against it, so only the fenced code blocks are checked.
  lines <- readLines(path, warn = FALSE)
  fence <- cumsum(grepl("^```", lines)) %% 2L == 1L
  code <- lines[fence & !grepl("^```", lines)]
  expect_false(any(grepl("eq5dsuite::eq5d_app", code, fixed = TRUE)))
  expect_match(txt, "`runApp()` inside `runApp()`", fixed = TRUE)
  expect_false(grepl("app.R\n\n```r\neq5dsuite::run_app", txt))
})

# ---------------------------------------------------------------------------
# eq5d_app(), the entry point a server uses
# ---------------------------------------------------------------------------

test_that("eq5d_app() returns the app rather than starting it", {
  skip_if_not_installed("shiny")
  withr::local_options(list(eq5dsuite.online.active = NULL,
                            eq5dsuite.online.config = NULL))
  app <- eq5d_app(online = FALSE)
  expect_s3_class(app, "shiny.appobj")
  # A server calls this; it must not have tried to listen on a port.
  expect_true(is.function(app$serverFuncSource) || !is.null(app$httpHandler))
})

test_that("eq5d_app() leaves the online settings in force", {
  skip_if_not_installed("shiny")
  withr::local_options(list(shiny.maxRequestSize = NULL,
                            shiny.sanitize.errors = NULL,
                            eq5dsuite.online.active = NULL,
                            eq5dsuite.online.config = NULL))
  withr::local_envvar(c(EQ5DSUITE_ONLINE = NA, EQ5DSUITE_MAX_UPLOAD_MB = NA))

  eq5d_app(online = TRUE)
  # Unlike run_app(), which restores them, these must hold for as long as the
  # process serves the app.
  expect_equal(getOption("shiny.maxRequestSize"), 10 * 1024^2)
  expect_true(getOption("shiny.sanitize.errors"))
  expect_true(getOption("eq5dsuite.online.active"))
  expect_true(getOption("eq5dsuite.online.config")$enabled)
})

test_that("eq5d_app() sets nothing locally", {
  skip_if_not_installed("shiny")
  withr::local_options(list(shiny.maxRequestSize = NULL,
                            shiny.sanitize.errors = NULL,
                            eq5dsuite.online.active = NULL,
                            eq5dsuite.online.config = NULL))
  withr::local_envvar(c(EQ5DSUITE_ONLINE = NA))

  eq5d_app(online = FALSE)
  expect_null(getOption("shiny.maxRequestSize"))
  expect_null(getOption("shiny.sanitize.errors"))
  expect_false(getOption("eq5dsuite.online.active"))
})

test_that("eq5d_app() honours the limits a deployment sets", {
  skip_if_not_installed("shiny")
  # Every option eq5d_app() sets is restored afterwards. This test once left
  # eq5dsuite.online.active on, so every later test file ran "online" and
  # update_value_sets() refused to run in them.
  withr::local_options(list(eq5dsuite.max_upload_mb = 4,
                            shiny.maxRequestSize = NULL,
                            shiny.sanitize.errors = NULL,
                            eq5dsuite.online.active = NULL,
                            eq5dsuite.online.config = NULL))
  eq5d_app(online = TRUE)
  expect_equal(getOption("shiny.maxRequestSize"), 4 * 1024^2)
})

test_that("the online-mode tests leave no option behind", {
  # Run last in this file: nothing above may leave online mode in force for
  # the test files that follow.
  expect_null(getOption("eq5dsuite.online.active"))
  expect_null(getOption("eq5dsuite.online.config"))
})

