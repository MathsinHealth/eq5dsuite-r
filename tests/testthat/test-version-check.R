# The app's version label and the check for a newer version on CRAN.
#
# Every request here is mocked: .cran_request() is replaced, so no test
# reaches the network. Each test starts from a clean, process-wide state.

desc_text <- function(version, package = "eq5dsuite")
  paste0("Package: ", package, "\nVersion: ", version, "\nTitle: x\n")

# A request that answers at once with `version`, counting how often it is made.
mock_cran <- function(version = NULL, status = 200L, failure = NULL,
                      env = parent.frame()) {
  calls <- new.env(); calls$n <- 0L
  local_mocked_bindings(
    .cran_request = function(url, done, fail, pool) {
      calls$n <- calls$n + 1L
      if (!is.null(failure)) return(fail(failure))
      done(list(status_code = status,
                content = charToRaw(desc_text(version))))
    },
    .cran_pump = function(pool) invisible(NULL),
    .env = env)
  calls
}

local_clean_state <- function(env = parent.frame()) {
  .cran_check_reset()
  withr::defer(.cran_check_reset(), envir = env)
}

test_that("the version on CRAN is read from its DESCRIPTION", {
  expect_identical(.parse_cran_version(desc_text("2.1.0")),
                   package_version("2.1.0"))
  expect_error(.parse_cran_version("Package: eq5dsuite\n"), "Version")
  expect_error(.parse_cran_version(desc_text("2.1.0", "otherpkg")), "eq5dsuite")
  expect_error(.parse_cran_version("<html>404</html>"))
})

test_that("versions are compared as versions, not as text", {
  local_clean_state()
  mock_cran("2.10.0")
  .cran_check_start()
  # "2.10.0" < "2.9.0" as strings; as versions it is newer.
  expect_true(.cran_update_status("2.9.0")$newer)
  expect_false(.cran_update_status("2.10.0")$newer)   # the same
  expect_false(.cran_update_status("2.10.1")$newer)   # ahead of CRAN
})

test_that("a newer version on CRAN is reported with both versions", {
  local_clean_state()
  mock_cran("9.0.0")
  .cran_check_start()
  s <- .cran_update_status("2.1.0")
  expect_identical(s$state, "done")
  expect_true(s$newer)
  expect_identical(s$cran, package_version("9.0.0"))
})

test_that("a failed check is quiet and claims nothing", {
  for (case in list(list(failure = "Could not resolve host"),
                    list(version = "9.0.0", status = 404L),
                    list(version = "not a version"))) {
    local_clean_state()
    do.call(mock_cran, case)
    .cran_check_start()
    s <- .cran_update_status("2.1.0")
    expect_identical(s$state, "failed")
    expect_false(s$newer)
    expect_null(s$cran)
  }
})

test_that("a request that cannot even be queued fails quietly", {
  local_clean_state()
  local_mocked_bindings(.cran_request = function(...) stop("no network"))
  expect_no_error(.cran_check_start())
  expect_identical(.cran_update_status()$state, "failed")
})

test_that("CRAN is asked once, not again until the answer is old", {
  local_clean_state()
  calls <- mock_cran("2.1.0")
  t0 <- Sys.time()
  for (i in 1:5) .cran_check_start(now = t0)
  expect_identical(calls$n, 1L)
  # Still fresh after 23 hours; asked again after 25.
  .cran_check_start(now = t0 + 23 * 3600)
  expect_identical(calls$n, 1L)
  .cran_check_start(now = t0 + 25 * 3600)
  expect_identical(calls$n, 2L)
})

test_that("a failure is retried after an hour, not at once", {
  local_clean_state()
  calls <- mock_cran(failure = "timeout")
  t0 <- Sys.time()
  .cran_check_start(now = t0)
  .cran_check_start(now = t0 + 30 * 60)
  expect_identical(calls$n, 1L)
  .cran_check_start(now = t0 + 61 * 60)
  expect_identical(calls$n, 2L)
})

test_that("a check under way is not started twice, and is advanced by polling", {
  local_clean_state()
  calls <- new.env(); calls$n <- 0L; calls$pumped <- 0L
  local_mocked_bindings(
    .cran_request = function(url, done, fail, pool) {
      calls$n <- calls$n + 1L
      calls$done <- done           # answered later, by the pump
    },
    .cran_pump = function(pool) {
      calls$pumped <- calls$pumped + 1L
      if (calls$pumped == 2L)
        calls$done(list(status_code = 200L,
                        content = charToRaw(desc_text("9.0.0"))))
    })
  .cran_check_start(); .cran_check_start()
  expect_identical(calls$n, 1L)
  expect_identical(.cran_check_poll(), "pending")
  expect_identical(.cran_check_poll(), "done")
  expect_true(.cran_update_status("2.1.0")$newer)
  # Once answered, polling does not pump again.
  .cran_check_poll()
  expect_identical(calls$pumped, 2L)
})

test_that("an error while pumping fails quietly", {
  local_clean_state()
  local_mocked_bindings(
    .cran_request = function(...) invisible(NULL),
    .cran_pump = function(pool) stop("connection reset"))
  .cran_check_start()
  expect_identical(.cran_check_poll(), "failed")
  expect_false(.cran_update_status()$newer)
})

# ── The app ───────────────────────────────────────────────────────────────────

test_that("the navbar shows 'eq5dsuite v' and the installed version", {
  skip_unless_app()
  e <- app_env()
  html <- as.character(e$mod_version_ui("version"))
  expect_match(html, paste0("eq5dsuite v", utils::packageVersion("eq5dsuite")),
               fixed = TRUE)
  expect_false(grepl("eq5dsuite package", html, fixed = TRUE))
  ui <- paste(readLines(file.path(app_dir(), "ui.R"), warn = FALSE),
              collapse = "\n")
  expect_false(grepl("eq5dsuite package", ui, fixed = TRUE))
})

test_that("a newer CRAN version shows an icon, and clicking it says which", {
  skip_unless_app()
  local_clean_state()
  withr::local_options(list(eq5dsuite.check_cran = TRUE))
  mock_cran("99.0.0")
  shown <- NULL
  local_mocked_bindings(showModal = function(ui, ...) shown <<- ui,
                        .package = "shiny")
  e <- app_env()
  shiny::testServer(e$mod_version_server, {
    session$flushReact()
    html <- as.character(output$update$html)
    expect_match(html, "circle-arrow-up", fixed = TRUE)
    session$setInputs(info = 1)
  })
  expect_match(as.character(shown),
               e$update_message(utils::packageVersion("eq5dsuite"), "99.0.0"),
               fixed = TRUE)
  expect_match(as.character(shown), paste0(
    "You are using eq5dsuite version ", utils::packageVersion("eq5dsuite"),
    ". Version 99.0.0 is available on CRAN."), fixed = TRUE)
})

test_that("no icon when up to date, when the check fails, or when it is off", {
  skip_unless_app()
  e <- app_env()
  installed <- as.character(utils::packageVersion("eq5dsuite"))
  for (case in list(list(version = installed), list(failure = "offline"))) {
    local_clean_state()
    withr::local_options(list(eq5dsuite.check_cran = TRUE))
    do.call(mock_cran, case)
    shiny::testServer(e$mod_version_server, {
      session$flushReact()
      expect_null(output$update$html)
    })
  }
  # Turned off: no request is made at all.
  local_clean_state()
  withr::local_options(list(eq5dsuite.check_cran = FALSE))
  calls <- mock_cran("99.0.0")
  shiny::testServer(e$mod_version_server, session$flushReact())
  expect_identical(calls$n, 0L)
})
