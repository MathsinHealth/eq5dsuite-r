# Q01 and Q08 (review of 2026-10-04): the app and the generated script read
# the data the same way, and neither truncates or reads factor codes.
#
# Q01: eq5d_apply_mapping() turned a fractional level into an integer before
# anything checked it, so mo = 1.9 became level 1 and scored as full health in
# the app, while eq5d_validate() said it would be NA and eq5d() on the raw
# values gave NA.
# Q08: the generated script converted with as.integer() / as.numeric(), which
# read a factor's level codes: an RDS upload with factor("3") dimensions,
# VAS factor("80") and age factor("30") was 33333 / 80 / 30 in the app and
# 11111 / 1 / 1 in the script, so every value and saved result differed.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")

# ── The parsers ───────────────────────────────────────────────────────────────

test_that("levels are parsed from labels and validated before any integer", {
  # Whole levels within the instrument, from numbers, text or factor labels.
  expect_identical(.parse_levels(c(1, 2, 3), 3L), c(1L, 2L, 3L))
  expect_identical(.parse_levels(c("1", "3"), 3L), c(1L, 3L))
  expect_identical(.parse_levels(factor("3"), 3L), 3L)          # code 1
  expect_identical(.parse_levels(factor(c("3", "1"), levels = c("3", "1")), 3L),
                   c(3L, 1L))
  # Everything else is NA, never truncated or recoded.
  bad <- list(1.9, 2.0000001, 0, 4, -1, Inf, -Inf, NaN, NA, "1.9", "abc", "",
              factor("1.9"), factor("x"))
  for (b in bad) expect_identical(.parse_levels(b, 3L), NA_integer_,
                                  info = format(b))
  # The instrument decides the range.
  expect_identical(.parse_levels(c(4, 5), 5L), c(4L, 5L))
  expect_identical(.parse_levels(c(4, 5), 3L), c(NA_integer_, NA_integer_))
  # Neighbours and order are kept.
  expect_identical(.parse_levels(c(1, 1.9, 2, 9, 3), 3L), c(1L, NA, 2L, NA, 3L))
})

test_that("numbers are parsed from labels, exactly", {
  expect_identical(.parse_number(factor(c("80", "30.5"))), c(80, 30.5))
  expect_identical(.parse_number(c("80", "x", NA)), c(80, NA, NA))
  x <- 0.1 + 0.2
  expect_identical(.parse_number(x), x)                    # no text round trip
  expect_identical(.parse_number(5L), 5)
})

test_that("eq5d_apply_mapping() leaves an invalid level NA, for each instrument", {
  for (ver in c("3L", "5L", "Y3L")) {
    hi <- if (ver == "5L") 5 else 3
    d <- data.frame(mo = c(1.9, 1, hi, hi + 1), sc = 1, ua = 1, pd = 1, ad = 1)
    m <- list(eq5d_version = ver, names_eq5d = DIMS)
    got <- eq5d_apply_mapping(d, m)
    expect_identical(got$mo, c(NA, 1L, as.integer(hi), NA), info = ver)
    # Character and factor forms of the same data give the same result.
    dc <- d; dc$mo <- as.character(d$mo)
    expect_identical(eq5d_apply_mapping(dc, m)$mo, got$mo, info = ver)
    df <- d; df$mo <- factor(as.character(d$mo))
    expect_identical(eq5d_apply_mapping(df, m)$mo, got$mo, info = ver)
  }
})

test_that("the review's two rows: NA then full health, in validation and scoring", {
  d <- data.frame(mo = c(1.9, 1), sc = 1, ua = 1, pd = 1, ad = 1)
  m <- list(eq5d_version = "3L", names_eq5d = DIMS)
  v <- eq5d_validate(d, m, quiet = TRUE)
  expect_true(any(grepl("will be set to NA", v$message)))
  p <- eq5d_apply_mapping(d, m)
  expect_identical(q(eq5d(p[DIMS], country = "GB", version = "3L")), c(NA, 1))
  expect_identical(q(eq5d(d[DIMS], country = "GB", version = "3L")), c(NA, 1))
})

test_that("the script's helpers are the package's own parsers", {
  skip_unless_app()
  src <- .script_parsers()
  env <- new.env()
  eval(parse(text = src), envir = env)
  inputs <- list(c(1, 1.9, 2, 9, Inf, NA), c("3", "x", "", "2.0"),
                 factor(c("3", "1", "1.9")), factor(c("80", "70")),
                 c(0.1 + 0.2, -3))
  for (x in inputs) {
    expect_identical(env$parse_number(x), .parse_number(x))
    for (k in c(3L, 5L))
      expect_identical(env$parse_levels(x, k), .parse_levels(x, k))
  }
})

# ── Through the app, then the script ──────────────────────────────────────────

# Load `d` as an upload, select the variables, validate, add a direct GB
# column and a NICE DSU column, save a value summary and a VAS summary, and
# return the session and the generated script's environment.
app_and_script <- function(e, d, ext = c("rds", "csv")) {
  ext <- match.arg(ext)
  path <- withr::local_tempfile(fileext = paste0(".", ext),
                                .local_envir = parent.frame())
  if (ext == "rds") saveRDS(d, path)
  else utils::write.csv(d, path, row.names = FALSE)
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L),
                              steps = list())
  rv[["t_path"]] <- path
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    p <- shiny::isolate(rv$t_path)
    session$setInputs(file = list(name = basename(p), datapath = p))
    session$setInputs(version = "3L", col_mo = "mo", col_sc = "sc",
                      col_ua = "ua", col_pd = "pd", col_ad = "ad",
                      col_fu = "", col_groupvar = "", col_id = "id",
                      col_vas = "vas", col_age = "age", col_sex = "sex",
                      col_utility = "", confirm = 1)
  }))
  q(shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    session$setInputs(proceed = 1)
  }))
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility_GB", add = 1)
    session$setInputs(method = "uk", age_col = "age", age_kind = "exact",
                      sex_col = "sex", male_value = "Male",
                      col_name = "utility_DSU_GB", add = 1)
  }))
  for (o in list(c("values", "31", "utility_GB"), c("values", "31", "utility_DSU_GB"),
                 c("vas", "21", ""))) {
    rv[["t_o"]] <- o
    q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
      o <- shiny::isolate(rv$t_o)
      session$setInputs(component = o[1])
      session$setInputs(output = o[2])
      if (nzchar(o[3])) session$setInputs(utility_col = o[3])
      session$setInputs(run = 1)
    }))
  }
  lines <- eq5dsuite:::script_from_session(shiny::isolate(rv$steps),
                                           shiny::isolate(rv$results))
  lines <- fill_data_path(lines, path)
  f <- withr::local_tempfile(fileext = ".R", .local_envir = parent.frame())
  writeLines(lines, f)
  env <- new.env(parent = globalenv())
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  q(sys.source(f, envir = env))
  list(rv = rv, env = env, lines = lines)
}

expect_same_session <- function(run) {
  app <- shiny::isolate(run$rv$processed_data)
  scr <- run$env$analysis_data
  for (col in c(DIMS, "vas", "utility_GB", "utility_DSU_GB"))
    expect_identical(scr[[col]], app[[col]], info = col)
  res <- shiny::isolate(run$rv$results)
  objs <- eq5dsuite:::.result_object_names(res)
  for (i in seq_along(res))
    expect_equal(run$env[[objs[i]]], res[[i]]$data, tolerance = 0,
                 info = objs[i])
}

factor_upload <- function() {
  data.frame(
    id  = 1:6,
    mo  = factor(c("3", "1", "2", "3", "1", "2")),
    sc  = factor(c("3", "1", "1", "2", "1", "2")),
    ua  = factor(c("3", "1", "2", "2", "1", "1")),
    pd  = factor(c("3", "2", "2", "3", "1", "2")),
    ad  = factor(c("3", "1", "1", "2", "1", "3")),
    vas = factor(c("80", "70", "95", "40", "100", "60")),
    age = factor(c("30", "45", "70", "52", "38", "61")),
    sex = factor(c("Male", "Female", "Male", "Female", "Male", "Female")))
}

test_that("a factor RDS upload: the script reproduces the app exactly", {
  skip_unless_app()
  e <- app_env()
  run <- app_and_script(e, factor_upload(), "rds")
  app <- shiny::isolate(run$rv$processed_data)
  # The app read labels, not codes: row 1 is 33333, VAS 80.
  expect_identical(app$mo[1], 3L)
  expect_identical(app$vas[1], 80)
  expect_equal(app$utility_GB[1], -0.594, tolerance = 1e-6)
  expect_false(anyNA(app$utility_DSU_GB))
  expect_same_session(run)
})

test_that("fractional responses are NA in the app and in the script", {
  skip_unless_app()
  e <- app_env()
  d <- factor_upload()
  d$mo <- c(1.9, 1, 2, 3, 1, 2)            # numeric, one fractional
  for (ext in c("rds", "csv")) {
    run <- app_and_script(e, d, ext)
    app <- shiny::isolate(run$rv$processed_data)
    expect_identical(app$mo, c(NA, 1L, 2L, 3L, 1L, 2L), info = ext)
    expect_true(is.na(app$utility_GB[1]), info = ext)
    expect_false(anyNA(app$utility_GB[-1]), info = ext)
    expect_same_session(run)
  }
})

test_that("the script still warns where validation does, before converting", {
  skip_unless_app()
  e <- app_env()
  d <- factor_upload()
  d$mo <- c(1.9, 1, 2, 3, 1, 9)
  run <- app_and_script(e, d, "rds")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(d, path)
  w <- testthat::capture_warnings(suppressMessages({
    f <- withr::local_tempfile(fileext = ".R")
    writeLines(fill_data_path(run$lines, path), f)
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    sys.source(f, envir = new.env(parent = globalenv()))
  }))
  expect_true(any(grepl("not a level of the instrument", w, fixed = TRUE)))
})
