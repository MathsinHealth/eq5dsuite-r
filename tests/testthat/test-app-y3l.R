# EQ-5D-Y-3L data in the app. The package values and analyses EQ-5D-Y-3L data
# (eq5dy3l(), and eq5d_version = "Y3L" in every analysis function), but the
# Data page offered only EQ-5D-3L and EQ-5D-5L. Choosing EQ-5D-Y-3L must offer
# the EQ-5D-Y-3L value sets and no crosswalk or mapping -- none exists for it
# -- and leave the analyses as they are.

q <- function(expr) suppressWarnings(suppressMessages(expr))

Y3L_CODES <- function()
  q(eqvs_display(version = "Y3L", return_df = TRUE))$VS_code

new_rv <- function()
  shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                        processed_data = NULL, results = list(),
                        load_example = 0L, value_cols = character(0L),
                        steps = list())

# example_data through Data and Validation, as EQ-5D-Y-3L (its levels are
# 1-3, with 9 for missing, so it stands in for EQ-5D-Y-3L data).
confirm_as <- function(e, version, rv = new_rv()) {
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    session$setInputs(version = version, col_mo = "mo", col_sc = "sc",
                      col_ua = "ua", col_pd = "pd", col_ad = "ad",
                      col_fu = "time", col_groupvar = "procedure",
                      col_id = "id", col_vas = "vas", col_age = "ageband",
                      col_sex = "gender", col_utility = "",
                      fu_order = c("Pre-op", "Post-op"), confirm = 1)
  }))
  q(shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    session$setInputs(proceed = 1)
  }))
  rv
}

option_values <- function(html, id) {
  sel <- regmatches(html, regexpr(paste0('<select[^>]*', id, '.*?</select>'),
                                  html))
  m <- regmatches(sel, gregexpr('value="[^"]*"', sel))[[1L]]
  gsub('^value="|"$', "", m)
}

test_that("the Data page offers EQ-5D-Y-3L, and the choice is kept", {
  skip_unless_app()
  e <- app_env()
  rv <- new_rv()
  q(shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    html <- as.character(output$mapping_ui$html)
    expect_match(html, 'value="Y3L"', fixed = TRUE)
    expect_match(html, "EQ-5D-Y-3L", fixed = TRUE)
  }))
  confirm_as(e, "Y3L", rv)
  expect_identical(shiny::isolate(rv$mapping$eq5d_version), "Y3L")
  expect_false(is.null(shiny::isolate(rv$processed_data)))
})

test_that("for EQ-5D-Y-3L data, only a direct value set is offered", {
  skip_unless_app()
  e <- app_env()
  expect_identical(unname(e$get_utility_method_choices("Y3L")), "direct")
  expect_match(names(e$get_utility_method_choices("Y3L")), "Y-3L", fixed = TRUE)
  # 3L and 5L keep their crosswalks and the NICE DSU mapping.
  expect_identical(unname(e$get_utility_method_choices("3L")),
                   c("direct", "xwr", "uk"))
  expect_identical(unname(e$get_utility_method_choices("5L")),
                   c("direct", "xw", "uk"))

  rv <- confirm_as(e, "Y3L")
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    html <- as.character(output$controls$html)
    expect_identical(option_values(html, "method"), "direct")
    expect_false(grepl("rosswalk 3L|NICE DSU UK mapping 3L", html))
  }))
})

test_that("the EQ-5D-Y-3L value sets are listed, and only those", {
  skip_unless_app()
  e <- app_env()
  rv <- confirm_as(e, "Y3L")
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct")
    html <- as.character(output$method_opts$html)
    expect_setequal(setdiff(option_values(html, "country"), ""), Y3L_CODES())
    expect_match(html, "EQ-5D-Y-3L value set", fixed = TRUE)
    expect_match(as.character(output$direction$html),
                 "EQ-5D-Y-3L responses are valued on an EQ-5D-Y-3L value set",
                 fixed = TRUE)
  }))
})

test_that("an EQ-5D-Y-3L value column holds eq5dy3l()'s values", {
  skip_unless_app()
  e <- app_env()
  rv <- confirm_as(e, "Y3L")
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "DE",
                      col_name = "utility_DE", add = 1)
  }))
  d <- shiny::isolate(rv$processed_data)
  expect_identical(d$utility_DE, q(eq5dy3l(d[, DIMS], country = "DE")))
  expect_gt(sum(!is.na(d$utility_DE)), 0L)
  # A set published only for the EQ-5D-3L is refused.
  before <- d
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "US",
                      col_name = "utility_US", add = 1)
  }))
  expect_false("utility_US" %in% names(shiny::isolate(rv$processed_data)))
})

test_that("levels 4 and 5 are invalid in EQ-5D-Y-3L data, as in EQ-5D-3L", {
  skip_unless_app()
  e <- app_env()
  d <- data.frame(mo = c(1, 4, 5), sc = 1, ua = 1, pd = 1, ad = 1)
  m <- list(eq5d_version = "Y3L", names_eq5d = DIMS)
  utils::capture.output(v <- q(eq5d_validate(d, m)))
  txt <- paste(v$message, collapse = " ")
  expect_match(txt, "from 1 to 3", fixed = TRUE)
  expect_false(grepl("within expected range", txt, fixed = TRUE))
  rv <- list(mapping = m)
  expect_identical(e$valid_profiles(d, rv), c(TRUE, FALSE, FALSE))
})

test_that("the analyses are those of EQ-5D-3L data, run as EQ-5D-Y-3L", {
  skip_unless_app()
  e <- app_env()
  run <- function(version, out, comp) {
    rv <- confirm_as(e, version)
    q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
      session$setInputs(component = comp)
      session$setInputs(output = out)
      session$setInputs(run = 1)
    }))
    r <- shiny::isolate(rv$results)
    expect_length(r, 1L)
    r[[1L]]
  }
  for (o in list(c("111", "profile"), c("21", "vas"), c("141", "profile"))) {
    y <- run("Y3L", o[1], o[2]); t3 <- run("3L", o[1], o[2])
    # The call shown and scripted names the instrument chosen.
    if (grepl('"3L"', t3$fn_call, fixed = TRUE))
      expect_match(y$fn_call, '"Y3L"', fixed = TRUE, info = o[1])
    expect_identical(y$data %||% y$table, t3$data %||% t3$table, info = o[1])
  }
})

test_that("the generated script reproduces an EQ-5D-Y-3L session", {
  skip_unless_app()
  e <- app_env()
  rv <- confirm_as(e, "Y3L")
  q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "DE",
                      col_name = "utility", add = 1)
  }))
  q(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "values")
    session$setInputs(output = "31")
    session$setInputs(utility_col = "utility")
    session$setInputs(run = 1)
  }))
  s <- shiny::isolate(list(steps = rv$steps, results = rv$results,
                           data = rv$processed_data))
  lines <- eq5dsuite:::script_from_session(s$steps, s$results)
  expect_true(any(grepl('"Y3L"', lines, fixed = TRUE)))
  f <- tempfile(fileext = ".R"); writeLines(lines, f)
  env <- new.env(parent = globalenv())
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  utils::capture.output(q(sys.source(f, envir = env)))
  expect_equal(env$analysis_data$utility, s$data$utility)
})

test_that("the Home page names the EQ-5D-Y-3L", {
  skip_unless_app()
  e <- app_env()
  html <- gsub("\\s+", " ", as.character(e$mod_home_ui("home")))
  expect_match(html, "EQ-5D-3L, EQ-5D-5L and EQ-5D-Y-3L", fixed = TRUE)
  expect_match(html, "EQ-5D-3L or EQ-5D-Y-3L (levels 1–3), or EQ-5D-5L (levels 1–5)",
               fixed = TRUE)
  # The mapping is still between the EQ-5D-3L and EQ-5D-5L only.
  expect_match(html, "maps between the EQ-5D-3L and EQ-5D-5L", fixed = TRUE)
})

test_that("'About the methods' describes only what the instrument is offered", {
  skip_unless_app()
  e <- app_env()
  about <- function(version) {
    rv <- confirm_as(e, version)
    html <- NULL
    q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
      html <<- gsub("\\s+", " ", as.character(output$controls$html))
    }))
    html
  }
  y <- about("Y3L")
  expect_match(y, "no crosswalk or mapping", fixed = TRUE)
  expect_match(y, "EQ-5D-Y-3L value set", fixed = TRUE)
  for (gone in c("van Hout", "NICE DSU", "XWR", "utility_DSU_GB"))
    expect_false(grepl(gone, y, fixed = TRUE), info = gone)
  for (v in c("3L", "5L")) {
    h <- about(v)
    for (kept in c("van Hout", "NICE DSU UK mapping", "utility_XWR_CA"))
      expect_match(h, kept, fixed = TRUE, info = paste(v, kept))
    expect_false(grepl("no crosswalk or mapping", h, fixed = TRUE), info = v)
  }
})
