# Server-side tests for the bundled Shiny app.
#
# These use shiny::testServer(), so they run under R CMD check without a
# browser. The app is not part of the package namespace: Shiny sources
# inst/shiny/global.R and inst/shiny/modules/*.R at startup, so the tests do
# the same into a throwaway environment.
#
# That environment is deliberately built so that eq5dsuite is *not* reachable
# on its search path (mask_exports() below). This is what would have caught
# the bug where the app called exported functions by bare name: run_app()
# loads the package but does not attach it, so every analysis failed unless
# the user happened to have run library(eq5dsuite) first.

# ---------------------------------------------------------------------------
# Navigation
# ---------------------------------------------------------------------------

test_that("the pages are in the order the work is done", {
  skip_unless_app()
  txt <- paste(readLines(file.path(app_dir(), "ui.R"), warn = FALSE),
               collapse = "\n")
  values <- regmatches(txt, gregexpr('nav_item\\([^)]*?"([a-z]+)",\\s*mod_',
                                     txt, perl = TRUE))[[1]]
  order <- sub('.*"([a-z]+)",\\s*mod_.*', "\\1", values)
  expect_identical(order,
                   c("home", "data", "validation", "values", "analysis",
                     "results"))
})

test_that("the UK mapping has no page of its own", {
  skip_unless_app()
  expect_false(file.exists(file.path(app_dir(), "modules", "mod_ukmap.R")))
  txt <- paste(readLines(file.path(app_dir(), "ui.R"), warn = FALSE),
               collapse = " ")
  expect_false(grepl("ukmap", txt, fixed = TRUE))
  txt <- paste(readLines(file.path(app_dir(), "server.R"), warn = FALSE),
               collapse = " ")
  expect_false(grepl("ukmap", txt, fixed = TRUE))
})

test_that("Validation sends the user on to the next page", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = eq5dsuite::example_data,
                              mapping = example_mapping(with_value = FALSE),
                              processed_data = NULL, results = list())
  shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    # The button is "Proceed", which is what the analysis guard quotes.
    expect_match(as.character(output$sidebar$html), ">Proceed<", fixed = TRUE)
    expect_false(grepl("Proceed to Analysis",
                       as.character(output$sidebar$html), fixed = TRUE))
    expect_silent(session$setInputs(proceed = 1))
    expect_false(is.null(rv$processed_data))
  })

  # The guard's wording matches the button.
  html <- as.character(e$analysis_guard(
    list(processed_data = NULL, mapping = example_mapping()), shiny::NS("a")))
  expect_match(html, "Proceed", fixed = TRUE)
  expect_false(grepl("Proceed to Analysis", html, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Home
# ---------------------------------------------------------------------------

test_that("the Home page's example button asks the Data module to load", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(load_example = 0L)
  shiny::testServer(e$mod_home_server, args = list(rv = rv), {
    session$setInputs(load_example = 1)
    expect_equal(rv$load_example, 1L)
  })
})

test_that("the Home page says what the app does and how to start", {
  skip_unless_app()
  e <- app_env()
  html <- as.character(e$mod_home_ui("home"))
  expect_match(html, "Upload your data", fixed = TRUE)
  expect_match(html, "Load the example dataset", fixed = TRUE)
  expect_match(html, "What your data needs", fixed = TRUE)
  expect_match(html, "Devlin", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("the example dataset loads, from the Data page and from Home", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L)

  shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$flushReact()
    expect_null(uploaded())

    # Home's button, which only bumps the counter.
    rv$load_example <- 1L
    session$flushReact()
    expect_equal(nrow(uploaded()), 10000L)

    # The Data page's own link.
    session$setInputs(use_example = 1)
    expect_equal(nrow(uploaded()), 10000L)
    expect_true(all(DIMS %in% names(uploaded())))
  })
})

test_that("confirming the mapping stores it, age and sex included", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L)

  shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    session$setInputs(version = "3L", col_mo = "mo", col_sc = "sc",
                      col_ua = "ua", col_pd = "pd", col_ad = "ad",
                      col_fu = "time", col_groupvar = "procedure",
                      col_id = "id", col_vas = "vas", col_age = "ageband",
                      col_sex = "gender", col_utility = "", confirm = 1)
    expect_equal(rv$mapping$names_eq5d, DIMS)
    expect_equal(rv$mapping$name_age, "ageband")
    expect_equal(rv$mapping$name_sex, "gender")
    expect_null(rv$mapping$name_utility)
    expect_equal(nrow(rv$raw_data), 10000L)
  })
})

test_that("the column suggestions find the columns in both datasets", {
  skip_unless_app()
  e <- app_env()

  s <- e$suggest_mapping(names(eq5dsuite::example_data))
  expect_equal(unlist(s[DIMS]),
               c(mo = "mo", sc = "sc", ua = "ua", pd = "pd", ad = "ad"))
  expect_equal(s$name_vas, "vas")
  expect_equal(s$name_age, "ageband")
  expect_equal(s$name_sex, "gender")

  # Names that do not match the canonical ones.
  s5 <- e$suggest_mapping(names(five_l_data()))
  expect_equal(unname(unlist(s5[DIMS])),
               c("mobility", "selfcare", "usual", "pain", "anxiety"))
  expect_equal(s5$name_fu, "visit")
  expect_equal(s5$name_id, "patient")
  expect_equal(s5$name_groupvar, "arm")
})

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

test_that("validation accepts example_data and Proceed sets processed_data", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = eq5dsuite::example_data,
                              mapping = example_mapping(),
                              processed_data = NULL, results = list())

  shiny::testServer(e$mod_validation_server, args = list(rv = rv), {
    v <- validation()
    types <- vapply(v$messages, function(m) m$type, character(1L))
    expect_false("error" %in% types)

    session$setInputs(proceed = 1)
    expect_equal(nrow(rv$processed_data), 10000L)
    # eq5d_apply_mapping() renames to the canonical names the analyses use.
    expect_true(all(c(DIMS, "fu", "vas", "groupvar", "id") %in%
                      names(rv$processed_data)))
  })
})

test_that("validation reports out-of-range levels", {
  skip_unless_app()
  e <- app_env()
  bad <- eq5dsuite::example_data
  bad$mo[1:5] <- 7L
  v <- eq5d_validate(bad, example_mapping(), quiet = TRUE)
  expect_true(any(v$type %in% c("warning", "error")))
})

# ---------------------------------------------------------------------------
# Analysis: every registered output, on both 3L and 5L data
# ---------------------------------------------------------------------------

# Run one analysis through the module and say what happened: "ok", "gated"
# (the data do not support it, and the UI says so), or the error message.
#
# The expression handed to testServer() is evaluated inside the module's own
# environment, so it cannot see this function's local variables. Everything it
# needs travels on `rv`, which the module is given as an argument.
run_one <- function(e, sp, rv, fu_levels) {
  rv[["t_spec"]] <- sp
  rv[["t_group_all"]] <- e$GROUP_FILTER_ALL
  rv[["t_fu"]]   <- fu_levels
  rv[["t_out"]]  <- NULL

  suppressWarnings(tryCatch(
    shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
      s <- shiny::isolate(rv$t_spec)
      session$setInputs(component = s$component)
      session$setInputs(output = s$id)
      ins <- list()
      if ("country" %in% s$opts)      ins$country <- "GB"
      if ("utility_col" %in% s$opts)  ins$utility_col <- e$value_columns(rv)[1L]
      if ("topn" %in% s$opts)         ins$topn <- 10L
      if ("two_fu" %in% s$opts)       ins$fu_levels <- shiny::isolate(rv$t_fu)
      if ("group_filter" %in% s$opts) ins$group_filter <- shiny::isolate(rv$t_group_all)
      if (length(ins)) do.call(session$setInputs, ins)

      if (length(missing()) > 0L) {
        rv[["t_out"]] <- "gated"
      } else {
        session$setInputs(run = 1)
        got <- if (identical(s$type, "plot")) res$plot else res$data
        rv[["t_out"]] <- if (is.null(got)) "no result produced" else "ok"
      }
    }),
    error = function(cond) rv[["t_out"]] <- conditionMessage(cond)))

  out <- shiny::isolate(rv[["t_out"]])
  if (is.null(out)) "nothing happened" else out
}


test_that("all 30 analyses run on example_data", {
  skip_unless_app()
  e <- app_env()

  outcomes <- vapply(e$ANALYSES, function(s) {
    run_one(e, s, processed_rv(e), c("Pre-op", "Post-op"))
  }, character(1L))
  names(outcomes) <- vapply(e$ANALYSES, `[[`, character(1L), "id")

  expect_identical(outcomes[outcomes != "ok"], setNames(character(0L), character(0L)))
  expect_equal(length(outcomes), 30L)
})

test_that("all 30 analyses run on uploaded 5L data", {
  skip_unless_app()
  e <- app_env()
  m  <- five_l_mapping()
  d  <- five_l_data()
  mk <- function() shiny::reactiveValues(
    raw_data = d, mapping = m, processed_data = eq5d_apply_mapping(d, m),
    results = list(), value_cols = "utility")

  outcomes <- vapply(e$ANALYSES, function(s) {
    run_one(e, s, mk(), c("Baseline", "Month 6"))
  }, character(1L))
  names(outcomes) <- vapply(e$ANALYSES, `[[`, character(1L), "id")

  # 1.2.3 and 1.2.4 used to be offered for EQ-5D-3L only. They work on 5L
  # data, so nothing here is gated by the instrument.
  expect_identical(outcomes[outcomes != "ok"],
                   setNames(character(0L), character(0L)))
  expect_equal(sum(outcomes == "ok"), 30L)
})

test_that("Shannon's indices are reported by timepoint, or for the sample", {
  skip_unless_app()
  e <- app_env()

  rv <- processed_rv(e)
  suppressWarnings(shiny::testServer(e$mod_analysis_server,
                                     args = list(rv = rv), {
    session$setInputs(component = "profile", output = "141")
    session$setInputs(run = 1)
  }))
  got <- shiny::isolate(rv$results[[1L]]$data)

  expect_identical(got$dimension,
                   c("mo", "sc", "ua", "pd", "ad", "Health state"))
  # One set of columns per timepoint, in the order the data present them.
  expect_named(got, c("dimension",
                      "H_Pre-op", "Hmax_Pre-op", "J_Pre-op",
                      "H_Post-op", "Hmax_Post-op", "J_Post-op"))
  # H'max is what the instrument allows: 3 levels per dimension, 243 states.
  expect_equal(got$`Hmax_Pre-op`, c(rep(log2(3), 5L), log2(243)))
  expect_true(all(got$`J_Pre-op` >= 0 & got$`J_Pre-op` <= 1))

  # With no timepoint mapped the function reports the sample as a whole, so
  # the app must not pass it a follow-up column it has not got.
  m <- example_mapping()
  m$name_fu <- NULL
  d <- valued_example_data()
  rv2 <- shiny::reactiveValues(
    raw_data = d, mapping = m, processed_data = eq5d_apply_mapping(d, m),
    results = list(), value_cols = "utility")
  suppressWarnings(shiny::testServer(e$mod_analysis_server,
                                     args = list(rv = rv2), {
    session$setInputs(component = "profile", output = "141")
    session$setInputs(run = 1)
  }))
  all_one <- shiny::isolate(rv2$results[[1L]]$data)
  expect_named(all_one, c("dimension", "H_All", "Hmax_All", "J_All"))
})

test_that("a saved result carries the code that produced it", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)

  suppressWarnings(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "profile", output = "111")
    session$setInputs(run = 1)
    expect_equal(length(rv$results), 1L)
    expect_equal(rv$results[[1L]]$result_type, "table")
    # The displayed code is unqualified: it is written for a user who has
    # attached the package, unlike the app itself.
    expect_match(rv$results[[1L]]$fn_call, "^eq5d_profile_level_summary\\(")
  }))
})

analysis_spec_for <- function(e, id) {
  for (s in e$ANALYSES) if (identical(s$id, id)) return(s)
  NULL
}

test_that("the value analyses need a value column, and say where to get one", {
  skip_unless_app()
  e <- app_env()

  # Before a value column exists, every EQ-5D value analysis is gated.
  rv <- processed_rv(e, with_value = FALSE)
  outcomes <- vapply(
    Filter(function(s) identical(s$component, "values"), e$ANALYSES),
    function(s) run_one(e, s, processed_rv(e, with_value = FALSE),
                        c("Pre-op", "Post-op")),
    character(1L))
  expect_true(all(outcomes == "gated"))

  # And the sidebar offers the page that produces one, rather than a dead end.
  shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "values", output = "fig34")
    expect_equal(missing(), "an EQ-5D value column")
    html <- as.character(output$run_ui$html)
    expect_match(html, "Calculate EQ-5D values", fixed = TRUE)
    expect_match(html, "goto_values", fixed = TRUE)
  })

  # With one, they run.
  expect_equal(run_one(e, analysis_spec_for(e, "fig34"), processed_rv(e),
                       c("Pre-op", "Post-op")), "ok")
})

test_that("an analysis whose columns are unmapped is gated, not broken", {
  skip_unless_app()
  e <- app_env()
  m <- example_mapping()
  m$name_groupvar <- NULL
  rv <- shiny::reactiveValues(
    raw_data = eq5dsuite::example_data, mapping = m,
    processed_data = eq5d_apply_mapping(eq5dsuite::example_data, m),
    results = list())

  shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "profile", output = "112")
    expect_equal(missing(), "a Group column")
    session$setInputs(run = 1)
    expect_null(res$data)          # nothing ran
    expect_equal(length(rv$results), 0L)
  })
})

# ---------------------------------------------------------------------------
# Calculate EQ-5D values
# ---------------------------------------------------------------------------

test_that("a value column can be calculated and is added to the data", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility", add = 1)
    expect_true("utility" %in% names(rv$processed_data))
    expect_equal(rv$mapping$name_utility, "utility")
    expect_equal(rv$mapping$country, "GB")
    v <- rv$processed_data$utility
    expect_gt(sum(!is.na(v)), 8000L)
    expect_true(all(v <= 1, na.rm = TRUE))
  }))
})

# ---------------------------------------------------------------------------
# Calculate EQ-5D values, including the NICE DSU UK mapping
# ---------------------------------------------------------------------------

test_that("every way of valuing is one choice in the Method selector", {
  skip_unless_app()
  e <- app_env()

  three <- e$get_utility_method_choices("3L")
  five  <- e$get_utility_method_choices("5L")
  expect_setequal(unname(three), c("direct", "xwr", "uk"))
  expect_setequal(unname(five),  c("direct", "xw", "uk"))
  # Each label states the direction, so the selector alone answers "which way
  # round is this?".
  expect_match(names(three)[three == "uk"], "3L\u21925L")
  expect_match(names(five)[five == "uk"],  "5L\u21923L")
  expect_match(names(three)[three == "xwr"], "3L\u21925L")
  expect_match(names(five)[five == "xw"],   "5L\u21923L")
})

test_that("the UK mapping direction follows the instrument version", {
  skip_unless_app()
  e <- app_env()
  expect_equal(e$uk_direction("3L")$fn, "eqxwr_UK")
  expect_equal(e$uk_direction("3L")$to, "EQ-5D-5L")
  expect_equal(e$uk_direction("5L")$fn, "eqxw_UK")
  expect_equal(e$uk_direction("5L")$to, "EQ-5D-3L")
})

test_that("banded ages take the midpoint of the band in completed years", {
  skip_unless_app()
  e <- app_env()

  a <- eq5d_age_band_midpoint(c("20 to 29", "30 to 39", "60 to 69", "80 to 89", NA))
  a <- list(value = as.numeric(a), is_banded = attr(a, "banded"),
            straddles = attr(a, "straddles"))
  expect_true(a$is_banded)
  # "30 to 39" covers the continuous interval [30, 40), midpoint 35 - not 34.5.
  expect_equal(a$value, c(25, 35, 65, 85, NA_real_))
  # The DSU's bands begin at 35, 45, 55 and 65, so those two straddle.
  expect_equal(a$straddles, c(FALSE, TRUE, TRUE, FALSE, FALSE))

  # A numeric column is exact ages, with nothing to infer.
  b <- eq5d_age_band_midpoint(c("34", "58", "72"))
  b <- list(value = as.numeric(b), is_banded = attr(b, "banded"),
            straddles = attr(b, "straddles"))
  expect_false(b$is_banded)
  expect_equal(b$value, c(34, 58, 72))
  expect_false(any(b$straddles))
})

test_that("a direct value set and a crosswalk both add a column", {
  skip_unless_app()
  e <- app_env()

  rv <- processed_rv(e, with_value = FALSE)
  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility", add = 1)
    expect_true("utility" %in% names(rv$processed_data))
    expect_equal(rv$mapping$name_utility, "utility")
    expect_match(as.character(output$direction$html), "Direct", fixed = TRUE)

    session$setInputs(method = "xwr", col_name = "xwr", add = 1)
    expect_true("xwr" %in% names(rv$processed_data))
    expect_match(as.character(output$direction$html), "Reverse crosswalk",
                 fixed = TRUE)
  }))
})

test_that("the UK mapping maps 3L data forwards to UK EQ-5D-5L values", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e, with_value = FALSE)

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "uk", age_col = "ageband", age_kind = "bands",
                      sex_col = "gender", male_value = "Male",
                      col_name = "eq5d_uk_5L", add = 1)

    expect_true("eq5d_uk_5L" %in% names(rv$processed_data))
    expect_equal(sum(!is.na(rv$processed_data$eq5d_uk_5L)), 8576L)

    # The page states which way the mapping runs.
    expect_match(as.character(output$direction$html),
                 "EQ-5D-3L \u2192 EQ-5D-5L", fixed = TRUE)
    # And that banded ages make it an approximation.
    expect_match(as.character(output$band_warning$html), "Ages are banded",
                 fixed = TRUE)

    cov <- coverage()
    # The reasons for not mapping are disjoint and account for every row.
    expect_equal(sum(cov$n[grepl("^Not mapped", cov$what)]),
                 cov$n[1L] - cov$n[2L])
    expect_true(any(grepl("straddling", cov$what)))
  }))
})

test_that("the UK mapping maps 5L data back, using exact ages", {
  skip_unless_app()
  e <- app_env()
  m <- five_l_mapping()
  d <- five_l_data()
  rv <- shiny::reactiveValues(raw_data = d, mapping = m,
                              processed_data = eq5d_apply_mapping(d, m),
                              results = list(), value_cols = "utility")

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "uk", age_col = "age", age_kind = "exact",
                      sex_col = "sex", male_value = "Male",
                      col_name = "eq5d_uk_3L", add = 1)

    expect_true("eq5d_uk_3L" %in% names(rv$processed_data))
    expect_equal(sum(!is.na(rv$processed_data$eq5d_uk_3L)), 400L)

    html <- as.character(output$direction$html)
    expect_match(html, "EQ-5D-5L \u2192 EQ-5D-3L", fixed = TRUE)
    # 5L data also get NICE's advice to value directly instead, dated.
    expect_match(html, "interim methods statement of 27 August 2026",
                 fixed = TRUE)
    expect_match(html, "topics started before that date", fixed = TRUE)

    # Exact ages, so no approximation warning.
    expect_null(output$band_warning$html)
  }))
})

test_that("the UK mapping asks for age and sex, and only then", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e, with_value = FALSE)

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    # A value set, not age and sex, for the methods that do not need them.
    session$setInputs(method = "direct")
    html <- as.character(output$method_opts$html)
    expect_match(html, "EQ-5D-3L value set", fixed = TRUE)
    expect_false(grepl("Age column", html, fixed = TRUE))

    session$setInputs(method = "uk")
    html <- as.character(output$method_opts$html)
    expect_match(html, "Age column", fixed = TRUE)
    expect_match(html, "Sex column", fixed = TRUE)
    expect_match(html, "Age recorded as", fixed = TRUE)
    expect_false(grepl("value set", html, fixed = TRUE))

    # example_data's ageband column holds bands, so Bands is the default.
    expect_match(html, 'value="bands"[^>]*checked')
  }))
})

test_that("an exact-age column does not default to Bands", {
  skip_unless_app()
  e <- app_env()
  m <- five_l_mapping()
  d <- five_l_data()
  rv <- shiny::reactiveValues(raw_data = d, mapping = m,
                              processed_data = eq5d_apply_mapping(d, m),
                              results = list(), value_cols = "utility")
  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "uk")
    expect_match(as.character(output$method_opts$html),
                 'value="exact"[^>]*checked')
  }))
})

test_that("the column name offered follows the method and value set", {
  skip_unless_app()
  e <- app_env()
  # utility_<METHOD>_<CODE>; see test-value-col-names.R for the behaviour.
  expect_equal(e$suggest_value_col("direct", "CA"), "utility_CA")
  expect_equal(e$suggest_value_col("xwr", "CA"), "utility_XWR_CA")
  expect_equal(e$suggest_value_col("xw", "DK"), "utility_XW_DK")
  expect_equal(e$suggest_value_col("uk"), "utility_DSU_GB")
  expect_equal(e$suggest_value_col("direct"), "utility")
})

# ---------------------------------------------------------------------------
# Which value sets a method offers
# ---------------------------------------------------------------------------

# Codes published for one instrument only. These are what separate a correct
# selector from one that happens to work: about half the value sets exist for
# both instruments, so offering the wrong instrument's list looks fine until a
# user picks one of these.
only_one_instrument <- function() {
  shown <- function(v) {
    out <- NULL
    utils::capture.output(out <- suppressMessages(suppressWarnings(
      eq5dsuite::eqvs_display(version = v, return_df = TRUE))))
    out$VS_code
  }
  v3 <- shown("3L"); v5 <- shown("5L")
  list(only_3L = setdiff(v3, v5), only_5L = setdiff(v5, v3),
       all_3L = v3, all_5L = v5)
}

test_that("a method's value sets are the target instrument's, not the data's", {
  skip_unless_app()
  e <- app_env()

  # A direct value set is the data's own instrument.
  expect_equal(e$value_set_version("direct", "3L"), "3L")
  expect_equal(e$value_set_version("direct", "5L"), "5L")
  # A crosswalk values one instrument's responses with the other's value set.
  expect_equal(e$value_set_version("xwr", "3L"), "5L")
  expect_equal(e$value_set_version("xw", "5L"), "3L")
  # An unrecognised or absent method falls back to the data's own.
  expect_equal(e$value_set_version(NULL, "3L"), "3L")
})

test_that("every value set the selector offers works with the method", {
  skip_unless_app()
  e <- app_env()
  codes <- only_one_instrument()
  skip_if(!length(codes$only_3L) || !length(codes$only_5L),
          "no instrument-specific value sets to tell the lists apart")

  # The regression: with 3L data the reverse crosswalk used to offer the 41
  # EQ-5D-3L value sets, 22 of which eq5d() refuses outright.
  for (case in list(
    list(data = "3L", method = "xwr",    want = "5L", eq5d_version = "XWR"),
    list(data = "5L", method = "xw",     want = "3L", eq5d_version = "XW"),
    list(data = "3L", method = "direct", want = "3L", eq5d_version = "3L"),
    list(data = "5L", method = "direct", want = "5L", eq5d_version = "5L"))) {

    offered <- e$get_country_choices(
      e$value_set_version(case$method, case$data))
    expect_setequal(as.character(offered),
                    codes[[paste0("all_", case$want)]])

    # Not just the right list -- a list eq5d() accepts, every code in it.
    refused <- vapply(as.character(offered), function(code)
      tryCatch(all(is.na(suppressMessages(suppressWarnings(
        eq5dsuite::eq5d(11111, country = code,
                        version = case$eq5d_version))))),
        error = function(e) TRUE), logical(1L))
    expect_identical(names(refused)[refused], character(0L),
                     info = paste(case$data, "data,", case$method))
  }
})

test_that("the selector lists the target instrument's sets and says so", {
  skip_unless_app()
  e <- app_env()
  codes <- only_one_instrument()
  skip_if(!length(codes$only_5L), "no 5L-only value sets")
  rv <- processed_rv(e, with_value = FALSE)     # EQ-5D-3L example data

  suppressWarnings(shiny::testServer(e$mod_values_server,
                                     args = list(rv = rv), {
    session$setInputs(method = "xwr")
    html <- as.character(output$method_opts$html)
    # The label names the instrument the values are on, so the user is not
    # left to work out which list they are looking at.
    expect_match(html, "EQ-5D-5L value set", fixed = TRUE)
    # Only 5L sets are in it.
    for (code in utils::head(codes$only_5L, 5L))
      expect_match(html, code, fixed = TRUE)
    for (code in utils::head(codes$only_3L, 5L))
      expect_false(grepl(paste0(">", code, "<"), html, fixed = TRUE))

    # And output$direction has said the same thing in a sentence since
    # before this list was right, which is how the bug went unnoticed.
    expect_match(as.character(output$direction$html),
                 "Your 3L responses are valued on a 5L value set",
                 fixed = TRUE)
  }))
})

test_that("changing the method drops a value set the new one cannot use", {
  skip_unless_app()
  e <- app_env()
  codes <- only_one_instrument()
  skip_if(!length(codes$only_3L), "no 3L-only value sets")
  rv <- processed_rv(e, with_value = FALSE)
  rv[["t_only_3L"]] <- codes$only_3L[1L]

  suppressWarnings(shiny::testServer(e$mod_values_server,
                                     args = list(rv = rv), {
    # A value set that exists for EQ-5D-3L only, chosen for a direct value.
    only_3L <- shiny::isolate(rv$t_only_3L)
    session$setInputs(method = "direct", country = only_3L)
    expect_match(as.character(output$method_opts$html), only_3L, fixed = TRUE)

    # Switching to the reverse crosswalk leaves it unusable, so it goes
    # rather than being carried into a call that would fail.
    session$setInputs(method = "xwr")
    html <- as.character(output$method_opts$html)
    expect_false(grepl(paste0(">", only_3L, "<"), html, fixed = TRUE))
    expect_false(grepl(paste0('value="', only_3L, '" selected'), html,
                       fixed = TRUE))

    # A set published for both survives the same switch.
    session$setInputs(method = "direct", country = "GB")
    session$setInputs(method = "xwr")
    # Every value set is in the HTML, so look for the selection itself.
    expect_match(as.character(output$method_opts$html), 'value="GB" selected',
                 fixed = TRUE)
  }))
})

test_that("adding a value column refuses a value set the method cannot use", {
  skip_unless_app()
  e <- app_env()
  codes <- only_one_instrument()
  skip_if(!length(codes$only_3L), "no 3L-only value sets")
  rv <- processed_rv(e, with_value = FALSE)
  rv[["t_only_3L"]] <- codes$only_3L[1L]

  suppressWarnings(shiny::testServer(e$mod_values_server,
                                     args = list(rv = rv), {
    # Setting the input directly, as a stale client could: the column is not
    # added, and nothing reaches eq5d() to fail there.
    session$setInputs(method = "xwr",
                      country = shiny::isolate(rv$t_only_3L),
                      col_name = "utility", add = 1)
    expect_null(rv$processed_data$utility)
    expect_false("utility" %in% (rv$value_cols %||% character(0L)))
  }))
})

test_that("adding a value column does not reset the pickers", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "value_uk", add = 1)
    # The mapping changed, so the controls re-render. The chosen method and
    # column name must survive, or the user has to set them again for every
    # column.
    html <- as.character(output$controls$html)
    expect_match(html, "value_uk", fixed = TRUE)
    expect_match(as.character(output$method_opts$html), 'value="GB" selected',
                 fixed = TRUE)
  }))
})

test_that("the preview leads with the rows the new column could value", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e, with_value = FALSE)

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility", add = 1)
    session$setInputs(method = "uk", age_col = "ageband", age_kind = "bands",
                      sex_col = "gender", male_value = "Male",
                      col_name = "eq5d_uk_5L", add = 1)
    # example_data's first rows have no age or sex, so ordering by the earlier
    # column would show the UK column as blanks and look broken.
    expect_equal(last_added(), "eq5d_uk_5L")
    d <- rv$processed_data
    expect_true(all(!is.na(d$eq5d_uk_5L[order(is.na(d$eq5d_uk_5L))][1:10])))
  }))
})

test_that("the mean-value calculator is gone", {
  skip_unless_app()
  e <- app_env()
  # It was the one part of the app that answered a question about a published
  # study rather than about the data loaded, and it has been removed.
  expect_false(exists("uk_mean_ui", envir = e, inherits = FALSE))
  files <- list.files(app_dir(), pattern = "\\.R$", recursive = TRUE,
                      full.names = TRUE)
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_false(grepl("mean_run|Map a mean value", txt), info = basename(f))
  }
})

# ---------------------------------------------------------------------------
# The guard on the pages that need validated data
# ---------------------------------------------------------------------------

test_that("the guard offers a way forward, not a dead end", {
  skip_unless_app()
  e <- app_env()
  ns <- shiny::NS("analysis")

  html <- as.character(e$analysis_guard(
    list(processed_data = NULL, mapping = NULL), ns))
  expect_match(html, "Go to Data", fixed = TRUE)
  expect_match(html, "analysis-goto_data", fixed = TRUE)

  html2 <- as.character(e$analysis_guard(
    list(processed_data = NULL, mapping = example_mapping()), ns))
  expect_match(html2, "Go to Validation", fixed = TRUE)
  expect_match(html2, "analysis-goto_validation", fixed = TRUE)

  expect_null(e$analysis_guard(
    list(processed_data = eq5dsuite::example_data,
         mapping = example_mapping()), ns))
})

test_that("the navbar carries the id the guard's buttons need", {
  skip_unless_app()
  txt <- readLines(file.path(app_dir(), "ui.R"), warn = FALSE)
  expect_true(any(grepl('id\\s*=\\s*"main_nav"', txt)))

  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list())
  shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    expect_silent(session$setInputs(goto_data = 1))
  })
})

# ---------------------------------------------------------------------------
# Dataset previews
# ---------------------------------------------------------------------------

test_that("a preview shows 10 rows, and offers 10, 20, 50 and 100", {
  skip_unless_app()
  e <- app_env()
  dt <- e$preview_table(head(eq5dsuite::example_data, 50L))
  expect_equal(dt$x$options$pageLength, 10L)
  expect_equal(dt$x$options$lengthMenu, c(10L, 20L, 50L, 100L))
  # The length menu needs "l" in dom or it is never drawn.
  expect_match(dt$x$options$dom, "l", fixed = TRUE)
})

test_that("value columns show three decimals without losing precision", {
  skip_unless_app()
  e <- app_env()
  d <- head(valued_example_data(), 20L)
  dt <- e$preview_table(d, value_cols = "value")

  # formatRound installs a column renderer that fires only for display, so
  # sorting, filtering and every download still see the full value.
  render <- unlist(lapply(dt$x$options$columnDefs, `[[`, "render"))
  expect_true(any(grepl("DTWidget.formatRound(data, 3", render, fixed = TRUE)))
  expect_true(any(grepl("type !== 'display' ? data", render, fixed = TRUE)))
  expect_identical(dt$x$data$value, d$value)
  expect_false(all(d$value[!is.na(d$value)] ==
                     round(d$value[!is.na(d$value)], 3)))

  # A column that is not a value column is left alone.
  expect_silent(e$preview_table(d))
})

test_that("the Data page labels the existing value column fully", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(raw_data = NULL, mapping = NULL,
                              processed_data = NULL, results = list(),
                              load_example = 0L, value_cols = character(0L))
  shiny::testServer(e$mod_data_server, args = list(rv = rv), {
    session$setInputs(use_example = 1)
    html <- as.character(output$mapping_ui$html)
    expect_match(html, "Existing EQ-5D value column", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# The working-dataset download lives with the value calculation
# ---------------------------------------------------------------------------

test_that("the Data page no longer offers the dataset download", {
  skip_unless_app()
  txt <- paste(readLines(file.path(app_dir(), "modules", "mod_data.R"),
                         warn = FALSE), collapse = "\n")
  expect_false(grepl("downloadButton", txt, fixed = TRUE))
  expect_false(grepl("downloadHandler", txt, fixed = TRUE))
})

test_that("the download carries the columns calculated in the session", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e, with_value = FALSE)

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "direct", country = "GB",
                      col_name = "utility", add = 1)
    session$setInputs(method = "uk", age_col = "ageband", age_kind = "bands",
                      sex_col = "gender", male_value = "Male",
                      col_name = "eq5d_uk_5L", add = 1)

    expect_match(as.character(output$download_ui$html),
                 "Download the working dataset", fixed = TRUE)

    tbl <- utils::read.csv(output$download_csv)
    expect_true(all(c("utility", "eq5d_uk_5L") %in% names(tbl)))
    expect_equal(nrow(tbl), 10000L)
    # Written at full precision, not the three decimals the preview shows.
    v <- tbl$eq5d_uk_5L[!is.na(tbl$eq5d_uk_5L)]
    expect_false(all(v == round(v, 3)))
    expect_equal(v, rv$processed_data$eq5d_uk_5L[!is.na(rv$processed_data$eq5d_uk_5L)],
                 tolerance = 1e-12)
  }))
})

# ---------------------------------------------------------------------------
# Which column the value analyses use
# ---------------------------------------------------------------------------

test_that("an existing column and calculated ones are all on offer", {
  skip_unless_app()
  e <- app_env()

  # Mapped on the Data page, so apply_mapping() renamed it to "utility".
  rv <- processed_rv(e)
  expect_equal(shiny::isolate(e$value_columns(rv)), "utility")

  suppressWarnings(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
    session$setInputs(method = "xwr", country = "GB",
                      col_name = "xwr_uk", add = 1)
    expect_equal(e$value_columns(rv), c("utility", "xwr_uk"))
  }))
})

test_that("the Utility column selector offers them and is passed through", {
  skip_unless_app()
  e <- app_env()
  d <- valued_example_data()
  d$second <- d$value / 2
  m <- example_mapping()
  rv <- shiny::reactiveValues(
    raw_data = d, mapping = m, processed_data = eq5d_apply_mapping(d, m),
    results = list(), value_cols = c("utility", "second"))

  suppressWarnings(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "values", output = "31")
    html <- as.character(output$opts$html)
    expect_match(html, "Utility column", fixed = TRUE)
    expect_match(html, 'value="utility"', fixed = TRUE)
    expect_match(html, 'value="second"', fixed = TRUE)

    # The chosen column is the one analysed, not a recalculation.
    session$setInputs(utility_col = "second", run = 1)
    expect_match(rv$results[[1L]]$fn_call, 'name_utility = "second"', fixed = TRUE)
    half <- res$data[["Pre-op"]][res$data$name == "Mean"]

    session$setInputs(utility_col = "utility", run = 1)
    whole <- res$data[["Pre-op"]][res$data$name == "Mean"]
    expect_equal(half, whole / 2, tolerance = 1e-10)
  }))
})

test_that("a value analysis will not run without a column chosen", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  suppressWarnings(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "values", output = "fig34")
    session$setInputs(utility_col = "not_a_column", run = 1)
    expect_null(res$plot)
    expect_length(rv$results, 0L)
  }))
})

# ---------------------------------------------------------------------------
# Reordering and removing saved results
# ---------------------------------------------------------------------------

test_that("results move up and down, and the ends are respected", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B", "C")))
  labs <- function() vapply(shiny::isolate(rv$results),
                            function(r) r$label, character(1L))

  shiny::isolate(e$move_result(rv, "r3", -1L))
  expect_equal(labs(), c("A", "C", "B"))
  shiny::isolate(e$move_result(rv, "r1", 1L))
  expect_equal(labs(), c("C", "A", "B"))

  # Past either end is a no-op, not an error or a dropped result.
  expect_false(shiny::isolate(e$move_result(rv, "r3", -1L)))
  expect_false(shiny::isolate(e$move_result(rv, "r2", 1L)))
  expect_equal(labs(), c("C", "A", "B"))

  # An unknown id changes nothing.
  expect_false(shiny::isolate(e$move_result(rv, "nope", 1L)))
  expect_equal(labs(), c("C", "A", "B"))
})

test_that("a removed result is gone, and the rest keep their order", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B", "C")))
  expect_true(shiny::isolate(e$remove_result(rv, "r2")))
  expect_equal(vapply(shiny::isolate(rv$results), function(r) r$label,
                      character(1L)), c("A", "C"))
  expect_false(shiny::isolate(e$remove_result(rv, "r2")))
})

test_that("the Results list is numbered, and a new result joins the end", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)

  suppressWarnings(shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    rv$results <- fake_results(c("First", "Second"))
    session$flushReact()
    html <- as.character(output$export_list$html)
    expect_match(html, "Results are exported in this order", fixed = TRUE)
    # Numbered, with controls on every row.
    expect_match(html, 'class="result-n">1<', fixed = TRUE)
    expect_match(html, 'class="result-n">2<', fixed = TRUE)
    expect_match(html, "up_r1", fixed = TRUE)
    expect_match(html, "down_r1", fixed = TRUE)
    expect_match(html, "rm_r1", fixed = TRUE)

    # Reordering, then adding, leaves the new one last.
    e$move_result(rv, "r2", -1L)
    rv$results <- c(rv$results, fake_results("Third")[1])
    expect_equal(vapply(rv$results, function(r) r$label, character(1L)),
                 c("Second", "First", "Third"))
  }))
})

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------

test_that("Export lists results in the Results order, and numbers them", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B", "C")))

  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    order_of <- function() {
      html <- as.character(output$export_list$html)
      pos <- vapply(c("A", "B", "C"),
                    function(l) regexpr(paste0(">", l, "<"), html, fixed = TRUE),
                    integer(1L))
      names(sort(pos[pos > 0]))
    }
    expect_equal(order_of(), c("A", "B", "C"))
    expect_match(as.character(output$export_list$html),
                 'class="result-n">1<', fixed = TRUE)

    # Reorder on the Results page; Export follows, because both read rv$results.
    e$move_result(rv, "r3", -1L)
    session$flushReact()
    expect_equal(order_of(), c("A", "C", "B"))

    # And a removed result leaves the list.
    e$remove_result(rv, "r1")
    session$flushReact()
    expect_equal(order_of(), c("C", "B"))
  })
})

test_that("the Word report is offered and follows the same order", {
  skip_unless_app()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  e <- app_env()
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B", "C")))

  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    e$move_result(rv, "r3", -1L)
    e$remove_result(rv, "r1")
    session$flushReact()

    path <- output$download_docx
    expect_match(basename(path), "^eq5dsuite_results_.*\\.docx$")

    dir <- tempfile("unz"); dir.create(dir)
    utils::unzip(path, exdir = dir)
    xml <- paste(readLines(file.path(dir, "word", "document.xml"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "")
    # pandoc may add attributes to <w:t>, so match the element loosely.
    pos <- vapply(c("A", "B", "C"),
                  function(l) regexpr(paste0("<w:t[^>]*>", l, "</w:t>"), xml),
                  integer(1L))
    expect_equal(names(sort(pos[pos > 0])), c("C", "B"))
    expect_equal(unname(pos[["A"]]), -1L)   # removed, so absent
  })
})

test_that("the archive leaves no working directory behind", {
  skip_unless_app()
  e <- app_env()
  rv <- shiny::reactiveValues(results = fake_results(c("A", "B")))
  before <- list.files(tempdir())

  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    zp <- output$download_all_zip
    expect_true(any(grepl("\\.csv$", utils::unzip(zp, list = TRUE)$Name)))
  })

  # The staging directory used to be named from the clock and never removed.
  left <- setdiff(list.files(tempdir()), before)
  expect_false(any(grepl("^eq5dzip", left)))
})

test_that("results export to CSV and to a zip", {
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)

  suppressWarnings(shiny::testServer(e$mod_analysis_server, args = list(rv = rv), {
    session$setInputs(component = "profile", output = "111")
    session$setInputs(run = 1)
  }))
  expect_equal(length(shiny::isolate(rv$results)), 1L)

  shiny::testServer(e$mod_export_server, args = list(rv = rv), {
    session$flushReact()
    # A download handler under testServer returns the path it wrote.
    csv <- output[[paste0("dl_csv_", rv$results[[1L]]$id)]]
    expect_match(basename(csv), "^01_Level_frequencies.*\\.csv$")
    expect_equal(nrow(utils::read.csv(csv)), nrow(rv$results[[1L]]$data))

    zp <- output$download_all_zip
    expect_match(basename(zp), "^eq5dsuite_results_.*\\.zip$")
    expect_true(any(grepl("\\.csv$", utils::unzip(zp, list = TRUE)$Name)))
  })
})

# ---------------------------------------------------------------------------
# Layout
# ---------------------------------------------------------------------------

# The lines of `txt` that give an element a fixed pixel height: a plot or a
# panel pinned to a size, so its content is clipped or scrolls inside a box
# instead of the page. `min-height` is a floor, not a fixed height - a figure
# may not shrink below it but grows freely - and the navbar logo is an image,
# which needs one.
#
# In CSS the rule's selector decides, so CSS is read rule by rule. The one
# exemption is a rule whose every selector is a scrollbar pseudo-element
# (::-webkit-scrollbar, -thumb, -track, ...): there `height` is the thickness
# of a horizontal scrollbar, not the size of anything on the page. A rule that
# also names an element is checked as usual.
fixed_height_lines <- function(txt, css = FALSE) {
  px <- '(?<!min-)height\\s*[=:]\\s*"?[0-9]+px'
  src <- paste(txt, collapse = "\n")
  if (css) {
    # Comments are not rules: blank them, keeping every line where it was.
    for (m in regmatches(src, gregexpr("/\\*[\\s\\S]*?\\*/", src, perl = TRUE))[[1L]])
      src <- sub(m, gsub("[^\n]", " ", m), src, fixed = TRUE)
    txt <- strsplit(src, "\n", fixed = TRUE)[[1L]]
  }
  hit <- grep(px, txt, perl = TRUE)
  hit <- hit[!grepl("logo|img|tags\\$img", txt[hit])]
  if (!css || !length(hit)) return(hit)
  line_of <- function(pos) {
    before <- substr(src, 1L, pos - 1L)
    1L + lengths(regmatches(before, gregexpr("\n", before, fixed = TRUE)))
  }
  # Innermost rules: a selector, then a block with no braces inside. A rule
  # nested in @media is found with its own selector.
  rules  <- gregexpr("([^{}]*)\\{([^{}]*)\\}", src, perl = TRUE)[[1L]]
  starts <- attr(rules, "capture.start")
  lens   <- attr(rules, "capture.length")
  exempt <- integer(0L)
  for (i in seq_along(rules)) {
    if (rules[i] < 0L) break
    sel <- trimws(strsplit(substr(src, starts[i, 1L],
                                  starts[i, 1L] + lens[i, 1L] - 1L), ",")[[1L]])
    sel <- sel[nzchar(sel)]
    if (!length(sel) ||
        !all(grepl("^::-webkit-scrollbar(-[a-z-]+)?(:[a-z-]+)*$", sel))) next
    body_start <- starts[i, 2L]
    body_end   <- body_start + max(lens[i, 2L] - 1L, 0L)
    exempt <- c(exempt, seq(line_of(body_start), line_of(body_end)))
  }
  setdiff(hit, exempt)
}

test_that("no plot or panel is given a fixed pixel height", {
  skip_unless_app()
  files <- list.files(app_dir(), pattern = "\\.(R|css)$", recursive = TRUE,
                      full.names = TRUE)
  offenders <- character(0L)
  for (f in files) {
    hit <- fixed_height_lines(readLines(f, warn = FALSE),
                              css = grepl("\\.css$", f))
    if (length(hit)) offenders <- c(offenders, sprintf("%s:%d", basename(f), hit))
  }
  expect_identical(offenders, character(0L))
})

test_that("the fixed-height check still catches panels, and exempts only scrollbars", {
  css <- c(
    "/* a comment: .x { height: 99px; } */",          # 1  comment: ignored
    ".plot-frame { height: 400px; }",                 # 2  caught
    ".card {",                                        # 3
    "  border: 0;",                                   # 4
    "  height: 300px;",                               # 5  caught, multi-line
    "}",                                              # 6
    "@media (max-width: 600px) {",                    # 7
    "  .sidebar { height: 250px; }",                  # 8  caught, inside @media
    "}",                                              # 9
    ".table-wrap { max-height: 500px; }",             # 10 caught, a cap pins too
    ".plot { min-height: 320px; }",                   # 11 floor: allowed
    "::-webkit-scrollbar { width: 7px; height: 7px; }", # 12 scrollbar: allowed
    "::-webkit-scrollbar-thumb:hover { height: 20px; }", # 13 scrollbar part: allowed
    ".panel, ::-webkit-scrollbar { height: 7px; }",   # 14 also names a panel: caught
    "::-webkit-scrollbar,",                           # 15
    ".modal-body { height: 600px; }"                  # 16 caught (selector list)
  )
  expect_identical(fixed_height_lines(css, css = TRUE), c(2L, 5L, 8L, 10L, 14L, 16L))
  # In R code the check is unchanged: a plot output pinned in pixels is caught.
  r <- c('shiny::plotOutput("p", height = "400px")',
         'shiny::tags$img(src = "logo.png", height = "24px")',
         'shiny::plotOutput("q", height = "100%")')
  expect_identical(fixed_height_lines(r), 1L)
})
