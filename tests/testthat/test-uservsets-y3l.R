# User-defined EQ-5D-Y-3L value sets.
#
# eqvs_add() built its health-state key with paste0("states_", version), which
# gives "states_Y3L". No code creates that key: the EQ-5D-Y-3L has the same 243
# health states as the EQ-5D-3L, and the package keeps a single copy of them
# under "states_3L" -- .fixPkgEnv() seeds uservsetsY3L from it and eq5d() looks
# Y3L states up in it. Every attempt to add an EQ-5D-Y-3L value set therefore
# failed the "should contain all health state indexes" check, and with nothing
# addable the whole Y3L user workflow was unreachable.

y3l_vs <- function(code = "MYY") {
  df <- data.frame(state = make_all_EQ_indexes("3L"),
                   value = round(seq(1, -0.5, length.out = 243L), 4))
  names(df)[2] <- code
  df
}

# ---------------------------------------------------------------------------
# The key itself
# ---------------------------------------------------------------------------

test_that("Y3L shares the EQ-5D-3L health states", {
  pkgenv <- local_eq_env()

  # The premise of the fix: there is no states_Y3L, by design.
  expect_false("states_Y3L" %in% names(pkgenv))
  expect_identical(.states_key("Y3L"), "states_3L")
  expect_identical(.states_key("3L"),  "states_3L")
  expect_identical(.states_key("5L"),  "states_5L")

  expect_identical(pkgenv$uservsetsY3L$state, pkgenv$states_3L$state)
  expect_identical(nrow(pkgenv$vsetsY3L_combined), 243L)
})

test_that(".eq5d_instrument() spells the instrument, not the version code", {
  expect_identical(.eq5d_instrument("3L"),  "EQ-5D-3L")
  expect_identical(.eq5d_instrument("5L"),  "EQ-5D-5L")
  expect_identical(.eq5d_instrument("Y3L"), "EQ-5D-Y-3L")
  expect_error(.eq5d_instrument("4L"), "Unknown version")
  # The value set repository folders are named after the instrument.
  for (v in .cache_versions) expect_identical(.vs_folder(v), .eq5d_instrument(v))
})

# ---------------------------------------------------------------------------
# The workflow, end to end
# ---------------------------------------------------------------------------

test_that("a user-defined Y3L value set can be added and displayed", {
  local_eq_env()

  expect_message(
    eqvs_add(y3l_vs(), version = "Y3L", country = "Fantasia",
             countryCode = "FA", VSCode = "MYY", saveOption = 1),
    "was added")

  df <- eqvs_display(version = "Y3L", return_df = TRUE)
  expect_true("MYY" %in% df$VS_code)
  expect_identical(nrow(df), 11L)          # 10 built-in, plus this one
  expect_identical(df$Name_short[df$VS_code == "MYY"], "Fantasia")

  # It did not leak into the other instruments.
  expect_false("MYY" %in% eqvs_display(version = "3L", return_df = TRUE)$VS_code)
  expect_false("MYY" %in% eqvs_display(version = "5L", return_df = TRUE)$VS_code)
})

test_that("values can be calculated from a user-defined Y3L value set", {
  local_eq_env()
  vs <- y3l_vs()
  suppressMessages(
    eqvs_add(vs, version = "Y3L", country = "Fantasia", countryCode = "FA",
             VSCode = "MYY", saveOption = 1))

  states   <- c(11111L, 12321L, 33333L)
  expected <- vs$MYY[match(states, vs$state)]

  expect_equal(unname(eq5dy3l(states, country = "MYY")), expected)
  expect_identical(eq5d(states, country = "MYY", version = "Y3L"),
                   eq5dy3l(states, country = "MYY"))
  # Lower-case works too, as for every other version argument.
  expect_identical(eq5d(states, country = "MYY", version = "y3l"),
                   eq5dy3l(states, country = "MYY"))

  # Every one of the 243 states resolves.
  all_vals <- eq5dy3l(make_all_EQ_indexes("3L"), country = "MYY")
  expect_false(anyNA(all_vals))
  expect_equal(unname(all_vals), vs$MYY)

  # Five-level states are not valid for a three-level instrument.
  expect_true(is.na(unname(eq5dy3l(55555, country = "MYY"))))
})

test_that("a user-defined Y3L value set survives a save and reload", {
  cache_dir <- withr::local_tempdir()
  local_eq_env(cache_dir)
  vs <- y3l_vs()

  # saveOption = 2 writes to the cache directory.
  suppressMessages(
    eqvs_add(vs, version = "Y3L", country = "Fantasia", countryCode = "FA",
             VSCode = "MYY", saveOption = 2))
  expect_true(file.exists(file.path(cache_dir, .cache_basename)))

  # A fresh environment reads it back. (.fixPkgEnv() rebuilds the derived
  # objects but does not read the cache; .onLoad() and eqvs_load() do that.)
  local_eq_env(cache_dir)
  expect_message(eqvs_load(cache_dir), "loaded")
  expect_true("MYY" %in% eqvs_display(version = "Y3L",
                                      return_df = TRUE)$VS_code)
  expect_equal(unname(eq5dy3l(11111, country = "MYY")), 1)

  # saveOption = 3 writes to an explicit path, read back with eqvs_load().
  save_dir <- withr::local_tempdir()
  local_eq_env()
  suppressMessages(
    eqvs_add(vs, version = "Y3L", country = "Fantasia", countryCode = "FA",
             VSCode = "MYY", saveOption = 3, savePath = save_dir))
  expect_true(file.exists(file.path(save_dir, .cache_basename)))

  local_eq_env()
  expect_false("MYY" %in% eqvs_display(version = "Y3L",
                                       return_df = TRUE)$VS_code)
  expect_message(eqvs_load(save_dir), "loaded")
  expect_true("MYY" %in% eqvs_display(version = "Y3L",
                                      return_df = TRUE)$VS_code)
  expect_equal(unname(eq5dy3l(33333, country = "MYY")), -0.5)
})

test_that("a user-defined Y3L value set can be dropped", {
  local_eq_env()
  suppressMessages(
    eqvs_add(y3l_vs(), version = "Y3L", country = "Fantasia",
             countryCode = "FA", VSCode = "MYY", saveOption = 1))

  expect_message(
    eqvs_drop(country = "MYY", version = "Y3L", saveOption = 1, ask = FALSE),
    "deleted")

  expect_false("MYY" %in% eqvs_display(version = "Y3L",
                                       return_df = TRUE)$VS_code)
  expect_identical(nrow(eqvs_display(version = "Y3L", return_df = TRUE)), 10L)
  # Gone for good: eq5dy3l() warns that the code was not found, then stops.
  # (eqvs_display() prints the remaining value sets on the way out.)
  expect_warning(
    suppressMessages(utils::capture.output(
      expect_error(eq5dy3l(11111, country = "MYY"),
                   "No valid countries listed"))),
    "not found")
})

test_that("the Y3L health-state check still rejects a malformed value set", {
  local_eq_env()

  # 3,125 rows: the five-level state space, not the three-level one.
  wrong <- data.frame(state = make_all_EQ_indexes("5L"), MYY = 0)
  expect_error(eqvs_add(wrong, version = "Y3L", VSCode = "MYY"),
               "should have exactly 243 rows", fixed = TRUE)

  # Right number of rows, wrong states.
  wrong2 <- data.frame(state = seq_len(243L), MYY = 0)
  expect_error(eqvs_add(wrong2, version = "Y3L", VSCode = "MYY"),
               "EQ-5D-Y-3L health state indexes", fixed = TRUE)
})
