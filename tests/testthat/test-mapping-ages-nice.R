# Column mapping, age bands, and the NICE mapping's input handling.
#
# Review findings F04, F05, F14 and F15. Each was a case of deciding something
# about the input -- which column is which, what an age label means, whether a
# vector holds states or values -- from the wrong information, or from all of
# the input when it should have been element by element.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")
std <- function(...) data.frame(mo = 1, sc = 2, ua = 3, pd = 1, ad = 1, ...)

# ---------------------------------------------------------------------------
# F04: eq5d_apply_mapping()
# ---------------------------------------------------------------------------

test_that("a swap between two dimension names keeps both columns", {
  # Renaming one at a time against already-renamed names gave
  # sc, sc, ua, pd, ad: the mobility column was gone and sc appeared twice.
  got <- q(eq5d_apply_mapping(std(), list(names_eq5d = c("sc", "mo", "ua", "pd", "ad"))))
  expect_setequal(names(got), DIMS)
  expect_false(anyDuplicated(names(got)) > 0L)
  # The column the user called sc is now mo, so mo holds 2 and sc holds 1.
  expect_equal(got$mo, 2L)
  expect_equal(got$sc, 1L)
})

test_that("a three-way cycle of dimension names is applied as one", {
  got <- q(eq5d_apply_mapping(std(), list(names_eq5d = c("sc", "ua", "mo", "pd", "ad"))))
  expect_setequal(names(got), DIMS)
  expect_equal(c(got$mo, got$sc, got$ua), c(2L, 3L, 1L))
})

test_that("optional columns can swap with each other", {
  d <- data.frame(mo = 1, sc = 1, ua = 1, pd = 1, ad = 1,
                  id = "a", fu = "Pre-op")
  got <- q(eq5d_apply_mapping(d, list(names_eq5d = DIMS,
                                      name_fu = "id", name_id = "fu")))
  expect_equal(got$fu, "a")
  expect_equal(got$id, "Pre-op")
})

test_that("a column mapped twice is refused", {
  expect_error(q(eq5d_apply_mapping(std(), list(names_eq5d = c("mo", "mo", "ua", "pd", "ad")))),
               "mapped to more than one role")
  d <- std(t = "Pre-op")
  expect_error(q(eq5d_apply_mapping(d, list(names_eq5d = DIMS, name_fu = "mo"))),
               "mapped to more than one role")
})

test_that("a target name already in use is refused rather than duplicated", {
  d <- data.frame(a = 1, b = 2, c = 3, e = 1, f = 1,
                  fu = "leftover", t = "Pre-op")
  expect_error(q(eq5d_apply_mapping(d, list(names_eq5d = c("a","b","c","e","f"),
                                            name_fu = "t"))),
               "two columns the name")
})

test_that("mapping a column to the name it already has is a no-op", {
  d <- std(fu = "Pre-op")
  got <- q(eq5d_apply_mapping(d, list(names_eq5d = DIMS, name_fu = "fu")))
  expect_equal(names(got), c(DIMS, "fu"))
})

test_that("factor dimensions and EQ VAS are read by label", {
  f <- as.data.frame(lapply(stats::setNames(rep("3", 5), DIMS), factor))
  f$v <- factor("70")
  got <- q(eq5d_apply_mapping(f, list(names_eq5d = DIMS, name_vas = "v")))
  expect_equal(unname(unlist(got[1L, DIMS])), rep(3L, 5L))
  expect_equal(got$vas, 70)
  # Character and numeric representations agree with the factor one.
  chr <- as.data.frame(lapply(stats::setNames(rep("3", 5), DIMS), as.character))
  expect_equal(q(eq5d_apply_mapping(chr, list(names_eq5d = DIMS)))[, DIMS],
               got[, DIMS])
})

test_that("the app and the generated script map identically", {
  skip_unless_app()
  # Execution-based, not text-based: the script's own renaming is run on the
  # same frame and the results compared.
  d <- std(t = "Pre-op", pid = 1, g = "A", v = 50)
  m <- list(eq5d_version = "3L", names_eq5d = c("sc", "mo", "ua", "pd", "ad"),
            name_fu = "t", name_id = "pid", name_groupvar = "g", name_vas = "v")
  from_app <- q(eq5d_apply_mapping(d, m))

  lines <- eq5dsuite:::script_from_session(
    list(list(kind = "load", source = "example"),
         list(kind = "map", mapping = m)), list())
  env <- new.env(parent = globalenv())
  env$raw_data <- d
  # The preamble defines the parsers; the variables are renamed in section 1
  # and the dimensions checked and converted in section 2.
  pre <- lines[seq_len(grep("^# 1\\. Load data", lines)[1L] - 1L)]
  q(eval(parse(text = paste(pre, collapse = "\n")), envir = env))
  block <- lines[seq(grep("^analysis_data <- raw_data$", lines)[1L], length(lines))]
  block <- block[seq_len(grep("^# 3\\.", block)[1L] - 1L)]
  q(eval(parse(text = paste(block, collapse = "\n")), envir = env))
  expect_equal(env$analysis_data[, names(from_app)], from_app)
})

# ---------------------------------------------------------------------------
# F05: one unusable record no longer voids the vector
# ---------------------------------------------------------------------------

test_that("valid states keep their values when an invalid one is present", {
  for (fn in list(eqxwr_UK, eqxw_UK)) {
    one <- q(fn(11111, age = 30, male = 1))
    expect_false(is.na(one))
    # Appended, interspersed and leading.
    expect_equal(q(fn(c(11111, 99999), age = 30, male = 1))[1L], one)
    expect_equal(q(fn(c(99999, 11111), age = 30, male = 1))[2L], one)
    expect_equal(q(fn(c(99999, 11111, 99999), age = 30, male = 1))[2L], one)
    expect_true(is.na(q(fn(c(11111, 99999), age = 30, male = 1))[2L]))
  }
})

test_that("an unusable state record is reported as such", {
  expect_warning(eqxwr_UK(c(11111, 99999), age = 30, male = 1),
                 "not an EQ-5D-3L health state")
})

test_that("aggregate score input still works, and all-NA input is unchanged", {
  expect_false(is.na(q(eqxwr_UK(0.5, age = 30, male = 1, bwidth = 0.1))))
  expect_true(is.na(q(eqxwr_UK(NA, age = 30, male = 1))))
  expect_length(q(eqxwr_UK(c(NA, NA), age = 30, male = 1)), 2L)
})

# ---------------------------------------------------------------------------
# F14: dim.names is honoured by both mappings
# ---------------------------------------------------------------------------

test_that("custom, canonical and unnamed dimension columns agree", {
  want <- q(eqxwr_UK(data.frame(mo = 1, sc = 2, ua = 3, pd = 2, ad = 1),
                     age = 30, male = 1))
  custom <- data.frame(A = 1, B = 2, C = 3, D = 2, E = 1)
  expect_equal(q(eqxwr_UK(custom, age = 30, male = 1,
                          dim.names = c("A", "B", "C", "D", "E"))), want)
  # Upper case, and the matrix form with no names at all.
  upper <- data.frame(MO = 1, SC = 2, UA = 3, PD = 2, AD = 1)
  expect_equal(q(eqxwr_UK(upper, age = 30, male = 1)), want)
  expect_equal(q(eqxwr_UK(as.matrix(data.frame(1, 2, 3, 2, 1)),
                          age = 30, male = 1)), want)
  # Columns given out of order, resolved by name.
  shuffled <- data.frame(C = 3, A = 1, E = 1, B = 2, D = 2)
  expect_equal(q(eqxwr_UK(shuffled, age = 30, male = 1,
                          dim.names = c("A", "B", "C", "D", "E"))), want)
})

test_that("the other direction honours dim.names too", {
  want <- q(eqxw_UK(data.frame(mo = 1, sc = 2, ua = 3, pd = 2, ad = 1),
                    age = 30, male = 1))
  expect_equal(q(eqxw_UK(data.frame(A = 1, B = 2, C = 3, D = 2, E = 1),
                         age = 30, male = 1,
                         dim.names = c("A", "B", "C", "D", "E"))), want)
})

test_that("age and sex columns beside the dimensions are still found", {
  d <- data.frame(A = 1, B = 2, C = 3, D = 2, E = 1, age = 30, male = 1)
  expect_equal(q(eqxwr_UK(d, age = "age", male = "male",
                          dim.names = c("A", "B", "C", "D", "E"))),
               q(eqxwr_UK(data.frame(mo = 1, sc = 2, ua = 3, pd = 2, ad = 1),
                          age = 30, male = 1)))
})

# ---------------------------------------------------------------------------
# F15: age bands
# ---------------------------------------------------------------------------

test_that("a closed band gives its midpoint in completed years", {
  expect_equal(as.numeric(q(eq5d_age_band_midpoint(c("20 to 29", "30-39", "60 to 69")))),
               c(25, 35, 65))
})

test_that("a band open at the bottom is not given an invented midpoint", {
  # "under 20" covers ages the mapping excludes as well as band 1, so it
  # cannot identify a category. It used to return 25, an age above the band.
  for (lab in c("under 20", "<20", "below 20", "less than 20", "20 and under")) {
    expect_true(is.na(as.numeric(q(eq5d_age_band_midpoint(lab)))), info = lab)
  }
  expect_warning(eq5d_age_band_midpoint("under 20"),
                 "do not identify a single NICE age band")
})

test_that("a band open at the top resolves only when it is one category", {
  # Every age in "65+" is in the last band, so the lower bound identifies it.
  expect_equal(as.numeric(q(eq5d_age_band_midpoint("65+"))), 65)
  expect_equal(as.numeric(q(eq5d_age_band_midpoint("65 and over"))), 65)
  # "50+" spans the 45-54, 55-64 and 65+ bands, so it does not.
  expect_true(is.na(as.numeric(q(eq5d_age_band_midpoint("50+")))))
  # And the value mapped is the same for 65 as for any other age in the band.
  expect_equal(q(eqxwr_UK(11111, age = 65, male = 1)),
               q(eqxwr_UK(11111, age = 80, male = 1)))
})

test_that("a bare number among bands is the age itself", {
  # sub() returns its input unchanged when the pattern does not match, so the
  # upper bound parsed as the number and "70" became 70.5.
  got <- as.numeric(q(eq5d_age_band_midpoint(c("30 to 39", "70"))))
  expect_equal(got, c(35, 70))
})

test_that("factor, character and numeric ages agree at band boundaries", {
  for (a in c(16, 34, 35, 44, 45, 54, 55, 64, 65, 99)) {
    want <- q(eqxwr_UK(11111, age = a, male = 1))
    expect_equal(q(eqxwr_UK(11111, age = as.character(a), male = 1)), want, info = a)
    expect_equal(q(eqxwr_UK(11111, age = factor(as.character(a)), male = 1)),
                 want, info = a)
  }
})

test_that("an unparseable label is NA rather than a guess", {
  expect_true(is.na(as.numeric(q(eq5d_age_band_midpoint("not an age")))))
  expect_warning(eq5d_age_band_midpoint("not an age"), "do not identify")
})

test_that("F05: the state-or-score decision is defined for every input", {
  # any(looks_state) settles the mode. An EQ-5D value cannot look like a
  # five-digit state, so a vector holding any state is a vector of states and
  # everything else in it is an invalid state.
  for (nm in c("eqxwr_UK", "eqxw_UK")) {
    fn <- get(nm)
    inf <- paste(nm, ":")
    # All invalid: states, all NA.
    got <- q(fn(c(99999, 88888), age = 30, male = 1))
    expect_length(got, 2L); expect_true(all(is.na(got)), info = inf)
    # All NA: unchanged from before, NA out, no warning about records.
    got <- q(fn(c(NA, NA), age = 30, male = 1))
    expect_length(got, 2L); expect_true(all(is.na(got)), info = inf)
    expect_silent(fn(c(NA_real_, NA_real_), age = 30, male = 1))
    # Empty in, empty out.
    expect_length(q(fn(numeric(0), age = 30, male = 1)), 0L)
    # Mixed state and score: the states are honoured, the score is reported
    # as an unusable state rather than silently reinterpreting the vector.
    got <- q(fn(c(11111, 0.5), age = 30, male = 1))
    expect_equal(got[1L], q(fn(11111, age = 30, male = 1)), info = inf)
    expect_true(is.na(got[2L]), info = inf)
    expect_warning(fn(c(11111, 0.5), age = 30, male = 1), "health state")
    # Character states are states.
    expect_equal(q(fn(c("11111", "12321"), age = 30, male = 1)),
                 q(fn(c(11111, 12321), age = 30, male = 1)), info = inf)
    # And a vector with no state in it is still read as scores.
    sc <- q(fn(c(0.5, 0.6), age = 30, male = 1, bwidth = 0.1))
    expect_length(sc, 2L); expect_false(any(is.na(sc)), info = inf)
  }
})

test_that("F05: a state out of range for the direction is an invalid state", {
  # 44444 is a five-level state, so it is not an EQ-5D-3L state and
  # eqxwr_UK() must refuse it -- while eqxw_UK(), which takes 5L input,
  # accepts it.
  expect_true(is.na(q(eqxwr_UK(44444, age = 30, male = 1))))
  expect_false(is.na(q(eqxw_UK(44444, age = 30, male = 1))))
  # And one of each in the same vector leaves the valid one alone.
  got <- q(eqxwr_UK(c(11111, 44444), age = 30, male = 1))
  expect_equal(got[1L], q(eqxwr_UK(11111, age = 30, male = 1)))
  expect_true(is.na(got[2L]))
})
