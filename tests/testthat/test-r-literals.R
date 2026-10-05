# Strings written into R code -- the generated script, and the data path the
# tests put into it -- must parse back to themselves on every platform.
#
# On Windows the tests put a temporary-file path such as
# C:\Users\RUNNER~1\AppData\Local\Temp\... into generated scripts through
# sub(), whose replacement treats a backslash as an escape: the deparsed
# "C:\\Users" became "C:\Users" and the script failed with "'\U' used without
# hex digits". .r_string() is now the one way a string is written into R
# code, and it is ASCII-only, so the result does not depend on the
# platform's encoding either.
#
# These run on any platform. The Windows paths below are written out, so on
# Linux they simulate the Windows case; they do not need Windows.

strings <- c(
  windows_path  = "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp\\RtmpAb\\file1.csv",
  escapes       = "C:\\new\\table\\x41\\u00e9\\U0001",
  unc_path      = "\\\\server\\share\\data set.csv",
  spaces        = "  a  file  name .csv ",
  double_quote  = "say \"hi\"",
  single_quote  = "It's a knee",
  backtick      = "`x` y",
  controls      = "tab\there\nnew line\rcr\001bell",
  latin         = "Hôpital – Côte d'Ivoire, Müller, Ñandú",
  cjk           = "健康状态 데이터",
  emoji         = "score \U0001F600 ok",
  empty         = "",
  ascii         = "plain")

round_trip <- function(code) eval(parse(text = code, keep.source = FALSE)[[1L]])
is_ascii <- function(x) all(utf8ToInt(x) < 128L)

test_that(".r_string() gives back every string, as ASCII", {
  for (nm in names(strings)) {
    lit <- .r_string(strings[[nm]])
    expect_true(is_ascii(lit), info = nm)
    expect_identical(round_trip(lit), enc2utf8(strings[[nm]]), info = nm)
  }
  expect_identical(.r_string(NA_character_), "NA_character_")
  expect_identical(round_trip(.r_string(NA_character_)), NA_character_)
  expect_identical(round_trip(.r_string(factor("Hôpital"))), "Hôpital")
})

test_that("strings in a native encoding are written correctly too", {
  latin1 <- iconv("Hôpital", "UTF-8", "latin1")
  Encoding(latin1) <- "latin1"
  expect_identical(round_trip(.r_string(latin1)), "Hôpital")
})

test_that(".deparse_arg() writes vectors and named values the same way", {
  v <- unname(strings)
  code <- .deparse_arg(v)
  expect_true(is_ascii(code))
  expect_identical(round_trip(code), enc2utf8(v))
  expect_identical(round_trip(.deparse_arg(character(0))), character(0))
  named <- c("Hôpital" = 1, "C:\\x" = 2)
  code <- .deparse_arg(named)
  expect_true(is_ascii(code))
  expect_identical(round_trip(code), named)
  expect_identical(round_trip(.deparse_arg(list(a = "Müller", b = 1:2))),
                   list(a = "Müller", b = 1:2))
})

test_that(".deparse_call() keeps argument names and values intact", {
  code <- .deparse_call("c", list("70 to 79" = 75, "Hôpital" = 1,
                                 x = "C:\\Users\\a.csv"), width = 0L)
  expect_true(is_ascii(code))
  expect_identical(round_trip(code),
                   c("70 to 79" = 75, "Hôpital" = 1, x = "C:\\Users\\a.csv"))
})

test_that("the tests' data path survives a Windows path (simulated on Linux)", {
  skip_unless_app()
  lines <- c("x <- 1", 'data_path <- "REPLACE BY ACTUAL PATH"', "y <- 2")
  for (p in strings[c("windows_path", "unc_path", "spaces", "latin", "cjk")]) {
    out <- fill_data_path(lines, p)
    expect_identical(out[c(1, 3)], lines[c(1, 3)])
    env <- new.env()
    eval(parse(text = out[2]), envir = env)
    expect_identical(env$data_path, enc2utf8(p))
  }
})

test_that("a generated script is ASCII, and a non-ASCII filter still selects its rows", {
  label <- "Hôpital – 健康 \"A\\B\""
  code <- .filter_code(list(column = "groupvar", value = label))
  expect_true(is_ascii(code))
  df <- data.frame(groupvar = c(label, "other", NA, label), x = 1:4,
                   stringsAsFactors = FALSE)
  got <- .apply_filter(df, list(column = "groupvar", value = label))
  expect_identical(got$x, c(1L, 4L))

  # A whole script: an uploaded file with a non-ASCII name, a filtered result.
  steps <- list(
    list(kind = "load", source = "file", file = "données – 2024.csv",
         ext = "csv", read_args = list(sep = ";", dec = ",")),
    list(kind = "map", mapping = list(
      eq5d_version = "3L", names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
      name_vas = "vas", name_groupvar = "grupo")))
  results <- list(list(label = "VAS", call = list(
    fn = "eq5d_vas_summary", args = list(name_vas = "vas"), type = "table",
    filter = list(column = "groupvar", value = label))))
  lines <- script_from_session(steps, results)
  expect_true(all(vapply(lines, is_ascii, NA)))
  expect_silent(parse(text = paste(lines, collapse = "\n")))
  # The literal in the script is the label itself.
  sub_line <- grep("which(as.character(", lines, fixed = TRUE, value = TRUE)
  lit <- regmatches(sub_line, regexpr('== "[^)]*"\\)', sub_line))
  expect_identical(round_trip(sub("^== (.*)\\)$", "\\1", lit)), label)
})
