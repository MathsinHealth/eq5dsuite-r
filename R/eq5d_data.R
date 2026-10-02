# Reading, mapping, validating and formatting EQ-5D data.
#
# These support the Shiny app, and nothing else in the package calls them.
# They are internal: a user analysing EQ-5D data from the console has no use
# for them, and they would only crowd the analysis functions in `eq5dsuite::`.
# The app reaches them through eq5dsuite:::, which it may because it runs
# inside this package.
#
# The script the app generates cannot, so it writes the same work out in full.
# What keeps the two from drifting is tests/testthat/test-script-generation.R,
# which runs the generated script and compares every value column, every
# result and every formatted table against what the app produced, and checks
# that the script's inlined checks stop and warn where eq5d_validate() does.

# ── eq5d_read_data ────────────────────────────────────────────────────────────

#' Read a data file of EQ-5D responses
#'
#' Reads a CSV, Excel or RDS file, choosing by the file's extension unless
#' \code{type} says otherwise. This is what the Shiny app's upload uses, and
#' what the R script it generates calls, so a script reproduces what the app
#' read.
#'
#' @param path Path to the file.
#' @param type File type: \code{"csv"}, \code{"xlsx"}, \code{"xls"} or
#'   \code{"rds"}. \code{NULL} (default) takes it from the extension of
#'   \code{path}.
#' @param sep Field separator for a CSV. Use \code{";"} for the
#'   semicolon-separated files common where the comma is the decimal mark.
#' @param dec Decimal mark for a CSV.
#' @param fileEncoding Encoding of a CSV, passed to \code{\link{read.csv}}.
#'   \code{""} uses the session's default; \code{"UTF-8"} and
#'   \code{"latin1"} are the usual alternatives.
#' @param sheet Sheet of an Excel file: a number or a name.
#' @param ... Further arguments passed to \code{\link[utils]{read.csv}} or
#'   \code{readxl::read_excel}.
#' @return A data frame. Character columns are not converted to factors and
#'   column names are left as they are in the file.
#' @seealso \code{\link{eq5d_apply_mapping}} for naming the columns an analysis
#'   needs, and \code{\link{eq5d_validate}} for checking them.
#' @keywords internal
eq5d_read_data <- function(path, type = NULL, sep = ",", dec = ".",
                           fileEncoding = "", sheet = 1, ...) {
  if (!length(path) || !nzchar(path))
    stop("`path` must be the path to a file.", call. = FALSE)
  if (is.null(type)) type <- tools::file_ext(path)
  type <- tolower(type)

  switch(type,
    csv = utils::read.csv(path, sep = sep, dec = dec,
                          fileEncoding = fileEncoding,
                          stringsAsFactors = FALSE, check.names = FALSE, ...),
    xlsx = ,
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE))
        stop("Package 'readxl' is required to read Excel files. ",
             "Install it with: install.packages(\"readxl\")", call. = FALSE)
      as.data.frame(readxl::read_excel(path, sheet = sheet, ...))
    },
    rds = readRDS(path),
    stop("Unsupported file type: ", type,
         ". Use a .csv, .xlsx, .xls or .rds file, or name the type with ",
         "`type =`.", call. = FALSE)
  )
}

# ── eq5d_apply_mapping ────────────────────────────────────────────────────────

#' Rename a data frame's columns to the names the analyses expect
#'
#' The analysis functions take their columns by name. This renames the columns
#' a user has identified to the canonical names those functions use -- "mo",
#' "sc", "ua", "pd", "ad" for the dimensions, and "fu", "groupvar", "id",
#' "vas" and "utility" for the optional ones -- and coerces the dimensions to
#' integer and the EQ VAS to numeric.
#'
#' @param df A data frame of EQ-5D responses.
#' @param mapping A named list describing the columns. Recognised entries:
#'   \describe{
#'     \item{\code{names_eq5d}}{character vector of five dimension columns, in
#'       the order mobility, self-care, usual activities, pain/discomfort,
#'       anxiety/depression.}
#'     \item{\code{name_fu}, \code{name_groupvar}, \code{name_id},
#'       \code{name_vas}, \code{name_utility}}{single column names, or
#'       \code{NULL} where there is none.}
#'   }
#'   Other entries, such as \code{eq5d_version} or \code{levels_fu}, are
#'   ignored here and used by the analysis functions.
#' @return \code{df} with the mapped columns renamed and coerced. Columns not
#'   named in \code{mapping} are left untouched.
#' @seealso \code{\link{eq5d_validate}}, which checks the same mapping.
#' @keywords internal
eq5d_apply_mapping <- function(df, mapping) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  std_dims <- c("mo", "sc", "ua", "pd", "ad")

  for (i in seq_along(std_dims)) {
    orig <- mapping$names_eq5d[i]
    std  <- std_dims[i]
    if (!is.null(orig) && !is.na(orig) && nzchar(orig) && orig != std &&
        orig %in% names(df)) {
      names(df)[names(df) == orig] <- std
    }
  }
  for (d in std_dims)
    if (d %in% names(df)) df[[d]] <- suppressWarnings(as.integer(df[[d]]))

  rename_col <- function(df, from, to) {
    if (!is.null(from) && !is.na(from) && nzchar(from) && from != to &&
        from %in% names(df)) {
      names(df)[names(df) == from] <- to
    }
    df
  }
  df <- rename_col(df, mapping$name_fu,       "fu")
  df <- rename_col(df, mapping$name_groupvar, "groupvar")
  df <- rename_col(df, mapping$name_id,       "id")
  df <- rename_col(df, mapping$name_vas,      "vas")
  df <- rename_col(df, mapping$name_utility,  "utility")

  if ("vas" %in% names(df)) df$vas <- suppressWarnings(as.numeric(df$vas))

  df
}

# ── eq5d_validate ─────────────────────────────────────────────────────────────

#' Check a data frame of EQ-5D responses against its column mapping
#'
#' Runs the checks the Shiny app's Validation page runs: that the dimension
#' columns are there, that their values are levels the instrument allows, how
#' much is missing, whether patient IDs repeat in a way the timepoint column
#' explains, and whether EQ VAS scores are in range.
#'
#' Each finding is reported as it is found -- with \code{message()} for a
#' check that passed, \code{warning()} for one that found something, and for a
#' problem that stops the analysis either \code{stop()} or a warning, according
#' to \code{stop_on_error}. The findings are also returned, so a caller that
#' wants to present them itself can.
#'
#' @param df A data frame, as returned by \code{\link{eq5d_apply_mapping}} or
#'   with the user's own column names.
#' @param mapping A named list of column names; see
#'   \code{\link{eq5d_apply_mapping}}. \code{eq5d_version} decides the levels
#'   the dimensions may take.
#' @param stop_on_error Whether a finding of type \code{"error"} should stop
#'   execution. \code{FALSE} (default) reports it as a warning and carries on,
#'   which is what an interactive caller wants; a script will usually want
#'   \code{TRUE}.
#' @param quiet Whether to suppress the messages, warnings and errors and only
#'   return the findings.
#' @return Invisibly, a data frame with one row per finding and the columns
#'   \code{type} (\code{"ok"}, \code{"warning"} or \code{"error"}) and
#'   \code{message}. The messages are plain text.
#' @seealso \code{\link{eq5d_apply_mapping}}
#' @keywords internal
eq5d_validate <- function(df, mapping, stop_on_error = FALSE, quiet = FALSE) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  found <- list()
  note <- function(type, ...) {
    found[[length(found) + 1L]] <<- list(type = type, message = paste0(...))
  }

  n_rows <- nrow(df)
  note("ok", format(n_rows, big.mark = ","), " rows loaded.")

  eq5d_cols <- mapping$names_eq5d
  gone <- eq5d_cols[!eq5d_cols %in% names(df)]
  if (length(gone)) {
    note("error", "EQ-5D columns not found in data: ",
         paste(gone, collapse = ", "), ".")
  } else {
    max_level <- if (identical(mapping$eq5d_version, "3L")) 3L else 5L
    out_of_range <- vapply(eq5d_cols, function(col) {
      v <- suppressWarnings(as.integer(df[[col]]))
      any(!is.na(v) & (v < 1L | v > max_level))
    }, logical(1L))

    if (any(out_of_range)) {
      note("warning", "Some values in [",
           paste(eq5d_cols[out_of_range], collapse = ", "),
           "] are outside the expected range (1\u2013", max_level,
           ") and will be set to NA.")
    } else {
      note("ok", "All EQ-5D values within expected range (1\u2013",
           max_level, ").")
    }

    n_complete <- sum(stats::complete.cases(
      lapply(eq5d_cols, function(col) suppressWarnings(as.integer(df[[col]])))))
    n_miss <- n_rows - n_complete
    if (n_miss > 0L) {
      note("warning", n_miss, " rows (",
           round(100 * n_miss / n_rows, 1), "%) have missing EQ-5D values.")
    } else {
      note("ok", "No missing EQ-5D values.")
    }
  }

  if (.has_col(mapping$name_id, df)) {
    ids <- df[[mapping$name_id]]
    n_dup <- sum(duplicated(ids))
    if (n_dup == 0L) {
      note("ok", "No duplicate patient IDs.")
    } else if (.has_col(mapping$name_fu, df)) {
      fu_vals <- df[[mapping$name_fu]]
      n_dup_pairs <- sum(duplicated(paste(ids, fu_vals, sep = "\u00b7")))
      n_tp <- length(unique(fu_vals))
      if (n_dup_pairs == 0L) {
        note("ok", "Repeated patient IDs detected across ", n_tp,
             " timepoints \u2014 this is expected for longitudinal data. ",
             "All ID\u2013timepoint combinations are unique.")
      } else {
        note("warning", n_dup_pairs, " ID\u2013timepoint combinations are ",
             "duplicated. Check for duplicate records.")
      }
    } else {
      note("warning", n_dup, " repeated patient IDs. For cross-sectional ",
           "data each row should have a unique ID. If this is longitudinal ",
           "data, map a Timepoint variable.")
    }
  }

  # Follow-up levels the user did not list.
  if (.has_col(mapping$name_fu, df) && length(mapping$levels_fu)) {
    seen <- unique(as.character(df[[mapping$name_fu]]))
    seen <- seen[!is.na(seen)]
    unlisted <- setdiff(seen, as.character(mapping$levels_fu))
    if (length(unlisted)) {
      note("warning", length(unlisted), " value(s) of \"", mapping$name_fu,
           "\" are not in the timepoint order given (",
           paste(utils::head(unlisted, 10L), collapse = ", "),
           "); rows holding them become NA.")
    }
  }

  if (.has_col(mapping$name_vas, df)) {
    v <- suppressWarnings(as.numeric(df[[mapping$name_vas]]))
    if (any(!is.na(v) & (v < 0 | v > 100))) {
      note("warning",
           "Some VAS values are outside the expected range (0\u2013100).")
    } else {
      note("ok", "VAS values within expected range (0\u2013100).")
    }
  }

  out <- data.frame(
    type    = vapply(found, `[[`, character(1L), "type"),
    message = vapply(found, `[[`, character(1L), "message"),
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    for (i in seq_len(nrow(out))) {
      switch(out$type[i],
        ok      = message(out$message[i]),
        warning = warning(out$message[i], call. = FALSE),
        error   = if (isTRUE(stop_on_error)) stop(out$message[i], call. = FALSE)
                  else warning(out$message[i], call. = FALSE))
    }
  }

  invisible(out)
}

# Is `name` a column of `df`?
.has_col <- function(name, df) {
  !is.null(name) && length(name) == 1L && !is.na(name) && nzchar(name) &&
    name %in% names(df)
}

# ── eq5d_age_band_midpoint ────────────────────────────────────────────────────

#' The midpoint of each age band
#'
#' Turns a column of age bands -- "30 to 39", "30-39", "65+", "under 20" -- into
#' the age at the middle of each band, for use where an analysis needs a number
#' and the data hold a band. The NICE Decision Support Unit's mapping between
#' the EQ-5D-3L and EQ-5D-5L (\code{\link{eqxwr_UK}},
#' \code{\link{eqxw_UK}}) is the case this exists for.
#'
#' @details
#' Age is recorded in completed years, so the band "30 to 39" covers the
#' continuous interval \eqn{[30, 40)} and its midpoint is 35, not 34.5. The
#' distinction decides the band a respondent is mapped into: the DSU's age
#' bands begin at 35, 45, 55 and 65, which are exactly those midpoints, so a
#' band that straddles one of those boundaries is placed in the upper one.
#'
#' A band left open at the top, such as "65+", is treated as ten years wide.
#'
#' Using a midpoint makes the result an approximation. Where exact ages are
#' available, use them.
#'
#' @param x A vector of age bands, or of ages. A column that is already
#'   numeric, or whose every value reads as a number, is returned as it is:
#'   there is nothing to infer.
#' @return A numeric vector of the same length as \code{x}, with two
#'   attributes: \code{banded}, \code{TRUE} when \code{x} held bands rather
#'   than numbers, and \code{straddles}, a logical vector marking the values
#'   whose band crosses one of the DSU's boundaries.
#' @seealso \code{\link{eqxwr_UK}}, \code{\link{eqxw_UK}}
#' @keywords internal
eq5d_age_band_midpoint <- function(x) {
  breaks <- .NICE_AGE_BREAKS
  chr <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(chr))

  if (is.numeric(x) || all(is.na(chr) | !is.na(num))) {
    return(structure(num, banded = FALSE,
                     straddles = rep(FALSE, length(num))))
  }

  lo <- suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", chr)))
  hi <- suppressWarnings(as.numeric(sub("^.*?[0-9]+[^0-9]+([0-9]+).*$", "\\1", chr)))
  open <- !is.na(lo) & is.na(hi)
  hi[open] <- lo[open] + 9

  mid <- (lo + hi + 1) / 2
  mid[is.na(lo)] <- NA_real_

  inner <- breaks[-1L]
  straddles <- !is.na(lo) & !is.na(hi) &
    vapply(seq_along(lo), function(i) {
      if (is.na(lo[i])) return(FALSE)
      any(inner > lo[i] & inner <= hi[i])
    }, logical(1L))

  structure(mid, banded = TRUE, straddles = straddles)
}

# ── eq5d_format_table ─────────────────────────────────────────────────────────

#' Format an analysis table for display
#'
#' Presents the output of an analysis function the way the Shiny app does:
#' proportions as percentages, counts with a thousands separator and no
#' decimals, other numbers to a fixed number of places, and column headings
#' with the underscores removed.
#'
#' @details
#' The analysis functions return proportions, whatever the column is called:
#' \code{Percentage} in \code{\link{eq5d_profile_top_states}} holds 0.225, and
#' so does \code{\%} in \code{\link{eq5d_profile_lfs_distribution}}. A column
#' is treated as a proportion when it is named \code{freq_*}; when it is a
#' \code{*_p} column whose matching \code{*_n} is also present, which is the
#' shape the PCHC tables produce; when it ends in "\% Total" or "\% Type"; or
#' when it is one of \code{p}, \code{cum_p}, \code{Percentage},
#' \code{Cumulative percentage}, \code{\%} or \code{Cum (\%)}.
#'
#' Every column becomes character, so the result is for reading, not for
#' further computation. The table passed in is not modified.
#'
#' @param df A data frame returned by an analysis function.
#' @param percent_digits Decimal places for the percentages.
#' @param digits Decimal places for the other numbers.
#' @param tidy_names Whether to tidy the column headings. Underscores stop
#'   Word and HTML from breaking a heading across lines, so a heading like
#'   \code{freq_Post-op_mo} forces a wide column; tidied, it reads
#'   "mo Post-op \%".
#' @return A data frame of character columns.
#' @keywords internal
eq5d_format_table <- function(df, percent_digits = 1L, digits = 2L,
                              tidy_names = TRUE) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  pct   <- .proportion_columns(df)
  whole <- .whole_number_columns(df, except = pct)

  for (cn in names(df)) {
    v <- df[[cn]]
    if (!is.numeric(v)) next
    df[[cn]] <- if (cn %in% pct) {
      ifelse(is.na(v), "",
             paste0(formatC(100 * v, format = "f", digits = percent_digits), "%"))
    } else if (cn %in% whole || is.integer(v)) {
      ifelse(is.na(v), "", formatC(v, format = "d", big.mark = ","))
    } else {
      ifelse(is.na(v), "", formatC(v, format = "f", digits = digits))
    }
  }
  if (isTRUE(tidy_names)) names(df) <- tidy_table_headings(names(df))
  df
}

# Which columns of an analysis table hold proportions. See eq5d_format_table().
.proportion_columns <- function(df) {
  nm <- names(df)
  paired_p <- nm[grepl("_p$", nm) & sub("_p$", "_n", nm) %in% nm]
  unique(c(
    nm[grepl("^freq_", nm)],
    paired_p,
    nm[grepl("% Total$|% Type$", nm)],
    intersect(c("p", "cum_p", "Percentage", "Cumulative percentage",
                "%", "Cum (%)"), nm)
  ))
}

# Double columns that only ever hold whole numbers. Counts come back as
# doubles from aggregate() and merge(), and "255.00" where the table means 255
# people is a distraction.
.whole_number_columns <- function(df, except = character(0L)) {
  nm <- setdiff(names(df)[vapply(df, is.double, logical(1L))], except)
  nm[vapply(nm, function(cn) {
    v <- df[[cn]][is.finite(df[[cn]])]
    length(v) > 0L && all(v == round(v))
  }, logical(1L))]
}
