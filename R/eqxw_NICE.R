# -----------------------------------------------------------------------------
# Shared internals for the NICE DSU mappings used by eqxw_UK() and eqxwr_UK().
#
# Both functions do the same work in opposite directions: resolve the input to
# health states or to EQ-5D values, band the ages, validate sex, then either
# look the mapped value up directly or, for a value ("score") input, take an
# Epanechnikov kernel-weighted mean of the mapped values within the
# respondent's age band and sex. Everything below is internal.
#
# inst/COPYRIGHTS records where the lookup tables come from, what was kept
# from them, and the citations the NICE DSU asks for.
# -----------------------------------------------------------------------------

# Direction metadata. `source` is the instrument the input is measured on.
.NICE_DIRECTIONS <- list(
  "5Lto3L" = list(source = "5L", target = "3L", max_level = 5L),
  "3Lto5L" = list(source = "3L", target = "5L", max_level = 3L)
)

# Lower bounds of the DSU age bands. Band 1 starts at 16: the DSU's R command
# admits ages from 16 although its table label reads "18-34", and its Stata
# help defines band 1 as "< 35".
.NICE_AGE_BREAKS <- c(16, 35, 45, 55, 65)

# DSU-recommended bandwidths for mapping mean (aggregate) values.
.NICE_BW_THRESHOLD <- 0.6
.NICE_BW_ABOVE     <- 0.1
.NICE_BW_AT_OR_BELOW <- 0.4

# The DSU treats a bandwidth of zero as this, giving an effectively exact match.
.NICE_BW_ZERO <- 1e-6


# Fetch a direction's lookup table from the package environment.
.nice_table <- function(direction, .fname) {
  pkgenv <- getOption("eq.env")
  if (is.null(pkgenv) || is.null(pkgenv$crosswalk_NICE[[direction]]))
    stop("Missing NICE crosswalk data in `options(eq.env)`. Please set the environment.",
         call. = FALSE)
  pkgenv$crosswalk_NICE[[direction]]
}


# Age in years -> band index 1-5, or NA.
#
# Ages below 16 are outside the DSU's estimation sample and return NA with a
# warning. There is no upper limit. Values 1-5 are ordinary ages here and are
# never silently read as band numbers, which is what the DSU's own R command
# does.
.nice_age_band <- function(age, .fname) {
  age <- suppressWarnings(as.numeric(age))
  band <- rep(NA_integer_, length(age))

  ok <- !is.na(age) & age >= .NICE_AGE_BREAKS[1L]
  band[ok] <- findInterval(age[ok], .NICE_AGE_BREAKS)

  too_young <- !is.na(age) & age < .NICE_AGE_BREAKS[1L]
  if (any(too_young))
    warning(sprintf(
      "[%s] %d age(s) below %d; the NICE mapping is not defined for them and they return NA.",
      .fname, sum(too_young), .NICE_AGE_BREAKS[1L]), call. = FALSE)

  band
}


# Sex -> 1 (male) / 0 (female), or NA. TRUE/FALSE are accepted.
.nice_male <- function(male, .fname) {
  if (is.logical(male)) return(as.integer(male))

  # A factor must go via its labels: as.numeric() on a factor returns the level
  # index, so factor("m") would otherwise silently become 1, i.e. "male".
  if (is.factor(male)) male <- as.character(male)

  m <- suppressWarnings(as.numeric(male))
  out <- rep(NA_integer_, length(male))
  out[!is.na(m) & m == 1] <- 1L
  out[!is.na(m) & m == 0] <- 0L

  # "Bad" means supplied but unusable. A value that was already NA is missing,
  # not invalid, and passes through silently.
  bad <- !is.na(male) & is.na(out)
  if (any(bad))
    warning(sprintf(
      "[%s] %d value(s) of `male` are neither 0 nor 1 and return NA: %s.",
      .fname, sum(bad),
      paste(utils::head(unique(male[bad]), 5L), collapse = ", ")), call. = FALSE)

  out
}


# Resolve `x`, `age` and `male` to three equal-length vectors.
#
# `x` may be a vector of state indexes or values, or a matrix/data.frame of
# dimension columns. When `x` is a data.frame, `age` and `male` may name
# columns in it or be supplied as vectors.
.nice_inputs <- function(x, age, male, dim.names, max_level, .fname) {

  pick <- function(v, nm) {
    if (is.character(v) && length(v) == 1L && is.data.frame(x) && v %in% names(x))
      return(x[[v]])
    v
  }

  is_2d <- length(dim(x)) == 2L

  if (is_2d) {
    cn <- tolower(colnames(x))
    has_dims <- !is.null(cn) && all(tolower(dim.names) %in% cn)

    if (has_dims) {
      age  <- pick(age,  "age")
      male <- pick(male, "male")
      dims <- as.data.frame(x)[, match(tolower(dim.names), cn), drop = FALSE]
      state <- suppressMessages(toEQ5Dindex(as.matrix(dims), quiet = TRUE))
      score <- NULL
    } else if (ncol(x) == 5L) {
      age  <- pick(age,  "age")
      male <- pick(male, "male")
      state <- suppressMessages(toEQ5Dindex(as.matrix(x), quiet = TRUE))
      score <- NULL
    } else {
      stop(sprintf(
        "[%s] `x` has %d column(s); expected the five dimension columns %s.",
        .fname, ncol(x), paste(dim.names, collapse = ", ")), call. = FALSE)
    }
  } else {
    v <- x
    chr <- trimws(as.character(v))
    # A health state is a 5-digit code whose digits are all valid levels for
    # the source instrument. Anything else is treated as an EQ-5D value.
    pat <- sprintf("^[1-%d]{5}$", max_level)
    looks_state <- !is.na(chr) & grepl(pat, chr)
    if (all(looks_state | is.na(chr))) {
      state <- suppressWarnings(as.integer(chr)); score <- NULL
    } else {
      state <- NULL; score <- suppressWarnings(as.numeric(v))
    }
  }

  n <- if (is.null(state)) length(score) else length(state)
  if (length(age)  == 1L) age  <- rep(age,  n)
  if (length(male) == 1L) male <- rep(male, n)
  if (length(age) != n || length(male) != n)
    stop(sprintf("[%s] `age` and `male` must be length 1 or %d; got %d and %d.",
                 .fname, n, length(age), length(male)), call. = FALSE)

  list(state = state, score = score, age = age, male = male, n = n)
}


# Resolve the bandwidth to one value per row.
#
# Numeric values are recycled. The string "default" selects the DSU's
# recommendation for mean (aggregate) values: 0.1 above 0.6, 0.4 at or below.
# Zero becomes 1e-6, as in both DSU implementations.
.nice_bwidth <- function(bwidth, score, n, .fname) {
  if (is.character(bwidth)) {
    if (!identical(tolower(bwidth[1L]), "default"))
      stop(sprintf("[%s] `bwidth` must be numeric or \"default\"; got \"%s\".",
                   .fname, bwidth[1L]), call. = FALSE)
    bw <- ifelse(!is.na(score) & score > .NICE_BW_THRESHOLD,
                 .NICE_BW_ABOVE, .NICE_BW_AT_OR_BELOW)
  } else {
    bw <- suppressWarnings(as.numeric(bwidth))
    if (length(bw) == 1L) bw <- rep(bw, n)
    if (length(bw) != n)
      stop(sprintf("[%s] `bwidth` must be length 1 or %d; got %d.",
                   .fname, n, length(bw)), call. = FALSE)
  }
  bw[!is.na(bw) & bw == 0] <- .NICE_BW_ZERO
  bw
}


# Epanechnikov weights: 1 - (d/bw)^2, truncated to 0 outside the bandwidth.
.nice_epan <- function(values, target, bwidth) {
  d <- (values - target) / bwidth
  ifelse(abs(d) >= 1, 0, 1 - d^2)
}


# The worker shared by eqxw_UK() and eqxwr_UK().
.eqxw_NICE <- function(x, age, male, direction,
                       dim.names = c("mo", "sc", "ua", "pd", "ad"),
                       bwidth = 0, .fname = "eqxw_NICE") {

  if (length(dim.names) != 5L)
    stop(sprintf("[%s] `dim.names` must be of length 5.", .fname), call. = FALSE)

  meta <- .NICE_DIRECTIONS[[direction]]
  cw   <- .nice_table(direction, .fname)

  inp  <- .nice_inputs(x, age, male, dim.names, meta$max_level, .fname)
  band <- .nice_age_band(inp$age,  .fname)
  sex  <- .nice_male(inp$male, .fname)

  out <- rep(NA_real_, inp$n)

  if (!is.null(inp$state)) {
    # ---- health state input: direct lookup, order preserved by match() ------
    valid <- !is.na(inp$state) & !is.na(band) & !is.na(sex)
    idx <- match(paste(inp$state[valid], band[valid], sex[valid], sep = "|"),
                 paste(cw$state,          cw$age,     cw$male,    sep = "|"))
    out[valid] <- cw$value_to[idx]

    unmatched <- valid & is.na(out)
    if (any(unmatched))
      warning(sprintf(
        "[%s] %d health state(s) not found in the mapping table and return NA.",
        .fname, sum(unmatched)), call. = FALSE)

  } else {
    # ---- value ("score") input: Epanechnikov kernel within age band and sex --
    bw    <- .nice_bwidth(bwidth, inp$score, inp$n, .fname)
    valid <- !is.na(inp$score) & !is.na(band) & !is.na(sex) & !is.na(bw) & bw > 0

    # One kernel per distinct (band, sex) cell, so the table is subset once
    # per cell rather than once per row.
    cell <- paste(band, sex, sep = "|")
    cw_cell <- split(seq_len(nrow(cw)), paste(cw$age, cw$male, sep = "|"))

    for (k in unique(cell[valid])) {
      rows <- which(valid & cell == k)
      sub  <- cw_cell[[k]]
      if (is.null(sub)) next
      from <- cw$value_from[sub]
      to   <- cw$value_to[sub]
      out[rows] <- vapply(rows, function(i) {
        w <- .nice_epan(from, inp$score[i], bw[i])
        if (all(w == 0)) NA_real_ else sum(w * to) / sum(w)
      }, numeric(1L))
    }

    unmapped <- valid & is.na(out)
    if (any(unmapped))
      message(sprintf(
        "[%s] %d value(s) had no EQ-5D value within the bandwidth and return NA; consider increasing `bwidth`.",
        .fname, sum(unmapped)))
  }

  out
}
