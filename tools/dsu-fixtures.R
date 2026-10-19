# Rebuild the NICE DSU reference fixtures used by tests/testthat/test-dsu-validation.R
# from the DSU's own R command for mapping between the EQ-5D-3L and EQ-5D-5L.
#
#   Rscript tools/dsu-fixtures.R <DSU directory>            # compare only
#   Rscript tools/dsu-fixtures.R <DSU directory> --write    # also rewrite them
#
# <DSU directory> holds the files of the DSU's R command, April 2026 release,
# as published at
# https://www.sheffield.ac.uk/nice-dsu/methods-development/mapping-eq-5d-5l-3l
# The DSU publishes them without a licence, so they are not part of this
# repository: download them there. The script checks them against the SHA-256
# sums below, which are those of the files the shipped fixtures were checked
# against, and says if they differ (a new DSU release, say).
#
# The DSU command needs dplyr, purrr, tidyr, stringr and tibble. It is run as
# published: state input in one call per direction, and value input one row
# per call, because its "default" bandwidth rule does not accept more.
#
# This directory is excluded from the built package (.Rbuildignore).

expected_sha256 <- c(
  "EQ5Dmap_table3.csv"       = "fe6a9452e0fc1699e1c9194a9dab04761d9b54e157e20ca488b9cf350862df78",
  "EQ5Dmap_table5.csv"       = "d7fe634ac66d6317930f87de5258b77af5ab8b87ae9d32993759aaf9d5f6a782",
  "eq5d_mapping-functions.R" = "dae2d01d931a5707468ea2700413c7c52e7250822e799e0268f88ebe8807ff82"
)

args <- commandArgs(TRUE)
if (!length(args)) stop("Usage: Rscript tools/dsu-fixtures.R <DSU directory> [--write]")
dsu_dir <- normalizePath(args[1L], mustWork = TRUE)
write   <- "--write" %in% args
fixture_dir <- normalizePath(file.path("tests", "testthat"), mustWork = TRUE)

files <- file.path(dsu_dir, names(expected_sha256))
if (!all(file.exists(files)))
  stop("Not found in ", dsu_dir, ": ",
       paste(names(expected_sha256)[!file.exists(files)], collapse = ", "))
got <- tools::sha256sum(files)
names(got) <- names(expected_sha256)
changed <- names(got)[got != expected_sha256]
if (length(changed))
  message("Note: these DSU files differ from the ones the fixtures were checked ",
          "against: ", paste(changed, collapse = ", "))

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(stringr); library(tibble)
})
owd <- setwd(dsu_dir)
on.exit(setwd(owd), add = TRUE)
source("eq5d_mapping-functions.R")           # defines fun_map()
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr))))
  out
}

all_states <- function(top) {
  g <- expand.grid(rep(list(seq_len(top)), 5L))
  as.integer(rowSums(sweep(as.matrix(g[, 5:1]), 2L, 10L^(4:0), `*`)))
}

# ---- health state input: every state x age band x sex ------------------------
state_fixture <- function(output) {
  top <- if (output == "3L") 5L else 3L          # the input instrument's levels
  grid <- expand.grid(state = sort(all_states(top)), age = c(25, 40, 50, 60, 70),
                      male = c(0L, 1L))
  d <- data.frame(age = grid$age, male = grid$male,
                  do.call(rbind, lapply(strsplit(as.character(grid$state), ""), as.integer)))
  cols <- paste0("Y_", 1:5)
  names(d)[3:7] <- cols
  res <- quiet(fun_map(output = output, input_df = d, age_col = "age",
                       male_col = "male", qol_col = cols, bwidth = 0,
                       country = c("UK", "UK")))
  if (nrow(res) != nrow(grid) || !identical(as.integer(res$Domain), grid$state) ||
      !identical(as.integer(res$X_male), grid$male))
    stop("fun_map() did not return the rows in input order (", output, ")")
  data.frame(state = grid$state, age = grid$age, male = grid$male,
             dsu = res[[paste0("util", output)]])
}

# ---- value input: values x bandwidths x age bands x sex, one row per call ----
value_fixture <- function(output) {
  grid <- expand.grid(score = c(-0.4, -0.2, 0, 0.2, 0.4, 0.55, 0.6, 0.65, 0.8, 0.9, 0.95, 1),
                      bwidth = c("0", "0.05", "0.1", "0.4", "default"),
                      age = c(25, 50, 70), male = c(0L, 1L),
                      stringsAsFactors = FALSE)
  one <- function(score, bwidth, age, male) {
    bw <- if (bwidth == "default") "default" else as.numeric(bwidth)
    res <- quiet(fun_map(output = output, input_df = data.frame(age = age, male = male, util = score),
                         age_col = "age", male_col = "male", qol_col = "util",
                         bwidth = bw, country = c("UK", "UK")))
    num <- unlist(res[vapply(res, is.numeric, logical(1L))])
    unname(num[length(num)])
  }
  grid$dsu <- unlist(Map(one, grid$score, grid$bwidth, grid$age, grid$male))
  grid
}

fixtures <- list(
  "fixture-dsu-state-5Lto3L.rds" = function() state_fixture("3L"),
  "fixture-dsu-state-3Lto5L.rds" = function() state_fixture("5L"),
  "fixture-dsu-value-5Lto3L.rds" = function() value_fixture("3L"),
  "fixture-dsu-value-3Lto5L.rds" = function() value_fixture("5L"))

ok <- TRUE
for (f in names(fixtures)) {
  message("Building ", f, " ...")
  new <- fixtures[[f]]()
  path <- file.path(fixture_dir, f)
  if (file.exists(path)) {
    old <- readRDS(path)
    same <- isTRUE(all.equal(old, new, tolerance = 0, check.attributes = FALSE))
    diff <- suppressWarnings(max(abs(old$dsu - new$dsu), na.rm = TRUE))
    message(sprintf("  %s: %d rows; %s the shipped fixture (largest difference %.3g)",
                    f, nrow(new), if (same) "identical to" else "differs from", diff))
    ok <- ok && same
  }
  if (write) saveRDS(new, path, version = 2)
}
if (!write && !ok) quit(status = 1)
