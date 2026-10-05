# The schema shared by country_codes and the user_defined_* tables.

test_that(".vs_meta_schema() is derived from the built-in country table", {
  schema <- .vs_meta_schema()

  expect_type(schema, "character")
  expect_identical(names(schema), colnames(.cntrcodes))
  expect_true(all(c("Version", "Name", "Name_short", "Country_code",
                    "VS_code", "doi", "citation") %in% names(schema)))
})

test_that(".new_vs_meta_row() matches the schema exactly", {
  schema <- .vs_meta_schema()

  row <- .new_vs_meta_row(Version = "3L", Name = "Fantasia",
                          Country_code = "FA", VS_code = "FAN")

  expect_s3_class(row, "data.frame")
  expect_identical(nrow(row), 1L)
  expect_identical(colnames(row), unname(names(schema)))
  expect_identical(vapply(row, function(x) class(x)[1L], character(1L)),
                   schema)
  expect_length(.validate_vs_meta(row), 0L)
})

test_that(".new_vs_meta_row() fills omitted columns with typed NA", {
  row <- .new_vs_meta_row(Version = "3L", VS_code = "FAN")

  # NA, not logical NA: the column type must still be character.
  expect_true(is.na(row$citation))
  expect_type(row$citation, "character")
  expect_true(is.na(row$Name))
  expect_type(row$Name, "character")
})

test_that(".new_vs_meta_row() rejects unknown columns", {
  expect_error(.new_vs_meta_row(Version = "3L", Nonsense = "x"),
               "Unknown value set metadata column")
})

test_that(".validate_vs_meta() names exactly what is wrong", {
  good <- .new_vs_meta_row(Version = "3L", VS_code = "FAN")

  expect_length(.validate_vs_meta(good), 0L)

  expect_match(.validate_vs_meta(NULL), "is missing")
  expect_match(.validate_vs_meta(list(a = 1)), "not a data.frame")

  missing_col <- good[, setdiff(colnames(good), "citation"), drop = FALSE]
  expect_match(.validate_vs_meta(missing_col), "missing the column\\(s\\) citation")

  extra_col <- good
  extra_col$surprise <- "x"
  expect_match(.validate_vs_meta(extra_col), "unexpected column\\(s\\) surprise")

  wrong_order <- good[, rev(colnames(good)), drop = FALSE]
  expect_match(.validate_vs_meta(wrong_order), "wrong order")

  wrong_type <- good
  wrong_type$doi <- NA          # logical, not character
  expect_match(.validate_vs_meta(wrong_type), "wrong type for doi")
})

test_that(".migrate_vs_meta() repairs a legacy table", {
  schema <- .vs_meta_schema()

  legacy <- legacy_user_objects("3L")$user_defined_3L
  expect_false("citation" %in% colnames(legacy))

  fixed <- .migrate_vs_meta(legacy)

  expect_length(.validate_vs_meta(fixed), 0L)
  expect_identical(colnames(fixed), unname(names(schema)))
  expect_true(is.na(fixed$citation))
  # Existing values survive the migration untouched.
  expect_identical(fixed$VS_code, "FAN")
  expect_identical(fixed$doi, "doi:legacy")
})

test_that(".migrate_vs_meta() gives up on tables it cannot repair", {
  expect_null(.migrate_vs_meta(NULL))
  expect_null(.migrate_vs_meta("not a table"))
  # Nothing identifies the value set, so there is nothing to keep.
  expect_null(.migrate_vs_meta(data.frame(a = 1, b = 2)))
})
