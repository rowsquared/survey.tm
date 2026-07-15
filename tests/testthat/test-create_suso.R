# Question coding as it is used in `parse_suso_titems()`
qcode_pattern_testthat <- "^[A-Za-z]+\\d+[A-Za-z0-9._]*\\.\\s+"

# Minimal source questionnaire with two text items that both carry a question code
source_questionnaire_testthat <- list(
  Translations = data.table(
    `Entity Id` = c("id-1", "id-2"),
    Variable = c("admin0", "admin1"),
    Type = c("Title", "Title"),
    Index = c(NA_character_, NA_character_),
    `Original text` = c("I1. ADMIN 0", "I2. ADMIN 1"),
    Translation = c(NA_character_, NA_character_)
  )
)

# Translation Database holding a translation for both items, but of different Status
tdb_language_testthat <- data.table(
  value.unique = c("admin0", "admin1"),
  `Questionnaire(s)` = c("test", "test"),
  Type = c("Title", "Title"),
  Text_Item = c("ADMIN 0", "ADMIN 1"),
  Status = c("translated", "don't translate"),
  Translation = c("Verwaltung 0", "Verwaltung 1"),
  `Comment/Note` = c(NA_character_, NA_character_)
)


test_that("create_suso_sheet keeps Translation NA when Status is excluded by 'statuses'", {
  # Mimic create_suso_file(): only 'translated' passes, "don't translate" is dropped
  result <- create_suso_sheet(
    source_questionnaire = source_questionnaire_testthat,
    sheet = "Translations",
    language.tdb.dt = tdb_language_testthat[Status %in% "translated"],
    qcode_pattern = qcode_pattern_testthat
  )

  # Question code is re-attached to the translated item
  expect_equal(result[`Entity Id` == "id-1", Translation], "I1. Verwaltung 0")
  # The excluded item stays NA and must not become the string "I2. NA"
  expect_true(is.na(result[`Entity Id` == "id-2", Translation]))

  # 'Original text' keeps its coding in both cases
  expect_equal(result$`Original text`, c("I1. ADMIN 0", "I2. ADMIN 1"))
})


test_that("create_suso_file writes an empty Translation cell for items excluded by 'statuses'", {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  create_suso_file(
    tdb.language = tdb_language_testthat,
    source_questionnaire = source_questionnaire_testthat,
    path = path,
    statuses = c("to translate", "machine", "reviewed", "translated"),
    qcode_pattern = qcode_pattern_testthat
  )

  result <- as.data.table(readxl::read_xlsx(path, sheet = "Translations"))

  expect_equal(result$Translation, c("I1. Verwaltung 0", NA_character_))
})
