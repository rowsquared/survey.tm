# All three items are still present in the source questionnaire
source_titems_testthat <- data.table(
  value.unique = c("admin0", "admin1", "admin2"),
  seq.id = 1:3,
  questionnaire = "qx",
  value = c("ADMIN 0", "ADMIN 1", "ADMIN 2"),
  type = "Title"
)

# Helper: build a one-language 'Translation Database' as read from the Google Sheet
make_tdb <- function(status, translation = NA_character_) {
  list(German = data.table(
    value.unique = "admin0",
    `Questionnaire(s)` = "qx",
    Type = "Title",
    Text_Item = "ADMIN 0",
    Status = status,
    Translation = translation,
    `Comment/Note` = NA_character_
  ))
}


test_that("update_tdb does not reset a status that legitimately has no Translation", {
  result <- update_tdb(
    tdb = make_tdb("don't translate"),
    source_titems = source_titems_testthat
  )

  expect_equal(result$German[value.unique == "admin0", Status], "don't translate")
})


test_that("update_tdb matches keep_statuses regardless of case and apostrophe variant", {
  # Google Sheets may hold a typographic apostrophe (U+2019) and arbitrary casing
  variants <- c("Don't Translate", "DON'T TRANSLATE", "don’t translate", "  don't translate  ")

  for (variant in variants) {
    result <- update_tdb(
      tdb = make_tdb(variant),
      source_titems = source_titems_testthat
    )
    expect_equal(
      result$German[value.unique == "admin0", Status], variant,
      info = paste("status variant:", variant)
    )
  }
})


test_that("update_tdb still flags an empty Translation whose status claims otherwise", {
  # Translator cleared the Translation but left Status = 'translated' -> inconsistent
  result <- update_tdb(
    tdb = make_tdb("translated"),
    source_titems = source_titems_testthat
  )

  expect_equal(result$German[value.unique == "admin0", Status], "to translate")
})


test_that("update_tdb sets a blank status without Translation to 'to translate'", {
  result <- update_tdb(
    tdb = make_tdb(NA_character_),
    source_titems = source_titems_testthat
  )

  expect_equal(result$German[value.unique == "admin0", Status], "to translate")
})


test_that("update_tdb honours a user supplied keep_statuses", {
  result <- update_tdb(
    tdb = make_tdb("no translation needed"),
    source_titems = source_titems_testthat,
    keep_statuses = "no translation needed"
  )

  expect_equal(result$German[value.unique == "admin0", Status], "no translation needed")
})


test_that("update_tdb keeps 'outdated' even if keep_statuses omits it", {
  # 'outdated' drives the reappeared/removed logic and must not be overridable away
  tdb <- make_tdb("translated", translation = "Verwaltung 0")
  # Item is no longer part of the source questionnaire
  result <- update_tdb(
    tdb = tdb,
    source_titems = source_titems_testthat[value.unique != "admin0"],
    keep_statuses = "don't translate"
  )

  expect_equal(result$German[value.unique == "admin0", Status], "outdated")
})
