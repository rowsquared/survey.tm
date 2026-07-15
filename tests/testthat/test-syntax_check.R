# Helper: one-language 'Translation Database' with a text item that HAS an html-tag issue
# (Text_Item carries <b>..</b>, Translation does not) and a Translation present.
make_tdb <- function(status,
                     text_item = "<b>ADMIN 0</b>",
                     translation = "ADMIN 0 without tag") {
  list(German = data.table(
    value.unique = "admin0",
    `Questionnaire(s)` = "qx",
    Type = "Title",
    Text_Item = text_item,
    Translation = translation,
    Status = status,
    `Comment/Note` = NA_character_
  ))
}

run_check <- function(...) suppressMessages(syntax_check(...))


test_that("syntax_check flags a genuine html-tag issue", {
  # Positive control: the check itself must keep working
  result <- run_check(tdb = make_tdb("translated"))

  expect_equal(result$German$Status, "to be checked")
  expect_match(result$German$`Comment/Note`, "Difference in count of html-tag")
})


test_that("syntax_check does not touch a text item that is not to be translated", {
  # The item has a Translation and a real html issue, but must not be flagged
  result <- run_check(tdb = make_tdb("don't translate"))

  expect_equal(result$German$Status, "don't translate")
  expect_true(is.na(result$German$`Comment/Note`))
})


test_that("syntax_check skips kept statuses for text substitution issues too", {
  result <- run_check(tdb = make_tdb(
    "don't translate",
    text_item = "%country% ADMIN 0",
    translation = "ADMIN 0 without substitution"
  ))

  expect_equal(result$German$Status, "don't translate")
  expect_true(is.na(result$German$`Comment/Note`))
})


test_that("syntax_check matches keep_statuses regardless of case and apostrophe variant", {
  variants <- c("Don't Translate", "DON'T TRANSLATE", "don’t translate", "  don't translate  ")

  for (variant in variants) {
    result <- run_check(tdb = make_tdb(variant))
    expect_equal(result$German$Status, variant, info = paste("status variant:", variant))
  }
})


test_that("syntax_check does not flag outdated items even if keep_statuses omits it", {
  result <- run_check(tdb = make_tdb("outdated"), keep_statuses = "don't translate")

  expect_equal(result$German$Status, "outdated")
})


test_that("syntax_check honours a user supplied keep_statuses", {
  result <- run_check(tdb = make_tdb("no translation needed"),
                      keep_statuses = "no translation needed")

  expect_equal(result$German$Status, "no translation needed")
})


test_that("syntax_check passes 'pattern' through to the text substitution check", {
  # Token uses {braces}, which the default pattern (%token%) does not match
  tdb <- make_tdb("translated",
                  text_item = "{country} ADMIN 0",
                  translation = "ADMIN 0 without substitution")

  # Default pattern: nothing to find, so the item is left alone
  expect_equal(run_check(tdb = tdb)$German$Status, "translated")

  # Custom pattern must actually reach get_txt_sub_issue_dt()
  result <- run_check(tdb = tdb, pattern = "\\{[a-zA-Z0-9_]+\\}")
  expect_equal(result$German$Status, "to be checked")
  expect_match(result$German$`Comment/Note`, "\\{country\\} not found in Translation")
})
