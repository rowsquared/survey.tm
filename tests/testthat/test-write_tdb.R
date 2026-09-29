test_that("Translation Database columns are aligned before writing", {
  dt <- data.table(
    Translation = "Hallo", Status = "translated", value.unique = "hello",
    Text_Item = "Hello", Type = "Title", `Comment/Note` = NA_character_,
    `Questionnaire(s)` = "qx"
  )
  input_names <- names(dt)

  prepared <- prepare_tdb_write_data(dt)

  expect_named(prepared, c("value.unique", "Questionnaire(s)", "Type",
                           "Text_Item", "Status", "Translation", "Comment/Note"))
  expect_equal(prepared$Translation, "Hallo")
  expect_identical(names(dt), input_names)
  expect_error(prepare_tdb_write_data(copy(dt)[, extra := "unexpected"]),
               "unexpected column")
})
