#' Add provided user's Translation to a Survey Solutions Translation sheet
#'
#' @param sheet Sheet in `source_questionnaire` to which translation should be added
#' @param data.table of translation to be added to Questionnaire
#' @inheritParams create_suso_file
#'
#' @return data table of `sheet` with Translation added
#'
create_suso_sheet <- function(source_questionnaire = "",
                              sheet = "",
                              language.tdb.dt = "",
                              qcode_pattern = NULL) {
  # Get Sheet from List
  qx.tsheet <- copy(source_questionnaire[[sheet]])


  #Got all cols?
  check.cols <- check_susotemplate_cols(qx.tsheet)
  # Assuming check.cols is the result from the check_tdb_exact_cols function

  # Check if there are missing columns and assert accordingly
  if (!identical(check.cols$missing_cols, character(0))) {
    assertthat::assert_that(FALSE,
                            msg = paste0(
                              "Sheet '", sheet, "' is missing expected column(s): ",
                              paste(check.cols$missing_cols, collapse = ", ")
                            ))
  }

  # Check if there are extra columns and assert accordingly
  if (!identical(check.cols$extra_cols, character(0))) {
    assertthat::assert_that(FALSE,
                            msg = paste0(
                              "Sheet '", sheet, "' has extra unexpected column(s): ",
                              paste(check.cols$extra_cols, collapse = ", ")
                            ))
  }


  # Get column names for later
  names.sheet <- names(qx.tsheet)


  # GET NEW ROW IDENTIFIER IN QUESTIONNAIRE SHEET
  qx.tsheet[, rowid := 1:.N]

  # Remove Coding
  # Get col of code itself
  if (!is.null(qcode_pattern)) qx.tsheet[grepl(qcode_pattern, `Original text`), coding := stringr::str_extract(`Original text`, qcode_pattern)]
  # Remove Col from Original Text
  if (!is.null(qcode_pattern)) qx.tsheet[grepl(qcode_pattern, `Original text`), `Original text` := stringr::str_remove(`Original text`, qcode_pattern)]

  # CREATE MERGE.VAR
  create.unique.var(qx.tsheet, col = "Original text")
  #Remove '\r\n' if at end of string as it would not be added to Google Sheets
  create.unique.var(qx.tsheet, regex="((\\\r\\\n)+)$", col = "value.unique")


  # ADD TRANSLATION TO DT
  qx.tsheet <- merge(
    # The current QX Sheet, without Translation column
    qx.tsheet[, "Translation" := NULL],
    # The language sheet, subset by status user supplied
    language.tdb.dt[, .(value.unique, Translation)],
    by = "value.unique", all.x = T
  )


  # Check for duplicate Option Title
  if (any(duplicated(qx.tsheet[!is.na(Translation)& Type == "OptionTitle"], by = c("Entity Id", "Type", "Translation")))) {
    # Identify duplicates
    qx.tsheet[!is.na(Translation) & Type == "OptionTitle", dupl := .N > 1,
      by = c("Entity Id", "Type", "Translation")
    ]
    # Print them to console
    message("Attention, categories title must be unique.")
    message(paste(
      qx.tsheet[dupl == T, .(Translation = Translation[c(1)]), by = "Original text"][, .(text = paste0("\t", paste(paste(`Original text`, collapse = ", "), Translation, sep = ": "))), by = "Translation"]$text,
      collapse = "\n"
    ))

    message("Translation(s) for these text items will not be added to upload file")
    qx.tsheet[dupl == T, Translation := NA][, "dupl" := NULL]
  }

  # Get back Coding to Original Text and Translation
  # Get col of code itself
  if (!is.null(qcode_pattern)) {
    # Text items without Translation must stay NA, otherwise paste0() turns them into "<coding>NA"
    qx.tsheet[!is.na(coding), `:=`
    (
      `Original text` = paste0(coding, `Original text`),
      Translation = fifelse(is.na(Translation), NA_character_, paste0(coding, Translation))
    )][, "coding" := NULL]
  }

  # Correct (Col) Order
  setcolorder(qx.tsheet, c("Entity Id", "Variable", "Type", "Index", "Original text", "Translation"))
  setorder(qx.tsheet, rowid)
  qx.tsheet[, c("rowid", "value.unique") := NULL]

  #Lastly, remove \n\r as there is conversion issue between GS, R, Excel and SuSo
  qx.tsheet[grepl("\\\n|\\r",Translation),Translation:=gsub("\\\n|\\\r"," ",Translation)]

  # Return
  return(qx.tsheet)
}





#' Create Survey Solutions Translation file for 'Designer' Upload
#'
#' This function creates an .xlsx file based on the 'Translation Database' list and Survey Solutions source questionnaire.
#' The generated file can be uploaded to the Survey Solutions Designer.
#'
#' @param tdb.language data.table. Element (Language) of 'Translation Database' object returned by \code{\link{get_tdb_data}} or \code{\link{update_tdb}}
#' @param source_questionnaire data.table. 'Questionnaire Template' to which translation shall be added.  Usually an element of list returned by \code{\link{get_suso_tfiles}}
#' @param path Character. Writable file path where Translation File should be stored at, including file name and extension
#' @param sheets Character vector. For which sheets of questionnaire template file language will be added. Default all sheets that are found in template file
#' @param qcode_pattern Regular expression that matches question coding. Should be specified if used in `parse_suso_titems()`.
#' @param statuses Character vector. Which text items of which statuses from Translation Google Sheet should be merged?
#'
#' @importFrom "stats" "setNames"
#'
#' @export
#'

#' @examples
#' \dontrun{
#' # Take the 'German' Translation from database and merge into the source
#' questionnaire as returned by \code{\link{get_suso_tfiles}}
#' # Consider only text items of particular statuses defined by Translator
#' create_suso_file(
#'   tdb.language = new_tdb[["German"]],
#'   source_questionnaire = suso_trans_templates[["NAME-OF-QUESTIONNAIRE"]],
#'   statuses = c("Machine", "reviewed", "translated"),
#'   path = "your-path/German_NAME-OF-QUESTIONNAIRE.xlsx"
#' )
#' }
#'
create_suso_file <- function(tdb.language,
                             source_questionnaire = list(),
                             path = stop("'path' must be specified"),
                             sheets = NULL,
                             statuses = c("machine", "reviewed", "translated"),
                             qcode_pattern = NULL) {

  # CHECK INPUT -------------------------------------------------------------

  # tbd.language data.table?
  assertthat::assert_that(is.data.table(tdb.language), msg = "The supplied parameter must be of class data.table")


  # Validate translation/language input: Got all Cols?
  check.cols <- check.tdb.cols(tdb.language)
  assertthat::assert_that(check.cols$result,
    msg = paste0(
      "'tdb.language'does not contain expected column(s): ",
      paste(check.cols$missing.cols, collapse = ", ")
    )
  )
  #Validate SuSo source_questionnaire

  # Validate sSuSoource_questionnaire input
  assertthat::assert_that(all(names(source_questionnaire) != ""), msg = "All elements in source_questionnaire must be named.")
  assertthat::assert_that(suppressWarnings(all(lapply(source_questionnaire, is.data.table))),
    msg = "All elements in source_questionnaire must be data.table. Did you supply a list that contains multiple questionnaires?"
  )


  # Validate path input
  assertthat::assert_that(grepl(".xlsx$", path), msg = "'path' must have .xlsx file extension")
  assertthat::assert_that(dir.exists(dirname(path)), msg = paste(path, "does not exist"))


  # Sheets
  # Check if user supplied sheet is actually in the sheets.
  if (!is.null(sheets)) {
    # Get the sheets in questionnaire list
    sheets.questionnaire <- names(source_questionnaire)

    # If "Translations" was not specified add it
    if (!"Translations" %in% sheets) sheets <- c(sheets, "Translations")

    # Get which ones are not found
    sheets.not.found <- sheets[!sheets %in% sheets.questionnaire]
    assertthat::assert_that(length(sheets.not.found) == 0, msg = paste(paste(sheets.not.found, collapse = ", "), "are sheets not found in source_questionnaire"))
  }
  # If Sheet is null, take all sheets found in Questionnaire Master
  if (is.null(sheets)) sheets <- names(source_questionnaire)

  # Filter by Statuses
  language.tdb.dt <- tdb.language[Status %in% statuses]

  # If there is no language in our preferred statuses, return simply the questionnaire
  if (nrow(language.tdb.dt) == 0) {
    message("No language found that is in Status as supplied in 'statuses'. Empty ")
    message("No excel file generated")
    return()
  }

  # Print to console first status
  message(paste0(
    "Translated Text Items with Status(es): ",
    paste(paste0("'", unique(language.tdb.dt$Status), "'"), collapse = ", "),
    " are added to the Questionnaire File"
  ))


  # Add Translation by Sheet. Results in Translation Workbook list
  workbook <- purrr::map(
    .x = sheets,
    .f = ~ create_suso_sheet(
      source_questionnaire = source_questionnaire,
      sheet = .x,
      language.tdb.dt = language.tdb.dt,
      qcode_pattern = qcode_pattern
    )
  )


  workbook <- stats::setNames(workbook, c(sheets))

  # Quickly display if there are still missing 'translations'
  for (name in names(workbook)) {
    data_table <- workbook[[name]]
    na_count <- sum(is.na(data_table$Translation))
    if (na_count>0) message(sprintf("Sheet %s has %d rows of missing 'Translation'", name, na_count))
  }


  # Write the Workbook!
  writexl::write_xlsx(
    workbook,
    path = path
  )
}
