# FUNCTIONS TO WORK WITH THE TRANSLATION MASTER SHEET

#' Pull 'Translation Database' Google Sheet
#'
#' A simple wrapper for \code{\link[googlesheets4]{read_sheet}}.
#' Retrieves all data from the 'Translation Database' Google Sheet.
#'
#' @param ss Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheets Sheets in ss to read. Should be ISO language names. By default reads all sheets found in `ss`
#'
#' @importFrom googlesheets4 gs4_get read_sheet
#' @importFrom data.table as.data.table
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#'
#' @return A list object where each element is one translation sheet
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_tdb_data(ss = "GOOGLE-SHEET-IDENTIFIER")
#' }
get_tdb_data <- function(ss = "",
                         sheets = NULL) {
  #TODO: CHECK THAT WE HAVE UNIQUE ITEMS PULLED FROM DB. IF NOT SOMETHING IS GOING WRONG ON THE GOOGLE SHEET

  # Check Sheets
  sheets.ss <- googlesheets4::gs4_get(ss)$sheets$name

  # Check if user supplied sheet is actually in the sheets
  if (!is.null(sheets)) {
    assertthat::assert_that(all(sheets %in% sheets.ss),
      msg =
        paste(paste(sheets[!sheets %in% sheets.ss], sep = ","), "is not a sheet in the 'Translation Database' Google Sheet")
    )
  }

  # Define sheets. By default all sheets found in google sheet. Read all as character
  if (is.null(sheets)) sheets <- sheets.ss

  tdb.list <- purrr::map(
    .x = sheets,
    .f = ~ data.table::as.data.table(
      googlesheets4::read_sheet(ss,
        sheet = .x,
        range = "A:G",
        col_types = "ccccccc"
      )
    )
  )
  tdb.list <- setNames(tdb.list, c(sheets))

  # Check we have all column Names in all sheets with util.function check.tdb.cols
  for (i in seq_along(tdb.list)) {
    # Check
    check <- check.tdb.cols(tdb.list[[i]])
    assertthat::assert_that(check$result,
      msg = paste0(
        "Sheet ", names(tdb.list)[1], " does not contain expected column(s): ",
        paste(check$missing.cols, collapse = ", ")
      )
    )
  }


  return(tdb.list)
}



#' Prepare columns for writing to the Translation Database
#' @noRd
prepare_tdb_write_data <- function(dt) {
  assertthat::assert_that(is.data.table(dt), msg = "'dt' must be a data.table.")
  check.cols <- check.tdb.cols(dt)
  assertthat::assert_that(check.cols$result,
    msg = paste0("'dt' does not contain expected column(s): ",
      paste(check.cols$missing.cols, collapse = ", ")))
  required_cols <- c("value.unique", "Questionnaire(s)", "Type", "Text_Item", "Status", "Translation", "Comment/Note")
  extra_cols <- setdiff(names(dt), required_cols)
  assertthat::assert_that(length(extra_cols) == 0,
    msg = paste("'dt' contains unexpected column(s):", paste(extra_cols, collapse = ", ")))
  copy(dt)[, ..required_cols]
}

#' Write one data.table to 'Translation Database' Google Sheet
#'
#' A simple wrapper for [googlesheets4::range_write()]. Writes a data.table to the specified
#' Google Sheet and sheet name.
#'
#' @param dt A data.table to write. Usually an element within the list of translations
#'           returned by `update_tdb()` that contains the translation of one language.
#'
#' @param ss Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#'
#' @param sheet The name of the sheet in `ss`. The ISO 639 language name, which
#'              is the name of the element in the list of translations.
#' @param range Cell Range to write
#' @param col_names A logical value indicating whether to send the column names of the data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Write one particular (updated) language to the 'Translation Database' Google Sheet
#' write_tdb_data(new_tdb[["German"]],
#'                ss = "GOOGLE-SHEET-IDENTIFIER",
#'                sheet = "German"
#' )
#'
#' #Or write all languages found in 'Translation Database' object to the Google Sheet
#' purrr::walk(
#'   .x = names(new_tdb),
#'   .f = ~ write_tdb_data(new_tdb[[.x]],
#'                         ss = "GOOGLE-SHEET-IDENTIFIER",
#'                         sheet = .x
#'   )
#' )
#' }
write_tdb_data <- function(dt,
                           ss = "",
                           sheet = "",
                           range = "A2:G",
                           col_names = FALSE) {

  # Check input and align values with the fixed Google Sheet column order.
  dt <- prepare_tdb_write_data(dt)
  assertthat::assert_that(is.character(ss) && nchar(ss) > 0, msg = "'ss' must be a non-empty character string.")
  assertthat::assert_that(is.character(sheet) && nchar(sheet) > 0, msg = "'sheet' must be a non-empty character string.")
  assertthat::assert_that(is.character(range) && nchar(range) > 0, msg = "'range' must be a non-empty character string.")
  assertthat::assert_that(is.logical(col_names) && length(col_names) == 1, msg = "'col_names' must be a single logical value.")

  googlesheets4::range_write(
    ss = ss,
    data = dt,
    sheet = sheet,
    range = range,
    col_names = col_names,
    reformat = FALSE
  )
}
