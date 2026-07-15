#' Update translation for one element in list
#'
#' @inheritParams  update_tdb
#' @noRd
update_tdb_lelement <- function(tdb,
                                source_titems,
                                keep_statuses = c("outdated", "don't translate")) {
  # "outdated" drives the reappeared/no-longer-in-script logic below, so it is always kept
  keep.statuses <- normalize_status(unique(c("outdated", keep_statuses)))
  # First Scenarion: Text Items exist in current Translation sheet and is found again in Master Questionnaire
  dt1 <- tdb[as.character(value.unique) %chin% source_titems$value.unique]
  #If a text item reappears, change status to "to translate" & Leave a note
  comment <- "Item reappeared in CAPI script."
  dt1[Status=="outdated" & is.na(`Comment/Note`),
     `Comment/Note`:=fcase(!is.na(`Comment/Note`),paste0(`Comment/Note`,"\n",comment),
                           is.na(`Comment/Note`),comment)]
  dt1[Status=="outdated",Status:="to be checked"]

  # Second Scenario: New items in Source Questionnaire not found yet in Translation Sheet
  dt2 <- source_titems[
    !value.unique %chin% as.character(tdb$value.unique),
    .(value.unique,
      `Questionnaire(s)` = questionnaire,
      Type = type,
      Text_Item = value,
      Status="to translate"
    )
  ]

  #Third Scenario: Items in Translation are no longer in CAPI Form => Keep but place to new Status
  #TODO: Write results of update?
  dt3 <- tdb[!as.character(value.unique) %chin% source_titems$value.unique]
  dt3[,Status:="outdated"]

  # Bind to one
  dt <- rbindlist(list(
    dt1, dt2, dt3
  ), fill = TRUE)

  # Set Status to "to translate" if NA.
  # Statuses in `keep_statuses` legitimately have no Translation and must survive a re-run,
  # otherwise e.g. "don't translate" would be reset on every update.
  dt[is.na(Translation) & !normalize_status(Status) %chin% keep.statuses,
     Status := "to translate"]

  # Get in current sequential order, using the Master Questionnaire as reference
  dt <- merge(dt, source_titems[, .(value.unique, seq.id)], by = "value.unique", all.x = T)
  setorder(dt, seq.id,na.last = T)
  dt[, "seq.id" := NULL]

  return(dt)
}


#' Compares 'Source Questionnaire' data against 'Translation Database'
#'
#' Removes any text item in the translation database object that no longer is part of the source questionnaire(s).
#' Adds any new text item from source questionnaire(s) not yet found in the database
#'
#' @param tdb List of translation database as returned by [get_tdb_data()]
#' @param source_titems Data table of questionnaire text items returned by either [parse_odk_titems()] or [parse_suso_titems()]
#' @param keep_statuses Character vector. Statuses for which an empty 'Translation' is legitimate
#'   and which therefore must not be reset to `"to translate"` on a re-run. Matched
#'   case-insensitively and ignoring the apostrophe variant. `"outdated"` is always kept.
#'
#' @return List of updated translation database
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_tdb <- update_tdb(
#' tdb = tdb_data,
#' source_titems = source_titems
#' )
#' }


update_tdb <- function(tdb = list(),
                               source_titems = data.table(),
                               keep_statuses = c("outdated", "don't translate")) {
  assertthat::assert_that(is.list(tdb), msg = "'tdb' must be a list.")
  assertthat::assert_that(is.data.table(source_titems), msg = "'source_titems' must be a data.table.")
  assertthat::assert_that(is.character(keep_statuses), msg = "'keep_statuses' must be a character vector.")


  # Check if new.items data.table has required columns
  required_columns <- c("value.unique", "seq.id", "questionnaire", "value", "type")
  assertthat::assert_that(all(required_columns %in% names(source_titems)),
    msg = paste(
      "The 'source_titems' data.table is missing required columns:",
      paste(required_columns[!required_columns %in% names(source_titems)], collapse = ", ")
    )
  )


  # Identify languages in current list of translations
  languages <- names(tdb)

  # Go through all sheets of current translation and compare against master
  updated.trans.sheets <- purrr::map(
    .x = languages,
    .f = ~ update_tdb_lelement(
      tdb = tdb[[.x]],
      source_titems = source_titems,
      keep_statuses = keep_statuses
    )
  )
  updated.trans.sheets <- stats::setNames(updated.trans.sheets, c(languages))

  return(updated.trans.sheets)
}
