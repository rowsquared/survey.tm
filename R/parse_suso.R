#' Get unique Text Items of one questionnaire
#' @inheritParams parse_suso_titems
#' @param questionnaire_list that contains sheets of one questionnaire
#' @import data.table
#' @noRd
parse_suso_titems.by.qx <- function(
    questionnaire_list,
    questionnaire = stop("questionnaire needs to be defined"),
    sheets = NULL,
    types = c(
      "Title", "Instruction", "OptionTitle", "ValidationMessage",
      "SpecialValue", "FixedRosterTitle", "CriticalRuleMessage"
    )) {

  # Keep only subset of sheets
  if (!is.null(sheets)) {
    sheets.list <- sheets[sheets %in% names(questionnaire_list)]
    questionnaire_list <- questionnaire_list[c(sheets.list)]
  }
  # READ ALL SHEETS INTO ONE DT- ONLY COLS OF INTEREST OF TYPE OF INTEREST
  dt <- rbindlist(
    lapply(questionnaire_list, function(sheet) {
      sheet[
        Type %chin% types | (is.na(Type) & "OptionTitle" %in% types),
        .(
          type = Type,
          value = `Original text`
        )
      ]
    })
  )

  #Set NA Type to "OptionTitle as it is read from "@@_sheets" (Unless "OptionTitle" is not in types specified)
  if ("OptionTitle" %in% types) dt[is.na(type),type:="OptionTitle"]

  # GET ROW IDENTIFIER - USED LATER IN TMS IN CASE NEW ITEMS ARE ADDED IN MIDDLE OF QX
  dt[, seq.id := 1:.N]

  # Assign questionnaire identifier
  dt[, questionnaire := questionnaire]

  return(dt)
}


#' Parse 'Source Questionnaires' object into unique text items
#'
#' This function processes a nested listed returned by \code{\link{get_suso_tfiles}}.
#' It identifies all text items across all sheets across all questionnaires found in `tmpl_list`. If `collapse=TRUE` only unique text items will be returned.
#'
#' @param tmpl_list A named nested list as returned by \code{\link{get_suso_tfiles}} (Source Questionnaire object)
#' @param sheets A character vector of sheet names within `tmpl_list` files to be parsed. By default, all sheets are parsed.
#' @param types A character vector specifying the types of text items to keep.
#'   By default, includes `CriticalRuleMessage`. Supply a subset to exclude types.
#' @param collapse Boolean. If TRUE, only unique text items will be returned.
#' @param qcode_pattern Optional. Regex pattern to remove question codes, e.g., "Q1." in "Q1. How old are you?". Default is NULL, keeping texts unmodified.
#' @import data.table
#' @importFrom readxl excel_sheets
#' @importFrom purrr map_df
#' @import data.table
#' @return A data.table containing (unique) text items found across questionnaires in `tmpl_list`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' source_titems <- parse_suso_titems(
#' tmpl_list = suso_trans_templates, #Nested list as returned by `get_suso_tfiles()`
#' collapse = TRUE # Keep only unique text items across questionnaires
#' )
#' }
parse_suso_titems <- function(tmpl_list,
                                sheets = NULL,
                                types = c(
                                  "Title", "Instruction", "OptionTitle", "ValidationMessage",
                                  "SpecialValue", "FixedRosterTitle", "CriticalRuleMessage"
                                ),
                              collapse = TRUE,
                              qcode_pattern=NULL
                              ) {
  # Check input
  # Allow callers to provide arbitrary subsets of Survey Solutions Type values.
  # match.arg() would silently reduce unknown values to the default choices.
  assertthat::assert_that(
    is.character(types),
    msg = "'types' must be a character vector."
  )


  # Translation
  assertthat::assert_that(
    all(!is.null(names(tmpl_list))),
    msg = "'tmpl_list' is not a named list. Use Questionnaire Name for each element"
  )

  # Check  Sheets
  if (!is.null(sheets)) {
    # Check if sheets is a character vector
    assertthat::assert_that(
      is.character(sheets),
      msg = "'sheets' must be a character vector."
    )

    # Check if user-supplied sheet is actually in the sheets, if not provide a warning
    # Get the sheets in tmpl_list list
    sheets.list <- unlist(lapply(tmpl_list, names))
    sheets.not.found <- sheets[!sheets %in% sheets.list]
    # Get which ones are not found
    if (length(sheets.not.found) > 0) {
      warning(paste(
        paste(sheets.not.found, collapse = ", "),
        "are sheet(s) not found in all Translation Files"
      ))
    }

    # #If "Translation" not within sheets, add it
    # if (!"Translations" %in% sheets) sheets <- c(sheets,"Translations")
  }

  #Question Code Pattern
  if (!is.null(qcode_pattern)) {
    # Check if it's a character string
    if (!is.character(qcode_pattern) || length(qcode_pattern) != 1) {
      stop("qcode_pattern should be a single character string.")
    }

    # Test if it's a valid regex
    tryCatch({
      grepl(qcode_pattern, "")
    }, error = function(e) {
      stop("Invalid regular expression provided in qcode_pattern.")
    })
  }


  # Go through all questionnaires in tmpl_list list and bind in one
  dt <- rbindlist(purrr::map(
    .x = names(tmpl_list),
    .f = ~ parse_suso_titems.by.qx(
      questionnaire_list = tmpl_list[[.x]],
      questionnaire = .x,
      sheets = sheets,
      types = types
    )
  ))

  # Convenience, convert to data.table
  dt <- as.data.table(dt)

  # Cleanup Text item to catch more duplicate text items
  create.unique.var(dt)
  #Remove '\r\n' if at end of string as it would not be added to Google Sheets
  create.unique.var(dt, regex="((\\\r\\\n)+)$",col="value.unique")

  #Remove Coding
  if (!is.null(qcode_pattern)) dt <- remove_coding(dt,
                      pattern=qcode_pattern)

  # Collapse if specified
  if (collapse) dt <- collapse_titems(dt)

  return(dt)
}
