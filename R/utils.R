#' CLEANUP TEXT ITEM - REMOVE ONLY PROPER WHITESPACE TO NOT REMOVE TABS AND NEWLINES
#' @importFrom stringr str_to_lower str_remove_all
#' @noRd
create.unique.var <- function(dt, regex = " ",col="value") {
  dt[, value.unique := stringr::str_to_lower(stringr::str_remove_all(get(col), regex))]
}


#' Normalise a 'Status' value for comparison
#'
#' Statuses are typed/picked by Translators in the Google Sheet, so casing, padding and the
#' apostrophe variant (Google Sheets may produce a typographic one) are not reliable.
#' Compare normalised values rather than raw ones. Returns NA for NA.
#' @noRd
normalize_status <- function(x) {
  x <- stringr::str_replace_all(as.character(x), "[‘’ʼ´`]", "'")
  stringr::str_to_lower(stringr::str_trim(x))
}



#' Check if Translation Database Object has all required columns
#' Returns boolean
#' @noRd
check.tdb.cols <- function(dt) {

  #Check all Sheets have expected column names
  required_cols <- c("value.unique", "Questionnaire(s)", "Type", "Text_Item", "Status", "Translation", "Comment/Note")

  names.dt <- names(dt)
  check <- list()
  check$result <- all(required_cols %in% names.dt)
  check$missing.cols <- required_cols[!required_cols %in% names.dt]
  return(check)
}


#' Check if Survey Solutions Questionnaire Template Object has the exact required columns
#' Returns boolean
#' @noRd
check_susotemplate_cols <- function(dt) {
  # Define the exact required columns
  required_cols <- c("Entity Id", "Variable", "Type", "Index", "Original text", "Translation")

  # Get the names of the columns in the data table
  names_dt <- names(dt)

  # Check if the data table has exactly the required columns
  result <- identical(sort(names_dt), sort(required_cols))

  # Create a list to store the result and any discrepancies
  check <- list()
  check$result <- result
  check$extra_cols <- setdiff(names_dt, required_cols)
  check$missing_cols <- setdiff(required_cols, names_dt)

  return(check)
}




#' Check if named vector of character type
#'
#' @importFrom stringr str_to_lower str_remove_all
#' @noRd
is.char.named.vector <- function(vec) {
  check <- is.vector(vec) & is.character(vec) & !is.null(names(vec)) &
    !any(is.na(names(vec))) & !any(names(vec) %in% "")

  return(check)
}


#' COLLAPSE dt OF TRANSLATION ITEMS INTO UNIQUE SET OF TRANSLATION ITEMS
#'
#' @param dt
#'
#' @return data table of unique items
#' @noRd
collapse_titems <- function(dt) {

  # COLLAPSE AND REMOVE IDENTIFIER
  # TODO: ASSERT THAT DT SUPPLIED FOLLOWS STANDARD
  # TODO: CODE CAN BE SIMPLIFIED/BEAUTIFIED


  #ADD SEQUENTIAL IDENTIFIER IF NOT EXISTENT. MAINLY IF USED FOR remove_coding
  if (!"seq.id" %in% names(dt)) dt[, seq.id := 1:.N]

  #Store nrow of initial dt so we can display result in end
  init.row <- nrow(dt)

  # KEEP UNIQUE:
  # ALWAYS ACCOUNT FOR SEQUENTIAL ID
  # FIRST BY QUESTIONNAIRE; VARIABLE AND VALUE TO AVOID HAVING MULTIPLE "label, label, label"
  dt <- dt[
    , .(
      seq.id = seq.id[c(1)],
      value = value[c(1)]
    ),
    by = .(questionnaire, type, value.unique)
  ]

  # NOW BY TYPE AND VALUE
  dt <- dt[
    , .(
      seq.id = seq.id[c(1)],
      value = value[c(1)],
      questionnaire = paste(questionnaire, collapse = "\n")
    ),
    by = .(type, value.unique)
  ]

  # NOW BY UNIQUE VALUE, WITH TYPE OF VARIABLE COLLAPSED
  setorder(dt, value.unique, seq.id)
  dt <- dt[, .(
    seq.id = seq.id[c(1)],
    questionnaire = questionnaire[c(1)],
    value = value[c(1)],
    type = paste(type, collapse = "\n")
  ), by = .(value.unique)]

  #SET ORDER OF APPEARANCE AND REMOVE IDENTIFIER
  setorder(dt, seq.id)

  #Some results
  message(paste("Collapse:",init.row-nrow(dt), "rows removed.",
                sprintf("%.1f%%", (1-nrow(dt)/init.row)*100),"of init dataset."))



  return(dt)

}





#' Remove the question coding of a text item within a data.table
#'
#' Identifies and removes supplied string pattern in dt to get rid of Question Coding.
#' Can be used to reduce workload for Translators as one does not need to include Question Coding in Translation (which reduces likelihood of typos/mistakes) and
#' can further collapse data table by removing potential duplicate text items for which only question coding differs
#'
#' @param dt  Data table of questionnaire text items returned by either [parse_odk_titems()] or [parse_suso_titems()]
#' @param pattern Regular expression that matches question coding
#' @param collapse boolean to indicate if after removing question coding, the set of translation items should be scanned for/collapsed to unique items
#'
#' @return dt
#' @export
#'
remove_coding <- function(dt,
                          pattern=stop("'pattern' must be specified")
) {
  #TODO: Check Input

  #Get copy of dt, as in place changes are made. If no object assigned with funciton might cause issues
  dt <- copy(dt)

  #Display number of text items
  message(paste(nrow(dt[grepl(pattern,value)]),"items identified for which Question Code will be removed"))
  #Remove
  dt[grepl(pattern,value),
     `:=` (
       #Value unique: Remove pattern but based on Original Text Item as it
       #could contain whitespace
       value.unique=stringr::str_remove(value.unique,
                                        stringr::str_to_lower(stringr::str_remove_all(
                                          stringr::str_extract(value,pattern),
                                          " "))),
       #Value, simply remove user pattern
       value=stringr::str_remove(value,pattern))
  ]


  return(dt)

}

