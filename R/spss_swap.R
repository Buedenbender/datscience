########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")



#' Swaps labels and col names from a .sav (SPSS) matrix
#' @description In many situations it can be useful to have the ability to replace the
#' uninformative variable names (colnames) from a SPSS matrix (e.g. SD02, SD08_02)
#' with their respective label attribute (e.g., "Gender/Sex" or "Age 15-25"). This
#' function utilizes the utility function clean_names to convert the
#' labels from SPSS to proper variable names (e.g.,"gender_sex" or "age_15_25") and sets
#' them as new column names
#' @param df tibble read in the \code{\link[haven]{read_sav}} from haven
#' @param repl_umlaut Default is TRUE. If provided True it replaces umlauts (vowel mutations)
#' like ä, ö, ü and ß with respective ae, oe, ue, and ss
#' @param old_itemnames Options are "prepend", "append" or, "remove". Default is "prepend"
#' With "prepend" the new item names will be prepended by the old item names. E.g., the old item name
#' was "q0003" and the description "Gender" the new item name will be "q0003_gender"
#' @return A tibble with more human readable names (old labels as names)
#'
#' @author Friedrich-Samuel Taubitz & Bjoern Buedenbender
#' @examples
#' \dontrun{
#' datscience::spss_swap(df_sav)
#' }
#' @export
#'
#' @importFrom labelled var_label
spss_swap <- function(df, repl_umlaut = TRUE, old_itemnames = c("remove", "prepend", "append")) {
  # TODO Write a mini Vignette for spss_swap
  # Validate user input
  if (missing(df)) stop("Need to specify the mandatory argument \"df\"")

  if (!is(df, "tbl_df")) {
    warning(paste(
      "Invalid argument type. The argument",
      "\"df\" is required to be an spss matrix read with haven::read_sav()"
    ))
  }
  # Backup old user input
  old_names <- colnames(df)
  new_names <- c()
  i <- 1
  # Iterate over each column
  for (i in 1:ncol(df)) {
    label <- labelled::var_label(df[, i])
    # Check if  label was supplied
    if (!is.null(unlist(label))) {
      # Default will be overwritten when old_itemnames is supplied
      clean_label <- clean_names(label, repl_umlaut = repl_umlaut)
      # Shall old itemnames be prepend o. appended?
      if (!missing(old_itemnames) & length(old_itemnames == 1)) {
        # Prepend old Item Names
        if (tolower(old_itemnames) == "prepend") {
          clean_label <- paste(names((df[, i])), # old name
            clean_names(label, repl_umlaut = repl_umlaut), # cleaned desc
            sep = "_"
          )
        }
        if (tolower(old_itemnames) == "append") {
          clean_label <- paste(clean_names(label, repl_umlaut = repl_umlaut), # cleaned desc
            names((df[, i])), # old name
            sep = "_"
          )
        }
      }
    } else {
      clean_label <- names((df[, i]))
    }
    # Append the new name to the character vector
    new_names <- c(new_names, clean_label)
  }
  # Replace the new names
  colnames(df) <- new_names
  # Replace the labels with the old col names
  labelled::var_label(df) <- old_names
  return(df)
}
