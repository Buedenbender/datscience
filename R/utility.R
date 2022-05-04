# Source: https://www.r-bloggers.com/2019/07/clean-consistent-column-names/
#' Pseudo Documentation see if we can get this running
#' @param .data vector of oclnames
#' @param unique Boolean if names should be unique
#' @param repl_umlaut Default is TRUE. If provided True it replaces umlauts (vowel mutations)
#' like ä, ö, ü and ß with respective ae, oe, ue, and ss
#' @noRd
clean_names <- function(.data, unique = FALSE, repl_umlaut = TRUE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  if (repl_umlaut) {
    n <- gsub("\u00E4", "ae", n)
    n <- gsub("\u00FC", "ue", n)
    n <- gsub("\u00F6", "oe", n)
    n <- gsub("\u00DF", "ss", n)
    # TODO: Find a Regex / Function that removes accents from letters e.g. é
    # Until then control for most common accent letter combinations
    n <- gsub("\u00E9", "e", n)
    n <- gsub("\u00E8", "e", n)
    n <- gsub("\u00E0", "a", n)
    n <- gsub("\u00E1", "a", n)
    n <- gsub("\u00EE", "i", n)
    n <- gsub("\u00F4", "o", n)
  }
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))

  n <- gsub("(^_+|_+$)", "", n)

  n <- gsub("_+", "_", n)

  if (unique) n <- make.unique(n, sep = "_")

  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

#' @title Reverses the Order of a Factor - Utility Function
#' @description Reverses the order of the levels of a factor.
#' @param f A factor
#' @return Factor f with reversed order of the levels
#' @details Replacement for forcats::fct_rev(), in order to reduce the number
#' of dependencies to datscience package
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   f <- factor(letters[c(1:5)])
#'   (reversed_factor <- util_rev_fac(f))
#' }
#' }
#' @rdname util_rev_fac
#' @author Björn Büdenbender
#' @export
util_rev_fac <- function(f) {
  l <- levels(f)
  factor(f, levels = l[c(length(l):1)])
}
