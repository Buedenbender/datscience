#' parse_oneNumber
#' @param s one string to be parsed to a number
#' @param first Bool default TRUE returns only the first decimal number found
#' @noRd
parse_oneNumber <- function(s, first = TRUE) {
  # Check given argument, else return NA
  if (length(s) == 0) {
    return(NA)
  }
  if (is.na(s)) {
    return(NA)
  }

  # remove all non number related characters
  s <- gsub("[^0-9.-]", "", s)
  # if first character is a decimal point . prepend leading 0
  if (substr(s, 1, 1) == ".") s <- paste0("0", s)
  # Only use digits after the -
  s <- gsub(".*-", "-", s)
  # extract multiple strings
  s <- unlist(regmatches(s, gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*", s, perl = TRUE)))
  # convert
  s <- as.numeric(s)
  if (first) {
    return(s[1])
  } else {
    return(s)
  }

  # ARCHIVE
  # # Only use the first decimal point (the digits before and after)
  # s <- unlist(strsplit(s,"\\."))
  # Only use digits after the -
  # s <- gsub(".*-","-",s)
  # # Remove now empty strings due to multiple .
  # s <- s[s!=""]
  # # If decimal place was present concat to one string
  # if (length(s)>1) s <- paste0(s[1],".",s[2])
}

#' number_parse
#' @description Takes one string or a vector of strings and converts them to numbers
#' Does retain the original dimensionality by rendering non convertable strings
#' to NA
#' @param s character or vector of characters, to be parsed to a number
#' @export
number_parse <- function(s) {
  # Check if multiple strings were given
  if (length(s) > 1) {
    # if more than one string is given, iterate over strings
    out <- c()
    for (w in s) {
      tmp <- parse_oneNumber(w)
      # if empty is returned (no number parseable, append NA to retain dimensions)
      if (length(tmp) != 0) out <- c(out, tmp) else out <- c(out, NA)
    }
  } else {
    tmp <- parse_oneNumber(s)
    if (length(tmp) != 0) out <- tmp else out <- NA
  }
  return(out)
}
