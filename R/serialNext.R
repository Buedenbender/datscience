########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. .)
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' Check if file exists, else append a integer number
#'
#' @description
#' In cases you want to write to a file which you do not want to override if existing.
#' This helper function checks if a file exists, if it does it just appends an integer to the filename
#' Source originally by user spacedman \href{https://stackoverflow.com/a/25429755/7318488}{on stackoverflow}.
#' @param path Character containing the path with fileextensions
#' @param n_digits  (Optional) Integer, number of leadings 0 before the filename. Default is n_digits = 3
#' @param maxruns (Optional) Integer, Default is maxruns = 500.
#' @param ... (Optional) Additional Parameters, not utiliezd in this function, enables passing from passing of previous functions
#' @return Character, a filename that currently does not exists, with indices.
#'
#' @author spacedman stackoverflow (advanced by Bjoern Buedenbender)
#'
#' @importFrom xfun file_ext
#' @importFrom stringr str_replace str_pad
#' @importFrom tools file_path_sans_ext
#' @export
serialNext <- function(path,
                       n_digits = 3,
                       maxruns = 500,
                       ...) {

  # Validate correct inputs
  if (missing(path)) stop("Need to specify the mandatory argument \"path\"")
  if (!is(path, "character")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"path\" is required to be a character"
    ))
  }
  if (!is(n_digits, "numeric")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"n_digits\" is required to be a numeric"
    ))
  }
  if (!is(maxruns, "numeric")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"maxruns\" is required to be a numeric"
    ))
  }



  # Currently only supports ignore file extension case
  # Check if file exists, if not just return filename
  if (!file.exists(path)) {
    return(path)
  }

  # Extract file extensions
  extension <- xfun::file_ext(path)
  # Extract filename
  filename <- tools::file_path_sans_ext(path)
  # Initialize Counter
  i <- 1

  # Append 3 digit integer number with leading 0
  repeat {
    f <- paste0(paste(filename,
      stringr::str_pad(i, n_digits, pad = "0"),
      sep = "_"
    ), ".", extension)
    if (!file.exists(f)) {
      return(f)
    } else if (i > maxruns) {
      print(paste0("All file names up until: ", basename(f), " are already taken."))
      print("Make sure this is intended. To solve this, you can either:")
      print(paste(" - increase maxruns arguments above:", maxruns))
      print(paste(" - Change (e.g., increase) the n_digits above:", n_digits))

      break
    }
    i <- i + 1
  }
}
