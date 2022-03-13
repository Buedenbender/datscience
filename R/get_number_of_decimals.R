########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for

#' Determine Number of Decimals by a Simple Convention (Based on N)
#'
#' @description
#' A simple \href{https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics}{convention}
#' for the determination of the appropriate number of decimals
#' will be applied, e.g.,  \emph{n}
#' \itemize{
#' \item < 100: no decimal
#' \item < 1000: 1 decimals
#' \item > 1001: 2 decimals
#' }
#'
#' @param n Number of observations (e.g., participants in a study)
#'
#' @return Integer the appropriate number of decimals, based on sample size (n)
#'
#' @author Bjoern Buedenbender
#'
#' @examples
#' get_number_of_decimals(n = 153)
#' @export
get_number_of_decimals <- function(n) {
  if (n < 100) {
    nod <- 0
  } else if (n < 1000) {
    nod <- 1
  } else {
    nod <- 2
  }
  return(nod)
}

