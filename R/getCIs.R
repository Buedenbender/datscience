########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' Get Boot Strapped CIs
#'
#' @description
#' Determines the CI for a vector of statistics / multiple stats.
#' As \code{\link[boot]{boot.ci}} only returns CI for the first statistic in a boot_object
#' getCIs (given a \code{\link[boot]{boot}} object), creates CI for all statistics.
#' Code by Ben Bolker, Check the
#' original Source: \href{https://stackoverflow.com/a/31818160/7318488}{@@stackoverflow}
#'
#' @param boot_obj boot::boot object
#'
#' @return A data.frame with Confidence intervalls for
#' all statistics, as well as the observed value
#'
#' @author Ben Bolker
#'
#' @export
#' @importFrom boot boot.ci
#' @importFrom utils tail
#'
#' @seealso \code{\link[boot]{boot.ci}}, \code{\link[boot]{boot}}

getCIs <- function(boot_obj) {
  getCI <- function(x, w) {
    b1 <- boot::boot.ci(x, index = w)
    ## extract info for all CI types
    tab <- t(sapply(b1[-(1:3)], function(x) {
      tail(c(x), 2)
    }))
    ## combine with metadata: CI method, index
    tab <- cbind(w, rownames(tab), as.data.frame(tab), x$t0[w])
    colnames(tab) <- c("index", "method", "lwr", "upr", "observed")
    tab
  }
  return(do.call(rbind, lapply(c(1:ncol(boot_obj$t)), getCI, x = boot_obj)))
}
