########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' Booted Eigenvalues
#' @description
#' Uses \code{\link[boot]{boot}}  to bootstrap Eigenvalues in factor analyses. \cr
#' Eigenvalues can be extracted for PCA as well as EFA \cr
#' Different Factor Methods are available for more details, see \code{\link[psych]{fa}}
#' Allows for extraction of Eigenvalues based on Pearson
#' as well as polychoric correlation matrices. These bootstrapped Eigenvalues can
#' serve as a foundation to construct a scree plot with confidence intervals around
#' the observed eigenvalues.
#'
#' @param df A data.frame
#' @param iterations number of resamples for the bootstrap
#' @param cor either "pearson" or "poly" for polychoric correlations,
#'  defaults to "pearson"
#' @param fa either "pc" for prinicipal component or
#' "fa" for [common] factor analysis, defaults to "pc"
#' @param fm factor method to use, irrelevant for pca.
#' For available factor methods check psych::fa for more details, defaults to minres
#'
#' @return A boot object (\code{boot::boot()}),
#' that contains SE for all Eigenvalues in DF, can be passed to \code{\link{getCIs}}  to create Confidence Intervals
#'
#' @author Bjoern Buedenbender
#'
#' @export
#' @importFrom boot boot
#' @importFrom psych fa polychoric
#' @seealso \code{\link[boot]{boot}}, \code{\link[psych]{fa}}, \cr
#' \code{\link{getCIs}} \code{\link{add_ci_2plot}}
booted_eigenvalues <-
  function(df,
           iterations = 1000,
           cor = "pearson",
           fa = "pc",
           fm = "minres") {
    eigenvalues_extractor <- function(d, i, cor, fa, fm) {
      d2 <- d[i, ]
      if (cor == "pearson") {
        rx <- cor(d2, use = "pairwise")
        nobs <- NA
      } else if (cor == "poly") {
        poly_cor <- psych::polychoric(d2, correct = FALSE)
        rx <- poly_cor$rho
        nobs <- poly_cor$n.obs
      }

      if (fa == "pc") {
        res <- eigen(rx)$values
      } else if (fa == "fa") {
        res <-
          psych::fa(
            rx,
            nfactors = 1,
            rotate = "none",
            fm = fm,
            warnings = FALSE,
            n.obs = nobs
          )$values
      }
      return(res)
    }

    boot_obj <- boot::boot(
      d = df,
      # Helper Function to Extract Eigenvalues
      eigenvalues_extractor,
      # Number of Resamples
      R = iterations,
      # Parameters for the Eigenvalue Extraction
      fa = fa,
      fm = fm,
      cor = cor
    )
    return(boot_obj)
  }
