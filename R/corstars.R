########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")
utils::globalVariables("Hmisc")

#' Corstars - Correaltions in Console
#' @description  Creates a pretty console correlation table (by Dominik Vogel)
#' method : correlation method. "pearson", "spearman" and "polychoric" are currently
#' supported the results will be displayed directly in console. There is the option
#' to save them in html or latex format or (recommended), to transform them to a
#' \code{\link[flextable]{flextable}} and export it directly to word.
#' labels_rows and labels_cols are character vectors for labeling rows and columns.
#' \href{https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R}{Reference for the original code}.
#' Additionally added the option to investigate polychoric correlation
#' For clarity to be able to calculate pearson or spearman correlation an installation
#' of the Hmisc package is required. Run install.packages("Hmisc")
#' @param x a matrix containing the data
#' @param method correlation method. "pearson", "spearman" or "polychoric" are supported
#' @param removeTriangle remove upper or lower triangle, or FALSE for not removing any triangle
#' @param rmDiag if one triangle of the matrix is removed, should the diagonal be kept = FALSE; or removed = TRUE
#' @param rmLastCol chose if the last column can be removed, to shorten the table if necessary, default = TRUE
#' @param result Print result in Console ("none"), generate HTML file ("html"), generate latex file ("latex")
#' @param labels_rows Labels for the rows (i.e., variable names). Length musst be same as number of variables
#' @param labels_cols Labels for columns. Length musst be same as number of variables - 1
#' @param sig.level Significance level (.1 or .05). If NA is provided, no stars marking the significance will be printed.
#' This helps formatting the decimal places. NA is especially used by the \code{\link{apa_corrTable}} function
#' @param nod Integer. Number of Decimals. Default is nod = 2. In case of -1 a simple convention based
#' on sample size is applied for determination of number of decimal points.
#' See \code{\link{get_number_of_decimals}} or \code{?datscience::get_number_of_decimals}
#'
#' @seealso \code{\link{get_number_of_decimals}}
#'
#' @return Correlation table in console or file
#'
#' @author Dominik Vogel (Adapted by Bjoern Buedenbender)
#'
#'
#' @examples
#' \dontrun{
#' # Console output
#' corstars(mtcars,
#'   method = "pearson", removeTriangle = "upper", result = "none",
#'   caption = "Correlations",
#'   sig.level = 0.1,
#'   labels_rows = c(
#'     "(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'     "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'     "(9) am", "(10) gear",
#'     "(11) carb"
#'   ),
#'   labels_cols = 1:10
#' )
#'
#' # HTML output
#' corstars(mtcars,
#'   method = "pearson", removeTriangle = "upper", result = "html",
#'   caption = "Correlations", filename = "corr.html",
#'   sig.level = 0.1,
#'   labels_rows = c(
#'     "(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'     "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'     "(9) am", "(10) gear",
#'     "(11) carb"
#'   ),
#'   labels_cols = 1:10
#' )
#' }
#' @export
corstars <- function(x,
                     method = c("pearson", "spearman", "polychoric"),
                     removeTriangle = c("upper", "lower", FALSE),
                     rmDiag = c(TRUE, FALSE),
                     rmLastCol = c(TRUE, FALSE),
                     result = c("none", "html", "latex"),
                     labels_rows = colnames(x),
                     labels_cols = labels_rows[1:length(labels_rows)],
                     sig.level = 0.05,
                     nod = 2
                     ) {
  ### TODO: Add overwrite parameters
  ### TODO: Remove require Name Space Quitely
  ### TODO: Add a Check, if a Hmisc is installed else give a warning
  ### TODO: Add Check of User Input

  if (!pacman::p_isinstalled(Hmisc)) warning("Package Hmisc is not installed, corstars will not work for method = pearson or spearman.", .callr = FALSE)

  # Determine number of decimals, if convention is desired (-1)
  if (nod == -1) nod <- get_number_of_decimals(nrow(x))
  # stopifnot(length(labels_rows) == ncol(x))
  # stopifnot(length(labels_cols) == ncol(x))

  # Prepare data.frame
  x <- as.matrix(x)

  # Prevent warning for no method provided, from vector as input to method
  if (length(method) > 1) {
    method <- method[1]
    # Set sig.level to NA in case of polychoric
    if (method == "polychoric") {
      sig.level <- NA
    }
  }

  # Compute correlation matrix
  if (method != "polychoric") {
    correlation_matrix <- Hmisc::rcorr(x, type = method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value

    ## Define notions for significance levels; spacing is important.
    # ifelse(sig.level == 0.1,
    #        mystars <- ifelse(p < .01,"**", ifelse(p < .05, "* ", "  ")),
    #        ifelse(sig.level == 0.05,
    #               mystars <- ifelse(p < .05, "*", " "),""))
    if (!is.na(sig.level)) {
      if (sig.level == .001) {
        mystars <-
          ifelse(p < .001, "*", " ")
      } else if (sig.level == .01) {
        mystars <-
          ifelse(p < .001, "**",
                 ifelse(p < .01, "* ", "  ")
          )
      } else {
        mystars <-
          ifelse(p < .001, "*** ",
                 ifelse(p < .01, "**  ",
                        ifelse(p < .05, "*   ", "    ")
                 )
          )
      }
    }
  } else if (method == "polychoric") {
    correlation_matrix <- psych::polychoric(x, global = FALSE, correct = 0)
    R <- correlation_matrix$rho
  } else {
    stop("Please provide a correct method for correlation analysis, chose between either:
        - method = \"pearson\",
        - method = \"spearman\",
        - method = \"polychoric\"")
  }

  ## trunctuate the correlation matrix to two decimal
  if (!is.na(sig.level)) {
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), nod))[, -1]
  }


  ## build a new matrix that includes the correlations with their appropriate stars
  if (method != "polychoric" && !is.na(sig.level)) {
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep = "")
  } else {
    Rnew <- round(R, nod)
  }
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")
  ## remove upper triangle of correlation matrix
  if (removeTriangle[1] == "upper") {
    Rnew <- as.matrix(Rnew)
    if (!is.na(sig.level)) {
      Rnew[upper.tri(Rnew, diag = rmDiag[1])] <- ""
    } else {
      Rnew[upper.tri(Rnew, diag = rmDiag[1])] <- NA
    }
    Rnew <- as.data.frame(Rnew)
    ## remove lower triangle of correlation matrix
  } else if (removeTriangle[1] == "lower") {
    Rnew <- as.matrix(Rnew)
    if (!is.na(sig.level)) {
      Rnew[lower.tri(Rnew, diag = rmDiag[1])] <- ""
    } else {
      Rnew[lower.tri(Rnew, diag = rmDiag[1])] <- NA
    }
    Rnew <- as.data.frame(Rnew)
  } else {
    Rnew <- as.data.frame(as.matrix(Rnew))
  }

  ## remove last column and return the correlation matrix
  if (rmLastCol[1]) {
    Rnew <- cbind(Rnew[1:length(Rnew) - 1])
    colnames(Rnew) <- labels_cols[1:length(labels_cols) - 1]
  }
  rownames(Rnew) <- labels_rows

  return(Rnew)
}
