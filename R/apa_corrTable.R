########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# General todos
# TODO: - Sort, e.g., all functions required for factoranalyses
# TODO: - INclude a Vignette for all Factor analysis function
# TODO:
#################### Basic Functions / Stand Alone ######################
# Setting up global exports to fix RMD Check problems for unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' Create APA Publication Ready Correlation Table and Export to Word or Powerpoint
#'
#' @description
#' The Idea was inspired by a blog post of my colleague Remi Theriault
#' (see \href{https://remi-theriault.com/blog_table.html}{remi-theriault.com})
#' Which utilized the ability of \code{\link[flextable]{flextable}} to be able to get an
#' APA-style formatted table directly from R into .docx (word). One frequent use case
#' is to get a correlation table into word. For the creation of the correlation table
#' this function uses the \code{\link{corstars}} function from this package, which
#' resolves around the code by
#' \href{https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R}{Dominik Vogel}.\cr
#' I built on those idea and created a function that creates a pretty correlation table
#' together with the summary stats of your choice, and returns a flextable, which
#' can be easily exported to word (see also  \code{\link{save_flextable}})\cr
#' \strong{Please note:}
#' \itemize{
#' \item This function only considers numeric variables (cols). Other
#' datatypes will be dropped from the data.frame df.
#' \item There is an interaction between the arguments \code{sig.level} and \code{nod}.
#' When sig.level is set to NA, this improves the formatting of decimals places in the finale table,
#' and removes the stars marking significance. However, this also prevents the table from having
#' differing number of decimal places. Meaning the number of decimals for the correlation will also determine
#' the number of decimals for the summary statistics
#' }
#'
#' @param df Data.frame, mandatory argument. Consider filtering before passing it
#' e.g. with dplyr::select() and dplyr::filter()
#' @param summarystats A vector with the summary stats to be included at the bottom
#' below the correlation. \cr Default is \code{c("mean","sd")}
#' Options are one or all of \code{c("mean","sd","median","range","min","max","skew",
#' "kurtosis","se","missing")}. The option "missing", adds missings per item as additional row
#' (accecpts both "missing" and "missings", spelling in table accordingly).
#' If NA is given, no summarystats will be added.
#' @param method Type of correlation. Options are currently: "pearson", "spearman" and "polychoric"
#' @param rmDiag Should the diagonal in the corr matrix kept (FALSE) or removed (TRUE)
#' @param sig.level How many stars per level of significance, options include
#' .05 .01 or .001. If NA no stars indicating significance will be output. This improves
#' formatting of decimals in the table. Note the default for polychoric is NA
#' @param nod (Optional) Integer or Integer Vector. Number of Decimals.
#' In case of -1 a simple convention based on sample size is applied for determination
#' of number of decimal points. See \code{\link{get_number_of_decimals}}.
#' You can also provide an Integer vector, if you want different number of decimals
#' for the correlations and the summary stats. The first integer determines nod for
#' correlations, the second for summary stats. E.g., \code{nod = c(2,-1)} would give 2 decimals
#' for correlations and apply the convention for summary stats. Default is \code{nod = c(2,-1)}. \cr
#' See \code{\link{get_number_of_decimals}} or\code{?datscience::get_number_of_decimals}
#' @param filepath (Optional) Path and filename were the APA ready table should
#' be saved, options include the common filetypes .docx (Word), .pptx (Powerpoint),
#' .html (Webpage). Default is \code{filepath = NA}. If NA is given, no file will be saved.
#' @param overwrite (Optional) Boolean, default is FALSE. When overwrite is
#' FALSE and the files already exists, a serialized version of the filename
#' will be created (i.e., appending _001, or _002). Utilizes \code{\link{serialNext}}
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...) or to \code{\link{serialNext}}
#'
#' @return A \code{\link[flextable]{flextable}} object with APA ready correlation table.
#'
#' @author Bjoern Buedenbender (Formatting based on Remi Theriault,
#' Correlations based on Dominik Vogel)
#'
#' @examples
#' library(datscience)
#' apa_corrTable(mtcars, table_caption = c("Table 2", "Correlations in the mtcars Data Set"))
#' @export
#' @importFrom stats na.omit
#' @importFrom dplyr select summarise across everything
#' @importFrom stringr str_to_title
#' @importFrom psych describe
#' @importFrom tibble rownames_to_column
#' @importFrom flextable flextable
#'
#' @seealso \code{\link{get_number_of_decimals}}, \code{\link{format_flextable}},
#' \code{\link{serialNext}}
apa_corrTable <- function(df,
                          summarystats = c("mean", "sd"),
                          method = c("pearson", "spearman", "polychoric"),
                          rmDiag = FALSE,
                          sig.level = 0.05,
                          nod = c(2, -1),
                          filepath = NA,
                          overwrite = FALSE,
                          ...) {
  # Exclude non-numeric cols
  df <- df |>  dplyr::select(where(is.numeric))

  # TODO: Add problematic correlations (as summarystat?, ferketisch)

  ### Determine number of decimals (nod)
  # Separate nod for correlations and summary stats?
  if (length(nod) == 1) {
    # Apply convention for number of decimals (nod) if -1
    if (nod == -1) nod <- get_number_of_decimals(nrow(df))
    nod_cor <- nod
    nod_sum <- nod
  } else if (length(nod) == 2) {
    if (nod[1] == -1) nod_cor <- get_number_of_decimals(nrow(df)) else nod_cor <- nod[1]
    if (nod[2] == -1) nod_sum <- get_number_of_decimals(nrow(df)) else nod_sum <- nod[2]
    # When no sig. is added to table, summary stats will by default use the nod of the
    # correlations. Thus in calculating them, one could provide the values at first place,
    # rather than rounding them to e.g. 1 and displaying 2 nods anyways
    if (is.na(sig.level)) nod_sum <- nod_cor
  }

  # Creating Correlation table
  corstars(df,
    rmLastCol = FALSE,
    rmDiag = rmDiag,
    method = method[1],
    sig.level = sig.level,
    nod = nod_cor
  ) -> correlations

  ### Getting Descriptives
  used.stats <- c() # empty vector iteratively filled, used to individualize formatting
  # Determine which summarystats are requested
  psych_sumstats <- c(
    "mean", "sd", "median", "range", "min", "max", "skew",
    "kurtosis", "se"
  )
  for (stat in summarystats) {
    if (any(stat %in% psych_sumstats)) {
      correlations[stringr::str_to_title(stat), ] <- round(psych::describe(df)[[stat]], nod_sum)
    } else if (stat == "missing" || stat == "missings") {
      correlations[stringr::str_to_title(stat), ] <- df |>
        dplyr::summarise(dplyr::across(
          dplyr::everything(),
          ~ sum(is.na(.))
        ))
    }
  }

  ### Flextable
  flextable::flextable(correlations |>
    tibble::rownames_to_column(" ")) -> corr_table

  # Formatting of the table by datscience::format_flextable(), inspired by Remi Theriault
  corr_table <- format_flextable(ft = corr_table, ...)

  # Saving the Table
  # Check if a correct file path is provided
  if (!is.na(filepath) && filepath != "") {
    save_flextable(ft = corr_table, filepath = filepath, overwrite = overwrite)
  }

  return(corr_table)
}
