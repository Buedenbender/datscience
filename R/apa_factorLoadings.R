########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")



#' Create an APA Formatted Table of Factor Loadings
#' @description Extracts factor loadings from factor analyses (either PCA or EFA) conducthed
#' with the \href{https://cran.r-project.org/web/packages/psychTools/vignettes/factor.pdf}{psych package},
#' and returns a APA 7th formatted table with the factor loadings and additional metrics (Communality, Uniqueness, Complexity).
#' If desired this table can directly be exported to a Word file (.docx).
#' @param fa_object Either an \code{\link[psych]{fa}} or \code{\link[psych]{principal}}, from
#' which the factor loadings are to be extracted
#' @param filepath (Optional) Path and filename were the APA ready table should
#' be saved, options include the common filetypes .docx (Word), .pptx (Powerpoint),
#' .html (Webpage). Default is filepath = NA. If NA is given, no file will be saved.
#' @param table_caption Takes a character vector. Recommend are the following elements
#' \itemize{
#' \item Table + number (e.g., "\strong{Table 3}")
#' \item The description, use APA capital case
#' (e.g., "\emph{PCA / EFA Factor Loadings}")
#' }
#' @param nod (Optional) Integer or Integer Vector. Number of Decimals.
#' In case of -1 a simple convention based on sample size is applied for determination
#' of number of decimal points. See ?datscience::get_number_of_decimals.
#' You can also provide an Integer vector, if you want different number of decimals
#' for the correlations and the summary stats. The first integer determines nod for
#' factor loadings, the second for additional stats. E.g., c(2,-1) would give 2 decimals
#' for factor loadings and apply the convention for Complexity, Uniqueness and Communality.
#' Default is nod = c(3,2). \cr
#' See \code{\link{get_number_of_decimals}} or\code{?datscience::get_number_of_decimals}
#' @param overwrite (Optional) Boolean, default is FALSE. When overwrite is
#' FALSE and the files already exists, a serialized version of the filename
#' will be created (i.e., appending _001, or _002). Utilizes \code{\link{serialNext}}
#' @param n.obs (Optional), Only required if you want to apply the convention to number
#' of decimals, and did calculate the factor analysis (PCA, EFA) directly on a correlation
#' or covariance matrix.
#' @param cutoff (Optional), Integer, determines the minimum threshold, values below are omitted.
#' Default is cutoff = 0.3.
#' @param orderbyloading (Optional), Boolean, default is \code{orderbyloading = TRUE}. If TRUE, items in the
#' table will be sorted by loading on the first factor / component (as in the convention). In some use cases, you rather
#' want to maintain the ordering you used to conduct the analysis (e.g., alphabetical). You can archieve this
#' by providing \code{orderbyloading = FALSE}
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...) or to \code{\link{serialNext}}
#'
#' @return A \code{\link[flextable]{flextable}} object with APA ready correlation table. If a filepath is provided
#' it also creates the respective file (e.g., a word .docx file)
#'
#' @author Bjoern Buedenbender
#' @examples
#' datscience::apa_factorLoadings(psych::fa(mtcars, nfactors = 2))
#' @export
#'
#' @importFrom psych fa.sort
#' @importFrom dplyr mutate across everything if_else bind_cols
#' @importFrom tibble rownames_to_column
#' @importFrom flextable flextable
#' @seealso \code{\link{get_number_of_decimals}}, \code{\link{format_flextable}},
#' \code{\link{serialNext}}
apa_factorLoadings <- function(fa_object, filepath = NA,
                               table_caption = c(
                                 "Table X",
                                 "PCA / EFA Factor Loadings"
                               ),
                               overwrite = FALSE,
                               nod = c(3, 2),
                               n.obs = NA,
                               cutoff = 0.3,
                               orderbyloading = TRUE,
                               ...) {

  ### Determine number of decimals (nod)
  # Separate nod for factor loadings and additional stats?

  # Check if n is available
  if (!is.na(fa_object$n.obs)) n <- fa_object$n.obs else n <- n.obs

  if (length(nod) == 1) {
    # Check if n is available
    # Apply convention for number of decimals (nod) if -1
    if (nod == -1 && !is.na(n)) {
      nod <- get_number_of_decimals(n)
    }
    nod_loadings <- nod
    nod_additional <- nod
  } else if (length(nod) == 2) {
    if (nod[1] == -1 && !is.na(n)) nod_loadings <- get_number_of_decimals(n) else nod_loadings <- nod[1]
    if (nod[2] == -1 && !is.na(n)) nod_additional <- get_number_of_decimals(n) else nod_additional <- nod[2]
  } else {
    print("There was a problem with the desired number of decimals (nod). If you
          wanted to apply the convention, either nod = -1 or nod = c(-1,-1) make sure
          that you provide the number of observations (n.obs)")
  }

  # Create Table / Extract Factor Loadings
  # pc_loadings <-
  if (orderbyloading) {
    fa_object <- fa_object |>
      psych::fa.sort()
  }
  pc_loadings <- fa_object$loadings |>
    round(nod_loadings) |>
    unclass() |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else((. < cutoff), "", as.character(.))
    )) |>
    dplyr::bind_cols(
      Communality = fa_object$communality,
      Uniqueness = fa_object$uniquenesses,
      Complexity = fa_object$complexity
    ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), round, nod_additional)) |>
    tibble::rownames_to_column("Item")

  # Create APA Flextable
  ft <- format_flextable(flextable::flextable(pc_loadings),
                         table_caption = table_caption,
                         ...
  )

  # Saving the Table
  # Check if a correct file path is provided
  if (!is.na(filepath) && filepath != "") {
    save_flextable(ft = ft, filepath = filepath, overwrite = overwrite)
  }

  return(ft)
}
