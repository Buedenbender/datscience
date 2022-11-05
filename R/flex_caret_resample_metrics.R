######################### Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")
# # Globals Variable Names not quoted due to tidyverse syntax
utils::globalVariables("Measure")
utils::globalVariables("Tune")
utils::globalVariables("value")

#' @author Bjoern Buedenbender
#'
#' @importFrom utils globalVariables
#' @importFrom flextable flextable merge_v
#' @importFrom psych describe
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom dplyr select all_of filter arrange desc relocate
#' @importFrom tidyr gather spread
#'
#' @seealso
#' \code{\link{format_flextable}},
#' \code{\link[flextable]{flextable}},
#' \code{\link[psych]{describe}}
#'
#' @title Extracts Classification Metrics during CV in Caret and Creates a Table for Publication
#'
#' @description
#'   A convenience function, that extracts desired classification metrics
#'   obtained during training (resampling, cross-validation), with caret,
#'   summarizes them (default min, mean, max, for more options see
#'   \code{\link[psych]{describe}}) and creates a
#'   \code{\link[flextable]{flextable}} object. The flextable is than formatted
#'   for publication with the \code{\link{format_flextable}} function.
#'   \if{html}{\figure{flex_resample_metrics.jpg}{Screenshot of example}}
#'
#' @param ls
#'     A list containing the name of the algorithm as index, and the resamples
#'     extracted from caret e.g.
#'     \code{models <- list("Decision Tree" = decision_tree$resample,
#'               "KNN" = knn$resample))}
#' @param nod
#'     The number of decimals to show for each classification metric
#' @param metrics Metrics that should be extracted from the resamples of the
#'     trained caret model. Please note that if you want to have full flexibility
#'     of parameters to evaluate you should use the summaryFunction
#'     \href{https://www.rdocumentation.org/packages/specmine/versions/3.1.6/topics/multiClassSummary}{caret::multiClassSummary} ,
#'     in \href{https://topepo.github.io/caret/model-training-and-tuning.html#the-traincontrol-function}{caret::trainControl}
#'     function. Defaults to a selection taken of multiClassSummary: \cr
#'     \code{metrics = c("Accuracy","Mean_Balanced_Accuracy", "Kappa", "logLoss",
#'     "Mean_Sensitivity", "Mean_Specificity" )}.
#' @param descriptives Summary stats that shall be calculated from the
#'     the resamples obtained in k-fold cross-validation training of a
#'     caret machine learning model. Summary stats utilize the
#'     \code{\link[psych]{describe}} function for calculation of summary.
#'     Per default the function extracts:
#'     \code{descriptives = c("min", "mean", "max")}. Other alternatives can
#'     be seen in the documentaiton of \code{\link[psych]{describe}} and
#'     comprise, e.g., median, skew, kurtosis, se
#' @param ...
#'   (Optional), Additional arguments. to be passed to
#'
#'   \code{\link[flextable]{flextable}}
#' @return
#'   A \code{\link[flextable]{flextable}} object with APA ready table that
#'   displays the performance metrics obtained during training with cross-validation
#'
#' @examples
#' \dontrun{
#' # Create Example Classifiers in the Iris Dataset
#' # set.seed(7)
#' # data(iris)
#' # Settings for the Cross-Validation
#' # control <- caret::trainControl(method="repeatedcv", number=10, repeats=3,
#' #                               summaryFunction = caret::multiClassSummary)
#' # Train Decision Tree
#' # suppressWarnings(
#' # decision_tree <- caret::train(Species~., data=iris, method="rpart",
#' #                                trControl=control, tuneLength=5)
#' # )
#' # Train k-Nearest Neighbors
#' # knn <-  caret::train(Species~., data=iris, method="knn",
#' #                     trControl=control, tuneLength=5)
#'
#' # Create a list of objects
#' # models <- list("Decision Tree" = decision_tree$resample,
#'               "KNN" = knn$resample)
#'
#' # save(models)
#'  models <- data(models)
#' # Create table with performance metrics during training
#' flex_resample_metrics(models)
#' }
#'
#' @export
flex_resample_metrics <- function(ls, nod = 3, metrics = c(
  "Accuracy",
  "Mean_Balanced_Accuracy", "Kappa",
  "logLoss",
  "Mean_Sensitivity", "Mean_Specificity"
),
descriptives = c("min", "mean", "max"),
...
) {

  # Inner function to calculate and extract descriptives of the resample
  extract_resample_metrics <- function(Algorithm, resamples,
                                       descriptives,
                                       metrics) {
    extracted_values <-      resamples |>
      psych::describe() |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "Measure") |>
      dplyr::select(Measure, dplyr::all_of(descriptives)) |>
      dplyr::filter(Measure %in% metrics) |>
      tibble::as_tibble() |>
      tidyr::gather(key = "Tune", value = value, 2:(1+length(descriptives))) |>
      tidyr::spread(key = "Measure", value = "value") |>
      dplyr::arrange(dplyr::desc(Tune))

    extracted_values$Algorithm <- Algorithm
    extracted_values <- extracted_values |>
      dplyr::relocate(Algorithm)

    return(extracted_values)
  }

  res <- do.call(rbind, lapply(names(ls), function(name) {
    extract_resample_metrics(
      Algorithm = name, resamples = ls[[name]],
      descriptives = descriptives,
      metrics = metrics
    )
  }))
  res <- mutate(res, dplyr::across(where(is.numeric), round, 3))
  if ("Mean_Balanced_Accuracy" %in% names(res)) res <- rename(res, "Mean Bal. Acc." = "Mean_Balanced_Accuracy")
  if ("Mean_Sensitivity" %in% names(res)) res <- rename(res, "Mean Sens." = "Mean_Sensitivity")
  if ("Mean_Specificity" %in% names(res)) res <- rename(res, "Mean Spec." = "Mean_Specificity")

  ft <- flextable::flextable(res) |>
    flextable::merge_v("Algorithm") |>
    format_flextable( ...)

  return(ft)
}
