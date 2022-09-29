#' Exemplary data of machine learning models for the flex_resample_metrics() function
#'
#' Models are created with 10-fold CV 3 x repeated in caret (see also the
#' source section of this documentation). The output is saved as .rda file.
#' This is to prevent the necessity of including the caret package as an
#' additional dependency to the datscience package
#'
#' @docType data
#'
#' @usage data(models)
#'
#' @format An object of class \code{"list"};
#'
#' @keywords datasets
#'
#' @source
#' # Create Example Classifiers in the Iris Dataset\cr
#' set.seed(7)\cr
#' data(iris)\cr
#' # Settings for the Cross-Validation\cr
#' control <- caret::trainControl(method="repeatedcv", number=10, repeats=3,
#'                                summaryFunction = caret::multiClassSummary)\cr
#' # Train Decision Tree\cr
#' decision_tree <- caret::train(Species~., data=iris, method="rpart",\cr
#'                                 trControl=control, tuneLength=5)\cr
#' # Train k-Nearest Neighbors\cr
#' knn <-  caret::train(Species~., data=iris, method="knn",
#'                      trControl=control, tuneLength=5)\cr
#'
#' # Create a list of objects\cr
#' models <- list("Decision Tree" = decision_tree$resample,
#'                "KNN" = knn$resample)\cr
#' @examples
#' data(models)
#' flex_resample_metrics(models, table_caption = NA)
"models"
