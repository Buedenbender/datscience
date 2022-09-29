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
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source
#' # Create Example Classifiers in the Iris Dataset
#' set.seed(7)
#' data(iris)
#' # Settings for the Cross-Validation
#' control <- caret::trainControl(method="repeatedcv", number=10, repeats=3,
#'                                summaryFunction = caret::multiClassSummary)
#' # Train Decision Tree
#' decision_tree <- caret::train(Species~., data=iris, method="rpart",
#'                                 trControl=control, tuneLength=5)
#' # Train k-Nearest Neighbors
#' knn <-  caret::train(Species~., data=iris, method="knn",
#'                      trControl=control, tuneLength=5)
#'
#' # Create a list of objects
#' models <- list("Decision Tree" = decision_tree$resample,
#'                "KNN" = knn$resample)
#' @examples
#' data(models)
#' flex_resample_metrics(models, table_caption = NA)
"models"
