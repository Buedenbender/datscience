######################### Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' @title pretty_cm - Pretty Caret Confusion Matrices
#' @description Takes a caret confusion matrix and plots a nice visualization
#' @param cm A \code{\link[caret]{confusionMatrix}} (mandatory parameter)
#' @param color_grad Pole of color gradient to use for the tiles, Default: c(alpha("yellowgreen", 0.4), alpha("springgreen3", 0.85))
#' @param midpoint Manually setting a middle point in percentage for the color scale, Default: 50
#' @param hideZero Hide tiles with 0 percentage, Default: FALSE
#' @param ord Order of the factor levels to display (if you want to change it manually for the plot), Default: NA
#' @param plot Shall the output also be plotted? Default: TRUE
#' @return ggplot2 object - visualization of the confusion matrix.
#' @author Björn Büdenbender
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Creating random example data: prediction of neural network on content of animal pictures
#'   set.seed(23)
#'   pred <- factor(sample(c("dog", "cat"), 100, replace = TRUE))
#'   ref <- factor(sample(c("dog", "cat"), 100, replace = TRUE))
#'   cm <- caret::confusionMatrix(pred, ref)
#'   # Plotting of the caret confusion matrix
#'   pretty_cm(cm)
#' }
#' }
#' @rdname pretty_cm
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom rlang .data
#' @import ggplot2
#' @export
#' @seealso
#' \code{\link[caret]{confusionMatrix}}

pretty_cm <- function(cm,
                      color_grad = c(
                        alpha("yellowgreen", 0.4),
                        alpha("springgreen3", 0.85)
                      ),
                      midpoint = 50, hideZero = FALSE, ord = NA,
                      plot = TRUE) {

  # PREPARATION OF THE DATA MATRIX
  # extract the confusion matrix values as data.frame
  cm_d <- as.data.frame(cm$table) %>%
    # Create the proportion of the rowsum (in the diagonal it is sensitivity)
    dplyr::group_by(.data$Reference) %>%
    dplyr::mutate(rowsum_ref = sum(.data$Freq)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Prop = round(.data$Freq / .data$rowsum_ref * 100, 1),
      Prop_t = paste0(.data$Prop, "%")
    )

  # order for the CM
  if (missing(ord)) {
    ord <- levels(cm_d$Reference)
  }

  # Change order of the y Axis
  cm_d$Reference <- factor(cm_d$Reference,
    levels = ord
  )
  cm_d$Prediction <- factor(cm_d$Prediction,
    levels = ord
  )

  cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Not the Diagonal
  cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
  if (hideZero) {
    cm_d[cm_d == "0%"] <- NA # Replace 0 with NA for white tiles
    cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
  }

  cm_d$Reference <- util_rev_fac(cm_d$Reference)
  cm_d$ref_Freq <- cm_d$Freq * ifelse(is.na(cm_d$diag), -1, 1)

  cm_d_p <- ggplot(data = cm_d, aes(
    x = .data$Prediction, y = .data$Reference,
    fill = .data$Freq
  )) +
    scale_x_discrete(position = "top") +
    geom_tile(data = cm_d[!is.na(cm_d$diag), ], aes(fill = .data$Prop)) +
    scale_fill_gradient2(
      low = alpha("white", 1),
      mid = color_grad[1],
      # high=alpha("limegreen",0.99),
      # high=alpha("forestgreen",1),
      high = color_grad[2],
      midpoint = midpoint,
      na.value = "black"
    ) +
    geom_text(aes(label = .data$Prop_t), color = "black", size = 5) +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.text.x = element_text(
        color = "black", size = 13, face = "plain", family = "sans", angle = 45, hjust = -0.1,
        margin = margin(t = 20, r = 0, b = 20, l = 0)
      ),
      axis.text.y = element_text(
        color = "black", size = 13,
        face = "plain", family = "sans"
      ),
      # margin to slightly increase distance to the axis ticks
      axis.title.x = element_text(
        color = "black", size = 16, face = "bold", family = "sans",
        margin = margin(t = 20, r = 0, b = 20, l = 0)
      ),
      axis.title.y = element_text(
        color = "black", size = 16, face = "bold", family = "sans",
        margin = margin(t = 0, r = 20, b = 0, l = 0)
      )
    ) +
    guides(fill = "none") +
    ylab("Reference")
  if (plot) plot(cm_d_p)
  return(cm_d_p)
}
