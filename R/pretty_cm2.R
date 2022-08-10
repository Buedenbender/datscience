######################### Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' @title pretty_cm - Pretty Caret Confusion Matrices
#' @description Takes a caret confusion matrix and plots a nice visualization.
#' Thanks to Felicitas Kininger for inspiring the inclusion of this function
#' in the package.
#' @param cm A \code{\link[caret]{confusionMatrix}} (mandatory parameter)
#' @param color_grad Pole of color gradient to use for the tiles, Default: c(alpha("yellowgreen", 0.4), alpha("springgreen3", 0.85))
#' @param midpoint Manually setting a middle point in percentage for the color scale, Default: 50
#' @param hideZero Hide tiles with 0 percentage, Default: FALSE
#' @param ord Order of the factor levels to display (if you want to change it manually for the plot), Default: NA,
#' @param diag Orientation of the diagonal (sensitivities), possible values diag = "r" or "reverse"
#' @param tile Character, either "p" or "prop" for proportion | "f" or "freq" for frequency | "b" or "both" for both.
#' if character is not recognized or missing -> Default: "both"
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
#' @importFrom dplyr group_by mutate ungroup case_when
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
                      diag = c("r","reverse"),
                      tile = c("both","b","prop","p","freq","f"),
                      plot = TRUE) {
  # Validate correct inputs
  if (missing(cm)) stop("Need to specify the mandatory argument \"cm\"")

  if (!is(cm, "confusionMatrix")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"cm\" is required to be a confusionMatrix"
    ))
  }
  # Check Numeric Vars: midpoint
  if (!is(midpoint, "numeric")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"midpoint\" is required to be a numeric"
    ))
  }
  # Check Logical Vars: plot, hideZero
  if (!is(plot, "logical")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"plot\" is required to be a logical"
    ))
  }
  if (!is(hideZero, "logical")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"hideZero\" is required to be a logical"
    ))
  }

  # PREPARATION OF THE DATA MATRIX
  cm_d <- as.data.frame(cm$table)
  labels <- names(cm_d)
  # extract the confusion matrix values as data.frame
  cm_d <- cm_d %>%
    # Create the proportion of the rowsum (in the diagonal it is sensitivity)
    dplyr::group_by(.data[[labels[2]]]) %>%
    dplyr::mutate(rowsum_ref = sum(.data[["Freq"]])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Prop = round(.data[["Freq"]] / .data$rowsum_ref * 100, 1),
      Prop_t = paste0(.data[["Prop"]], "%")
    )

  # order for the CM
  if (missing(ord)) {
    ord <- levels(cm_d[[labels[2]]])
  } else if (length(ord)==1){
    if(ord == "r" | ord == "reverse") ord <- levels(util_rev_fac(cm_d[[labels[2]]]))
  }

  # Change order of the y Axis
  cm_d[[labels[2]]] <- factor(cm_d[[labels[2]]],
    levels = ord
  )
  if(missing(diag) | length(diag) != 1){
    cm_d[[labels[1]]] <- factor(cm_d[[labels[1]]], levels = ord)
  } else{
    if (diag == "r" | diag == "reverse") {
      cm_d[[labels[1]]] <- util_rev_fac(factor(cm_d[[labels[1]]],
                                               levels = ord))
    } else     cm_d[[labels[1]]] <- factor(cm_d[[labels[1]]], levels = ord)
  }



  cm_d$ndiag <- cm_d[[labels[1]]] != cm_d[[labels[2]]] # Not the Diagonal
  cm_d$diag <- cm_d[[labels[1]]] == cm_d[[labels[2]]] # Get the Diagonal
  if (hideZero) {
    cm_d[cm_d == "0%"] <- NA # Replace 0 with NA for white tiles
    cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
  }

  cm_d[[labels[2]]] <- util_rev_fac(cm_d[[labels[2]]])
  cm_d$ref_Freq <- cm_d$Freq * ifelse(is.na(cm_d$diag), -1, 1)

  # Setting inscription of the tiles
  if(!missing(tile)){
    tile_content <- dplyr::case_when(
      tile == "f" | tile == "freq"  ~ as.character(cm_d$Freq),
      tile == "p" | tile == "prop"  ~ as.character(cm_d$Prop_t),
      TRUE ~ paste0(cm_d$Freq," (",cm_d$Prop_t,")")
    )
  } else{
    tile_content <- paste0(cm_d$Freq," (",cm_d$Prop_t,")")
  }


  # === Creating the Plot ===
  cm_d_p <- ggplot(data = cm_d, aes(
    x = .data[[labels[1]]], y = .data[[labels[2]]],
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
    geom_text(aes(label = tile_content), color = "black", size = 5) +
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
    ylab(labels[2])
  if (plot) plot(cm_d_p)
  return(cm_d_p)
}
