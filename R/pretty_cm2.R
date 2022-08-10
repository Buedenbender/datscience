######################### Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' @author Björn Büdenbender
#'
#' @import ggplot2
#' @importFrom dplyr group_by mutate ungroup case_when
#' @importFrom rlang .data
#'
#' @seealso
#' \code{\link[caret]{confusionMatrix}}
#' \code{\link[ggplot2]{theme}}
#'
#' @title pretty_cm - Pretty Confusion Matrices
#'
#' @description
#'   Takes a confusion matrix (either a data.frame, table or an
#'   \code{\link[caret]{confusionMatrix}} object and plots a nice visualization.
#'   Thanks to Felicitas Kininger for inspiring the inclusion of this function
#'   into the package.
#'
#' @details
#'   You can change all fonts of the plot later on with
#'   \code{\link[ggplot2]{theme}}. Use the following inside the call to theme
#'   \itemize{
#'     \item
#'       \code{theme(axis.title.x = element_text(size=14))}
#'       to change axis title
#'     \item
#'       \code{axis.text.x = element_text(size=12)}
#'       to change axis ticks (description labels)
#'   }
#'
#' @param cm
#'   Either a \code{\link[caret]{confusionMatrix}}, a table or a data.frame,
#'   with prediction as the
#'   the rows and reference as the columns (mandatory parameter)
#' @param color_grad
#'   Pole of color gradient to use for the tiles, Default:
#'   c(alpha("yellowgreen", 0.4), alpha("springgreen3", 0.85))
#' @param midpoint
#'   Numeric, Default = 50;
#'   Manually setting a middle point in percentage for the color
#'   scale.
#' @param hide_zero Hide tiles with 0 percentage, Default: FALSE
#' @param ord
#'   Character, Default = NA;
#'   Order of the factor levels to display (if you want to change it
#'   manually for the plot).
#' @param diag
#'   Orientation of the diagonal (sensitivities), possible values
#'   diag = "r" or "reverse"
#' @param tile
#'   Character, Default = "both"; Either "p" or "prop" for proportion | "f" or
#'   "freq" for frequency | "b" or "both" for both.
#'   If character is not recognized or missing it goes to "both".
#' @param tile_size
#'   Numeric, Default = 3.5; Determines the size of the font in the tiles.
#'   Be wary, other scale than for usual font size.
#' @param tile_nod
#'   Numeric (or NA), Default = 1; Determines the number of decimals
#'   to be displayed in case tiles should show percentages "p".
#' @param plot Logical, Default = TRUE; Shall the output also be plotted?
#'
#' @return ggplot2 object - visualization of the confusion matrix.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Creating random example data: prediction of neural network on content
#'   # of animal pictures
#'   set.seed(23)
#'   pred <- factor(sample(c("dog", "cat"), 100, replace = TRUE))
#'   ref <- factor(sample(c("dog", "cat"), 100, replace = TRUE))
#'   cm <- caret::confusionMatrix(pred, ref)
#'   # Plotting of the caret confusion matrix
#'   pretty_cm(cm)
#' }
#' }
#'
#' @rdname pretty_cm
#'
#' @export

pretty_cm <- function(cm,
                      color_grad = c(
                        alpha("yellowgreen", 0.4),
                        alpha("springgreen3", 0.85)
                      ),
                      midpoint = 50, hide_zero = FALSE, ord = NA,
                      diag = c("r", "reverse"),
                      tile = c("both", "b", "prop", "p", "freq", "f"),
                      tile_size = 3.5,
                      tile_nod = 1,
                      plot = TRUE) {

  ### Validate correct inputs ###
  if (missing(cm)) stop("Need to specify the mandatory argument \"cm\"")

  if (!is(cm, "confusionMatrix") & !is(cm, "table") & !is(cm, "data.frame")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"cm\" is required to be a confusionMatrix, table or data.frame"
    ))
  }
  # Check Numeric Vars: midpoint, tile_size, tile_nod
  if (!is(midpoint, "numeric")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"midpoint\" is required to be a numeric"
    ))
  }
  if (!is(tile_size, "numeric")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"tile_size\" is required to be a numeric"
    ))
  }
  #   - confirm there is only a single number supplied to tile_nod
  if(length(tile_nod)!=1){
    stop(paste(
      "Invalid argument type. The argument",
      "\"tile_nod\" is required to be only a single numeric value"
    ))
  }
  #   - if NA is provided convert it to 0
  if(is.na(tile_nod)) tile_nod <- 0
  if (!is(tile_nod, "numeric")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"tile_nod\" is required to be a numeric"
    ))
  }
  # Check Logical Vars: plot, hide_zero
  if (!is(plot, "logical")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"plot\" is required to be a logical"
    ))
  }
  if (!is(hide_zero, "logical")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"hide_zero\" is required to be a logical"
    ))
  }
  # Check character Vars: tile, diag
  if (missing(tile)) tile <- "both"
  if (!is(tile, "character") | length(tile) != 1) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"tile\" is required to be a character of length 1"
    ))
  }
  if (!missing(diag) & (!is(diag, "character") | length(diag) != 1)) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"diag\" is required to be a character of length 1"
    ))
  }

  ### PREPARATION OF THE DATA MATRIX ###
  if (is(cm, "confusionMatrix")) {
    cm_d <- as.data.frame(cm$table)
  } else if (is(cm, "table")) {
    cm_d <- as.data.frame(cm)
  } else {
    cm_d <- cm
  }
  labels <- names(cm_d)

  # extract the confusion matrix values as data.frame
  cm_d <- cm_d %>%
    # Create the proportion of the rowsum (in the diagonal it is sensitivity)
    dplyr::group_by(.data[[labels[2]]]) %>%
    dplyr::mutate(rowsum_ref = sum(.data[["Freq"]])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Prop = round(.data[["Freq"]] / .data$rowsum_ref * 100, tile_nod),
      Prop_t = paste0(.data[["Prop"]], "%")
    )

  # order for the CM
  if (missing(ord)) {
    ord <- levels(cm_d[[labels[2]]])
  } else if (length(ord) == 1) {
    if (ord == "r" | ord == "reverse") {
      ord <- levels(util_rev_fac(
        cm_d[[labels[2]]]
      ))
    }
  }

  # Change order of the y Axis
  cm_d[[labels[2]]] <- factor(cm_d[[labels[2]]],
    levels = ord
  )
  if (missing(diag) | length(diag) != 1) {
    cm_d[[labels[1]]] <- factor(cm_d[[labels[1]]], levels = ord)
  } else {
    if (diag == "r" | diag == "reverse") {
      cm_d[[labels[1]]] <- util_rev_fac(factor(cm_d[[labels[1]]],
        levels = ord
      ))
    } else {
      cm_d[[labels[1]]] <- factor(cm_d[[labels[1]]], levels = ord)
    }
  }

  cm_d$ndiag <- cm_d[[labels[1]]] != cm_d[[labels[2]]] # Not the Diagonal
  cm_d$diag <- cm_d[[labels[1]]] == cm_d[[labels[2]]] # Get the Diagonal
  if (hide_zero) {
    cm_d[cm_d == "0%"] <- NA # Replace 0 with NA for white tiles
    cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
  }

  cm_d[[labels[2]]] <- util_rev_fac(cm_d[[labels[2]]])
  cm_d$ref_Freq <- cm_d$Freq * ifelse(is.na(cm_d$diag), -1, 1)

  # Setting inscription of the tiles
  if (!missing(tile)) {
    tile_content <- dplyr::case_when(
      tile == "f" | tile == "freq" ~ as.character(cm_d$Freq),
      tile == "p" | tile == "prop" ~ as.character(cm_d$Prop_t),
      TRUE ~ paste0(cm_d$Freq, " (", cm_d$Prop_t, ")")
    )
  } else {
    tile_content <- paste0(cm_d$Freq, " (", cm_d$Prop_t, ")")
  }


  ### Creating the Plot ###
  cm_d_p <- ggplot(data = cm_d, aes(
    x = .data[[labels[1]]], y = .data[[labels[2]]],
    fill = .data$Freq
  )) +
    scale_x_discrete(position = "top") +
    geom_tile(data = cm_d[!is.na(cm_d$diag), ], aes(fill = .data$Prop)) +
    scale_fill_gradient2(
      low = alpha("white", 1),
      mid = color_grad[1],
      high = color_grad[2],
      midpoint = midpoint,
      na.value = "black"
    ) +
    geom_text(aes(label = tile_content), color = "black", size = tile_size) +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.text.x = element_text(
        color = "black", size = 12, face = "plain", family = "sans", angle = 45,
        hjust = -0.1, margin = margin(t = 20, r = 0, b = 20, l = 0)
      ),
      axis.text.y = element_text(
        color = "black", size = 12,
        face = "plain", family = "sans"
      ),
      # margin to slightly increase distance to the axis ticks
      axis.title.x = element_text(
        color = "black", size = 14, face = "bold", family = "sans",
        margin = margin(t = 20, r = 0, b = 20, l = 0)
      ),
      axis.title.y = element_text(
        color = "black", size = 14, face = "bold", family = "sans",
        margin = margin(t = 0, r = 20, b = 0, l = 0)
      )
    ) +
    guides(fill = "none") +
    ylab(labels[2])
  if (plot) plot(cm_d_p)
  return(cm_d_p)
}
