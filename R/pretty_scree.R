########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' Pretty Scree
#' @description
#' Creates an Screeplot Including a Parallel Analysis (Horn), formatted according to APA
#' 7th style. The original code was developed by John Sakaluk
#' Check the original source: \href{https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/#psych}{at his wordpress blog}.
#' @param parallel an parallel object returned by \code{psych::fa.parallel}
#' @param fa either "pc" or "fa" factor methods are allowed for the parallel analysis
#' @param quant default = .95 the quantile of the simulated values used to plot
#' @param custom_optimal default = NA, e.g., setting a marked dashed line to the location
#' determined by fa.parallel. Given 0 hides the line, all other value > 0 set the dashed
#' line to a custom location (e.g., to indicate a prefered number of factors determined by VSS or MAP)
#'
#' @return APA Ready Plot of Parallel Analyssis
#'
#' @author John Sakaluk (Wrapped in a Function and Advanced by Bjoern Buedenbender)
#'
#'
#' @export
#' @import ggplot2
#' @seealso \code{\link[psych]{fa.parallel}}
pretty_scree <- function(parallel, fa, quant = .95, custom_optimal = NA) {
  num <- eigenvalue <- type <- NULL
  # Abbreviations
  #   - noi = Number of Interest, Components or Factors determined by Parallel-Analysis after Horn
  #   - cf = Common Factor
  #   - pc = Prinicipal Components
  # Calculate quantiles for eigenvalues, for sim. pc and cf
  percentile <- apply(parallel$values, 2, function(x) {
    stats::quantile(x, quant)
  })

  # If Screeplot should be prettified for principal components
  if (fa == "pc") {
    index <- grep("CSim", names(percentile))
    if (length(index) == 0) {
      index <- grep("Sim", names(percentile))
    }
    if (length(index) == 0) {
      index <- grep("C", names(percentile))
    }
    obs <- data.frame(parallel$pc.values)
    percentile1 <- percentile[index]
    noi <- parallel$ncomp
  } else if (fa == "fa") {
    index <- grep("Fsim", names(percentile))
    if (length(index) == 0) {
      index <- grep("Sim", names(percentile))
    }
    if (length(index) == 0) {
      index <- grep("F", names(percentile))
    }
    obs <- data.frame(parallel$fa.values)
    percentile1 <- percentile[index]
    noi <- parallel$nfact
  }
  # If custom_optimal, replace noi
  if (!is.na(custom_optimal) && is.numeric(custom_optimal)) noi <- custom_optimal
  # Create Data Frame for Observed Eigenvalues
  obs$type <- c("Observed Data")
  obs$num <- c(row.names(obs))
  obs$num <- as.numeric(obs$num)
  colnames(obs) <- c("eigenvalue", "type", "num")
  # Create data frame called with simulated eigenvalue data
  sim <- data.frame(percentile1)
  sim$type <- c("Simulated Data (95th %ile)")
  sim$num <- c(row.names(obs))
  sim$num <- as.numeric(sim$num)
  colnames(sim) <- c("eigenvalue", "type", "num")
  # Merge the two data frames (obs and sim) together into data frame called eigendat
  eigendat <- rbind(obs, sim)

  # Create an APA Theme for the Plot
  apatheme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Arial"),
      legend.title = ggplot2::element_blank(),
      legend.position = c(.7, .8),
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.line.y = ggplot2::element_line(color = "black")
    )

  # Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
  p <- ggplot2::ggplot(eigendat, ggplot2::aes(x = num, y = eigenvalue, shape = type)) +
    # Add lines connecting data points
    ggplot2::geom_line() +
    # Add the data points.
    ggplot2::geom_point(size = 4) +
    # Label the y-axis 'Eigenvalue'
    ggplot2::scale_y_continuous(name = "Eigenvalue") +
    # Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
    ggplot2::scale_x_continuous(
      name = "Factor Number",
      breaks = min(eigendat$num):max(eigendat$num)
    ) +
    # Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
    ggplot2::scale_shape_manual(values = c(16, 1))

  if (is.na(custom_optimal)) {
    p <- p +
      # Add vertical line indicating parallel analysis suggested max # of factors to retain
      ggplot2::geom_vline(xintercept = noi, linetype = "dashed")
  } else {
    if (custom_optimal != 0) {
      print("debug")
    }
  }

  p <- p +
    # Apply our apa-formatting theme
    apatheme
  # Call the plot. Looks pretty!
  return(p)
}
