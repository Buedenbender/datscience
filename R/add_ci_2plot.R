########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables(".")

#' Add CI to Pretty Screeplot
#' @description After CIs are determined by a combination of
#' \code{\link{booted_eigenvalues}} & \code{\link{getCIs}}
#' This function provided with the plot object and the data.frame with CIs
#' adds CI around the observd eigenvalues
#'
#' @param plot The plot object as obtained by pretty_scree()
#' @param CIs Data.frame obtained from getCIs() and booted_eigenvalues()
#' @param met the type of CI to be used default "normal"
#' | other options: "basic", "stud", "perc", "bca"
#' @param color If type of visual display of the CI = "band"
#' the color of the area between Upper and Lower
#' @param outline Color of the Outline for the Area
#' @param transparency Alpha value, how transparent shall the filling color be
#' @param type either "band" or "errorbars", defaults to "band"
#' @param lt Linetype, 0 for no outline around the band, other options include: "dotted", "dashed", "solid"
#'
#' @return A new pretty_plot with the Parallel Analysis,
#' including CIs around the observed Eigenvalues.
#'
#' @author Bjoern Buedenbender
#'
#' @export
#' @importFrom dplyr rename select mutate
#' @importFrom ggplot2 geom_ribbon geom_errorbar aes
#' @importFrom magrittr "%>%"
#' @seealso \code{\link{booted_eigenvalues}}, \code{\link{getCIs}}
add_ci_2plot <- function(plot,
                         CIs,
                         met = "normal",
                         color = "darkseagreen3",
                         outline = "black",
                         transparency = 0.5,
                         type = "band",
                         lt = "dotted") {

  # globalVariables(c("mpg", "hp", "mpg_div_hp"))
  method <- index <- observed <- lwr <- upr <- NULL

  cis <- CIs %>%
    filter(method == met) %>%
    dplyr::rename(num = index) %>%
    dplyr::select(-method) %>%
    dplyr::mutate(
      type = "Observed Data",
      eigenvalue = observed
    )
  if (type == "band") {
    new_plot <-
      plot + ggplot2::geom_ribbon(
        data = cis,
        ggplot2::aes(ymin = lwr, ymax = upr),
        fill = color,
        color = outline,
        alpha = transparency,
        linetype = lt
      )
  } else if (type == "errorbars") {
    new_plot <-
      plot + ggplot2::geom_errorbar(
        data = cis,
        ggplot2::aes(ymin = lwr, ymax = upr),
        color = outline,
        alpha = transparency,
        width = .15
      )
  }

  return(new_plot)
}

