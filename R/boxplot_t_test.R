########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#'  Creates Boxplots with Significance Makers
#' @description
#' Code for this function is based on the \href{https://www.datanovia.com/en/blog/how-to-perform-multiple-t-test-in-r-for-different-variables/}{Guide of A. Kassambra on datanovia}
#' The functions creates given a vector of dependent variables (DV), nicely formatted boxplots
#' with facetwrap for all DVs and calculates an independent sample T-Test
#' to include significance bars
#'
#' @param df data.frame.
#' @param dependentvars Character vector.
#' @param group Character vector.
#' @param adjust_p Character vector.  "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#' See \code{\link[rstatix]{adjust_pvalue}} for more details
#' @param ylimits Numeric vector.
#'
#'
#' @examples
#' boxplot_t_test(mtcars, c("mpg", "hp"), group = "am")
#' @return List(Plot and stats)
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom rstatix t_test adjust_pvalue add_significance add_xy_position
#' @importFrom ggpubr ggboxplot
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom ggpubr theme_pubr
boxplot_t_test <-
  function(df,
           dependentvars,
           group,
           adjust_p = "BH",
           ylimits = c(0, 150)) {
    # Validate argument types
    if (!is.data.frame(df)) stop("Invalid argument type, df is required to be a data.frame or tibble")
    if (!is.character(dependentvars)) stop("Invalid argument type, dependentvars is required to be a character or a vector of characters containing the names of the respective columns")
    if (!is.character(group)) stop("Invalid argument type, group is required to be a character or a vector of characters containing the names of the respective columns")
    if (!is.character(adjust_p)) stop("Invalid argument type, adjust_p is required to be a character or a vector of characters containing the names of the respective columns")

    variables <- NULL
    df_s <- df |>
      dplyr::select(tidyselect::all_of(dependentvars), tidyselect::all_of(group)) |>
      dplyr::as_tibble()

    # Pivot Table
    df_p <- df_s |>
      tidyr::pivot_longer(-tidyselect::all_of(group),
        names_to = "variables", values_to = "value"
      )

    # Iterating over possible groups
    output = list()

    for (g in group) {
      # Calc T Test
      stat.test <- df_p |>
        dplyr::group_by(`variables`) |>
        rstatix::t_test(stats::as.formula(paste("value ~", g))) |>
        rstatix::adjust_pvalue(method = adjust_p) |>
        rstatix::add_significance()
      # stat.test

      # Creating the Plot
      p1 <-
        ggpubr::ggboxplot(
          df_p,
          x = g,
          y = "value",
          fill = g,
          palette = "npg",
          legend = "none",
          ggtheme = ggpubr::theme_pubr(border = TRUE)
        ) +
        # ylim(ylimits)+
        ggplot2::facet_wrap(~variables)

      stat.test <- stat.test |> rstatix::add_xy_position(x = g)
      p1 <-
        p1 + ggpubr::stat_pvalue_manual(stat.test, label = "p.adj.signif")


      output[[g]] <- list(plot = p1, stats = stat.test)
    }
    return(output)
  }
