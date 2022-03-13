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
#' @param adjust_p Character vector.
#' @param ylimits Numeric vector.
#'
#'
#' @examples
#' boxplot_t_test(mtcars, c("mpg", "hp"), group = "am")
#' @return List(Plot and stats)
#'
#' @export
#' @import dplyr
#' @import rstatix
#' @import ggplot2
#' @importFrom ggpubr ggboxplot
#' @importFrom tidyr pivot_longer
#' @importFrom ggpubr theme_pubr
#' @importFrom magrittr "%>%"
boxplot_t_test <-
  function(df,
           dependentvars,
           group,
           adjust_p = "BH",
           ylimits = c(0, 150)) {
    variables <- NULL
    df_s <- df %>%
      dplyr::select(dependentvars, group) %>%
      dplyr::as_tibble()

    # Pivot Table
    df_p <- df_s %>%
      tidyr::pivot_longer(-all_of(group),
        names_to = "variables", values_to =
          "value"
      )

    # Calc T Test
    stat.test <- df_p %>%
      dplyr::group_by(`variables`) %>%
      rstatix::t_test(stats::as.formula(paste("value ~", group))) %>%
      rstatix::adjust_pvalue(method = adjust_p) %>%
      rstatix::add_significance()
    # stat.test

    # Creating the Plot
    p1 <-
      ggpubr::ggboxplot(
        df_p,
        x = group,
        y = "value",
        fill = group,
        palette = "npg",
        legend = "none",
        ggtheme = ggpubr::theme_pubr(border = TRUE)
      ) +
      # ylim(ylimits)+
      ggplot2::facet_wrap(~variables)

    stat.test <- stat.test %>% rstatix::add_xy_position(x = group)
    p1 <-
      p1 + ggpubr::stat_pvalue_manual(stat.test, label = "p.adj.signif")
    return(list(plot = p1, stats = stat.test))
  }
