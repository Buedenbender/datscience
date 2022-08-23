#' Conduct a comparison for independent sample means - either t-test or one-way ANOVA
#' @description  A convenience function, that provides and easy to use wrapper for a step-by-step comparison of means
#' from independent groups. The steps that are run for each analysis are:
#' \itemize{
#' \item \code{0. Determination of groups} For 2 groups conduct independent sample t-test. For more than 2 groups an
#' ANOVA is conducted instead
#' \item \code{1. Detect outliers and extreme values} including boxplots to identify those values. Utilizes \link[rstatix]{identify_outliers}
#' \item \code{2. Calculate descriptive statistics} for each groups, self-explanatory.
#' \item \code{3. Check normal distribution (assumptions)} uses a formal test (Shapiro-Wilk) as well as a visual inspection method (qqplots).
#' Carefully judge this step, as the function has no default handling for non-normality. Options include check the literature if the test in your scenario is
#' robust for violations (see also central limit theorem and Glass, 1972), or switch to a non-parametric alternative.
#' \item \code{4. Check homogeneity of variances (assumption)} for this purpose a levene test will be conducted. A significant (by convention p < .05) result indicates
#' the H0 (of equal variances) must be disregarded. Depending on the result the student t-test with pooled variance / "normal" ANOVA
#' will be used. If there is heterogeneity of variances (i.e., unequal variances) the Welch correction of the degrees of freedom will be used instead.
#' \item \code{5. The test (t-Test or ANOVA)} will be conducted, including Post-Hoc Comparisons and a calculation of an effect size.
#' Additionally the ggpubr package is used together with rstatix to display a Boxplot including the result of the hypothesis test.
#' }
#' This function is a mere wrapper for the excellent tutorials provided by Alboukadel Kassambara over at Datanovia. See the original
#' \href{https://www.datanovia.com/en/lessons/t-test-assumptions/independent-t-test-assumptions/}{Guide for the t-test}
#' \href{https://www.datanovia.com/en/lessons/anova-in-r/}{and for the ANOVA}. It just packages all the steps in one function for
#' convenience
#' @param data The dataset containing the variables for the table1 call (all terms from the str_formula must be present)
#' @param dv The name of the dependend variable as character.
#' @param iv The name of the independend variable as character.
#' @param alternative In case of only two groups one can specify if a directed hypothesis should be tested. Default is "two.sided"
#' @param stepwise Boolean, default = TRUE, if TRUE the analysis is carried out in small steps, after each step (e.g. test for normality),
#' the output is printed is to the console and a user input is required. For a list of the steps see the description
#' @param verbose Boolean, default = TRUE, if TRUE the output of each step is printed to the console
#' @param add Additions to the boxplot, see also \link[ggpubr]{ggboxplot}. I primarily recommend to use either "none" or "jitter"
#' @param fill False for no filled colors
#' @param palette Color palette for the boxplot, see also \link[ggpubr]{ggboxplot}.
#' @param ... (Optional), Additional arguments that can be passed to \code{\link[rstatix]{t_test}}
#' (e.g., fontsize, font ...), to \code{\link{serialNext}} or to \code{\link[ggpubr]{ggboxplot}}
#' @return A list with all results of the check for the assumptions as well as the hypothesis test itself.
#'
#' @author Bjoern Buedenbender (adapted from Alboukadel Kassambara)
#' @examples
#' t_test <- independent_sample_means(mtcars, dv = "disp", iv = "vs", add = "none")
#' ANOVA <- independent_sample_means(mtcars, dv = "disp", iv = "gear", add = "none")
#' @export
#' @importFrom stats na.omit as.formula var.test t.test chisq.test fisher.test oneway.test aov
#' @importFrom dplyr tibble mutate select group_by
#' @importFrom forcats fct_drop as_factor
#' @importFrom tidyselect all_of
#' @importFrom rstatix identify_outliers shapiro_test get_summary_stats levene_test t_test add_significance cohens_d add_xy_position get_test_label anova_test tukey_hsd get_pwc_label
#' @importFrom ggpubr ggboxplot set_palette ggqqplot stat_pvalue_manual
#' @seealso
#' \code{\link[ggpubr]{ggboxplot}}
#' \code{\link[rstatix]{t_test}}
#' \href{https://www.datanovia.com/en/lessons/t-test-assumptions/independent-t-test-assumptions/}{Guide for the t-test}
#' \href{https://www.datanovia.com/en/lessons/anova-in-r/}{and for the ANOVA}

independent_sample_means <- function(data, dv, iv, alternative = c("two.sided", "less", "greater"),
                                     stepwise = TRUE, verbose = TRUE,
                                     add = "jitter", fill = iv, palette = "jco", ...) {

  # Validate correct inputs
  if (missing(data)) stop("Need to specify the mandatory argument \"data\"")
  if (missing(dv)) stop("Need to specify the mandatory argument \"dv\"")
  if (missing(iv)) stop("Need to specify the mandatory argument \"iv\"")

  if (!is(data, "data.frame")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"data\" is required to be a data.frame (incl. tibble)"
    ))
  }
  # Check Dependent Var: Character, Length = 1
  if (!is(dv, "character")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"dv\" is required to be a character"
    ))
  }
  if (length(dv)!=1) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"dv\" can not handle character vectors"
    ))
  }
  # Check Independent Var: Character, Length = 1
  if (!is(iv, "character")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"iv\" is required to be a character"
    ))
  }
  if (length(iv)!=1) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"dv\" can not handle character vectors"
    ))
  }
  # Check Alternative
  if (!is(alternative, "character")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"alternative\" is required to be a character"
    ))
  }
  if (missing(alternative)) alternative <- "two.sided"
  if (length(alternative)!=1) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"alternative\" can not handle character vectors"
    ))
  }
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop(paste(
      "Invalid argument type. The argument",
      "\"alternative\" please provide one of: \"two.sided\",",
      "\"less\", \"greater\""
    ))
  }

  # Adapted from Datanovia
  # https://www.datanovia.com/en/lessons/t-test-assumptions/independent-t-test-assumptions/
  # Prep of Data
  df <- data %>%
    dplyr::tibble() %>%
    dplyr::mutate(dplyr::across(all_of(iv), ~ forcats::fct_drop(forcats::as_factor(.)))) %>%
    dplyr::mutate(dplyr::across(all_of(dv), ~ as.numeric(as.character(.)))) %>%
    dplyr::select(all_of(c(iv, dv))) %>%
    stats::na.omit()

  # Construction formula object
  f <- stats::as.formula(paste(dv, "~", iv))

  # Number of groups k
  k <- length(levels(df[[iv]]))

  if (verbose) print(paste("Recognizing k =", as.character(k), "groups"))

  # Determine if Colors
  if (fill != iv & fill != TRUE) {
    fill <- "white"
  }


  # 1) EXTREME VALUES
  if (verbose) print("1) Checking for extreme values")

  outlier <- df %>%
    dplyr::group_by((!!as.symbol(iv))) %>%
    rstatix::identify_outliers((!!as.symbol(dv)))

  bxp <- ggpubr::ggboxplot(
    df,
    x = iv, y = dv,
    ylab = paste(dv, "(DV)"), xlab = paste(iv, "(IV)"), add = add,
    fill = fill, ...
  )

  if (fill != "white") bxp <- ggpubr::set_palette(bxp, palette)

  if (verbose) print(outlier)
  if (verbose) print(bxp)
  if (stepwise) trash <- readline("2) Continue with Descriptives?")

  # 2) DESCRIPTIVES
  sum_stats <- df %>%
    dplyr::group_by((!!as.symbol(iv))) %>%
    rstatix::get_summary_stats(type = "mean_sd")


  if (verbose) print(sum_stats)
  if (stepwise) trash <- readline("3) Continue with Normality Check?")

  # 3) NORMALITY
  sw_test <- df %>%
    dplyr::group_by((!!as.symbol(iv))) %>%
    rstatix::shapiro_test((!!as.symbol(dv)))

  qqplot <- ggpubr::ggqqplot(df, x = dv, facet.by = iv)


  if (verbose) print(sw_test)
  if (verbose) print(qqplot)
  if (stepwise) trash <- readline("4) Continue with check Homogeneity of Variances?")

  # 4) HOMOGENITY
  levene_test <- rstatix::levene_test(df, f)


  if (verbose) print(levene_test)
  if (stepwise) trash <- readline("5) Continue with the Test, Effect Size and Final Plot?")

  # 5) HYPOTHESIS TEST
  if (k == 2) { # T-Test / Welch in Case of 2 Groups
    stat.test <- df %>%
      rstatix::t_test(f,
        var.equal = (levene_test$p > .05),
        alternative = alternative, ...
      ) %>%
      rstatix::add_significance()

    # EFFECTSIZE
    es <- rstatix::cohens_d(df, f, var.equal = (levene_test$p > .05))


    # FINAL PLOT
    stat.test <- stat.test %>% rstatix::add_xy_position(x = iv)
    bxp_final <- bxp +
      ggpubr::stat_pvalue_manual(stat.test, tip.length = 0) +
      labs(subtitle = rstatix::get_test_label(stat.test, detailed = TRUE))

    pwc <- NA

    if (verbose) print(stat.test)
    if (verbose) print(es)
  } else {
    stat.test <- rstatix::anova_test(df, f, effect.size = "pes")
    pwc <- rstatix::tukey_hsd(df, f)

    # Visualization: box plots with p-values
    pwc <- pwc %>% rstatix::add_xy_position(x = iv)
    bxp_final <- bxp +
      ggpubr::stat_pvalue_manual(pwc, hide.ns = TRUE) +
      labs(
        subtitle = rstatix::get_test_label(stat.test, detailed = TRUE),
        caption = rstatix::get_pwc_label(pwc)
      )
    es <- NA # TODO add Effect Size estimate for ANOVA
    if (verbose) print(stat.test)
    if (verbose) print(pwc)
  }


  if (verbose) print(bxp_final)

  # 6) GENERATING FINAL OUTPUT
  ls <- list(
    "outlier" = outlier,
    "bxp" = bxp,
    "sum_stats" = sum_stats,
    "sw_test" = sw_test,
    "qqplot" = qqplot,
    "levene_test" = levene_test,
    "stat.test" = stat.test,
    "pwc" = pwc,
    "effect_size" = es,
    "final_plot" = bxp_final
  )

  return(ls)
}
