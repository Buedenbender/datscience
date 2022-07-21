######################### Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

# Globalds Created with Table1 invisble to CMD Check
utils::globalVariables("p")
utils::globalVariables("Characteristic")
utils::globalVariables("table_caption")

# TODO: NOCH EINZUBAUEN https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
# TODO: Add support for grouping variables with > 2 levels e.g., ANOVA


#' Creates a Descriptive Bivariate Table1 for Publication (Table1 + Flextable)
#' @description A convenience function, that provides and easy wrapper for the two main enginges of the function
#' \itemize{
#' \item \code{\link[table1]{table1}} provides a nice API given a formula to create demographics tables.
#' I basically just advanced the functionality of the p-value function, added the possibility
#' to correct p-values with either Bonferroni or Sidark, and set some sensible defaults to achieve a nice look
#' \item \code{\link[flextable]{flextable}} which gives all the power to format the table as you please (e.g., conditional formatting ->
#' adding bold for p values below .05), adding italic headers or notes explaining what was done.
#' }
#' Really all credit should go to these two packages their developers, on full disclosure
#' my function just provides an easy to use API or wrapper around their packages to get
#' a beautiful publication ready bivariate comparison Table 1. New feature since version (0.2.3) comparisons with (Welch) ANOVA for more than
#' 2 Groups
#' @include utility_numberparse.R
#' @param str_formula A string representing a formula, e.g., \code{"~ Sepal.Length + Sepal.Width | Species"}
#' used to construct the \code{\link[table1]{table1}}.
#' @param data The dataset containing the variables for the table1 call (all terms from the str_formula must be present)
#' @param correct Character, currently available are "bonf" for Bonferroni correction or "sidark" for Sidark correction.
#' Provide NA if you dont want any correction. Defaults to "bonf"
#' @param num Integer number of comparisons. If NA will be determined automatically, by the number of terms in the formula
#' @param table_caption Caption for the table, each element of the vector represents
#' a new line. The first line will be bold face. All additional lines are in italic.
#' @param ref_correction Boolean, default = TRUE, if TRUE corrected p-Values will be referenced in the foot note.
#' @param include_teststat Boolean, default = TRUE, if TRUE includes two additional columns in the table.
#' 1) Test statistic (either t, f or X²) and 2) degrees of Freedom
#' @param drop_unused_cats Boolean, default = TRUE, if TRUE categories (i.e., factor levels) with 0 observations will be dropped.
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...) or to \code{\link{serialNext}}
#'
#' @return A \code{\link[flextable]{flextable}} object with APA ready correlation table. If a filepath is provided
#' it also creates the respective file (e.g., a word .docx file)
#'
#' @author Bjoern Buedenbender
#' @examples
#' \dontrun{
#' # Comparison of just two Groups
#' str_formula <- "~ Sepal.Length + Sepal.Width +test | Species"
#' data <- dplyr::filter(iris, Species %in% c("setosa", "versicolor"))
#' data$test <- factor(rep(c("Female", "Male"), 50))
#' table_caption <- c("Table 1", "A test on the Iris Data")
#' flex_table1(str_formula, data = data, table_caption = table_caption)
#'
#' # Comparison of Multiple Groups (ANOVA)
#' str_formula <- "~ Sepal.Length + Sepal.Width + Gender_example | Species"
#' data <- dplyr::filter(iris, Species %in% c("setosa", "versicolor"))
#' data <- iris
#' data$Gender_example <- factor(rep(c("Female", "Male"), nrow(data)/2))
#' table_caption <- c("Table 1", "A test on the Iris Data")
#' flex_table1(str_formula, data = data, table_caption = table_caption)
#' }
#' @export
#' @importFrom stats terms as.formula var.test t.test chisq.test fisher.test oneway.test aov
#' @importFrom table1 table1 stats.default stats.apply.rounding t1flex
#' @importFrom flextable bold compose as_paragraph as_chunk align
#' @importFrom rstatix levene_test
#' @importFrom forcats fct_drop
#' @seealso
#' \code{\link{format_flextable}},
#' \code{\link[flextable]{flextable}}
#' \code{\link[table1]{table1}}
flex_table1 <- function(str_formula,
                        data,
                        correct = "bonf",
                        num = NA,
                        table_caption = NA,
                        ref_correction = TRUE,
                        include_teststat = TRUE,
                        drop_unused_cats = TRUE,
                        ...) {


  # 1) Prepare Formula String -----------------------------------------------
  # Remove line breaks
  str_formula <- sub("\\\n","",str_formula)

  # Determine the number of Comparisons: Necessary for Correction of the p-value
  #   - Use the number of Terms in the formula
  if (is.na(num)) {
    # Remove the grouping variable | as this is not recognized correctly
    s_num <- sub("\\|.*", "", str_formula)
    num <- length(labels(stats::terms(as.formula(s_num))))
  }

  # Drop unused / empty categorie
  if(missing(drop_unused_cats) | drop_unused_cats){
    data <- data %>%
      dplyr::mutate(dplyr::across(where(is.factor), ~forcats::fct_drop(.)))
  }

  grp_var <- trimws(unlist(strsplit(str_formula, "\\|"))[2])
  n_grps <- unname(lengths(unique(data[grp_var])))

  ### Helper Functions for the table1
  # https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
  pvalue <- function(x, ...) {
    # Flaggs for append superscript letters for correction
    fisher <- FALSE
    levene <- FALSE

    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    # Determine number of groups
    n_grps <- length(levels(g))

    # Numerical Dependent Var
    if (is.numeric(y)) {
      # Determine Homogeneity of Variances Across groups
      homogeneity <- rstatix::levene_test(dv ~ grp, data = data.frame(dv = y, grp = g))$p > .05

      # Decide t-test or ANOVA
      #   - Albeit with Equal Variance the Results are the same
      #     as t² = F and p are identical

      # t-Test
      if (n_grps == 2) {
        # Either perform a standard t-Test or a Welch Test for Heterogeneity of variances
        p <- stats::t.test(y ~ g, var.equal = homogeneity)$p.value
      }
      # ANOVA
      else {
        if (homogeneity) { # "Normal" ANOVA for Homogenous Variances Between Groups
          ano <- stats::aov(y ~ g)
          p <- unname(unlist(summary(ano)))[9]
        } else {
          p <- stats::oneway.test(y ~ g)$p.value
        }
      }
    }
    # Categorical Dependent Var
    else {
      # For categorical variables, perform a chi-squared test of independence
      tmp <- suppressWarnings(stats::chisq.test(table(y, g)))

      # Or if one expected cell count is below 5a fishers exact test
      if (any(tmp$expected < 5)) {

        # Try to run a Fisher test w default workspace for comupting time advantage
        result <- tryCatch(
          {
            stats::fisher.test(table(y, g))
          },
          error = function(cond) {
            # If Fisher's Test fails due to small workspace, increase it
            if (grepl("^FEXACT error 7", cond$message)) {
              # Error due to small workspace for a reference see:
              # https://github.com/Lagkouvardos/Rhea/issues/17#issuecomment-442861341
              return(stats::fisher.test(table(y, g), workspace = 2e8))
            }
            # Choose a return value in case of error
            return(cond)
          }
        ) # END tryCatch

        p <- result$p.value
        fisher <- TRUE
      } else {
        p <- tmp$p.value
      }
    }

    # Bonferroni Correction
    if (!is.na(correct)) {
      if (correct == "bonf") p <- p * num
      if (correct == "sidark") p <- 1 - (1 - p)**num
    }

    # Due to Correction replace values bigger > 1 with 1
    if (p > 1) p <- 1

    # Format the p-value, using an UNICODE for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    out <- sub("<", "< ", sub("0.", ".", format.pval(round(p, 4), digits = 3, eps = .001)))
    if (ref_correction) {
      if (fisher) out <- paste(out, "\u0363")
      if (levene) out <- paste(out, "\u1D47")
    }
    out
  }


  # Function for test statistic
  teststat <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    # Determine number of groups
    n_grps <- length(levels(g))

    # Numerical Dependent Var
    if (is.numeric(y)) {
      # Determine Homogeneity of Variances Across groups
      homogeneity <- rstatix::levene_test(dv ~ grp, data = data.frame(dv = y, grp = g))$p > .05
      # t-Test
      if (n_grps == 2) {
        # Either perform a standard t-Test or a Welch Test for Heterogeneity of variances
        stat <- stats::t.test(y ~ g, var.equal = homogeneity)$statistic
      }
      # ANOVA
      else {
        if (homogeneity) { # "Normal" ANOVA for Homogenous Variances Between Groups
          res <- stats::aov(y ~ g)
          stat <- summary(res)[[1]][1,4]
        } else {
          res <- stats::oneway.test(y ~ g)
          stat <- res$statistic
        }
      }
    }
    # Categorical Dependent Var
    else {
      # For categorical variables, perform a chi-squared test of independence
      tmp <- suppressWarnings(stats::chisq.test(table(y, g)))

      # Or if one expected cell count is below 5a fishers exact test
      if (any(tmp$expected < 5)) {

        # Try to run a Fisher test w default workspace for comupting time advantage
        result <- tryCatch(
          {
            stats::fisher.test(table(y, g))
          },
          error = function(cond) {
            # If Fisher's Test fails due to small workspace, increase it
            if (grepl("^FEXACT error 7", cond$message)) {
              # Error due to small workspace for a reference see:
              # https://github.com/Lagkouvardos/Rhea/issues/17#issuecomment-442861341
              return(stats::fisher.test(table(y, g), workspace = 2e8))
            }
            # Choose a return value in case of error
            return(cond)
          }
        ) # END tryCatch

        stat <- ""
      } else {
        stat <- tmp$statistic
      }
    }

    if (is.character(stat)) stat else round(stat,2)
  }

  # Function for test statistic
  degreesfreedom <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    # Determine number of groups
    n_grps <- length(levels(g))

    # Numerical Dependent Var
    if (is.numeric(y)) {
      # Determine Homogeneity of Variances Across groups
      homogeneity <- rstatix::levene_test(dv ~ grp, data = data.frame(dv = y, grp = g))$p > .05
      # t-Test
      if (n_grps == 2) {
        # Either perform a standard t-Test or a Welch Test for Heterogeneity of variances
        df <- round(stats::t.test(y ~ g, var.equal = homogeneity)$parameter,2)
      }
      # ANOVA
      else {
        if (homogeneity) { # "Normal" ANOVA for Homogenous Variances Between Groups
          res <- stats::aov(y ~ g)
          df <- paste0(summary(res)[[1]][1,1],", ",summary(res)[[1]][2,1])
        } else {
          res <- stats::oneway.test(y ~ g)
          param <- round(res$parameter,2)
          df <- paste0(param[1],", ",param[2])
        }
      }
    }
    # Categorical Dependent Var
    else {
      # For categorical variables, perform a chi-squared test of independence
      tmp <- suppressWarnings(stats::chisq.test(table(y, g)))

      # Or if one expected cell count is below 5a fishers exact test
      if (any(tmp$expected < 5)) {

        # Try to run a Fisher test w default workspace for comupting time advantage
        result <- tryCatch(
          {
            stats::fisher.test(table(y, g))
          },
          error = function(cond) {
            # If Fisher's Test fails due to small workspace, increase it
            if (grepl("^FEXACT error 7", cond$message)) {
              # Error due to small workspace for a reference see:
              # https://github.com/Lagkouvardos/Rhea/issues/17#issuecomment-442861341
              return(stats::fisher.test(table(y, g), workspace = 2e8))
            }
            # Choose a return value in case of error
            return(cond)
          }
        ) # END tryCatch

        df <- ""
      } else {
        df <- tmp$parameter
      }
    }

    df
  }


  # Helper to format the output of Metric Vars
  my.render.cont <- function(x) {
    with(
      table1::stats.apply.rounding(table1::stats.default(x), digits = 2),
      c("Mean (SD)" = sprintf("%s (\u00B1 %s)", MEAN, SD))
    )
  }

  # Helper to format the output of Categorical Vars
  my.render.cat <- function(x) {
    c("", sapply(table1::stats.default(x), function(y) {
      with(
        y,
        sprintf("%d (%0.0f %%)", FREQ, PCT)
      )
    }))
  }

  # Convert string to formula
  form <- stats::as.formula(str_formula)

  # Number of Extra Columns
  if (include_teststat) {
    extra_col <- list(
      "t / X"=teststat,
      "df" = degreesfreedom,
      "p" = pvalue)
  }else extra_col <- list("p" = pvalue)

  tbl1 <- table1::table1(
    form,
    data = data,
    render.continuous = my.render.cont, render.categorical = my.render.cat,
    render.missing = NULL, droplevels = TRUE, overall = FALSE,
    extra.col = extra_col,
    rowlabelhead = "Characteristic"
  )

  note <- NA
  # Add a Note if p-values corrected
  # Determine if t-test or one way ANOVA was conducted
  type_test <- ifelse(n_grps == 2, "independent sample t-test", "one way ANOVA")
  if (ref_correction) {
    note <- paste(
      "Differences are determined by",
      type_test,
      "or Pearon's \u03C7\u00B2-test."
    )
  } else {
    note <- paste(
      "Differences determined by",
      type_test,
      "(Welch correction applied if necessary) or Pearon's \u03C7\u00B2-test",
      "(Fisher\'s test for expected counts \u2264 5)."
    )
  }

  if (is(correct, "character")) {
    note <- paste(
      note,
      "Reported p-values are",
      if (correct == "bonf") "Bonferroni" else "Sidark",
      "corrected."
    )
  }
  # t / \u03C7\u00B2

  ft <- table1::t1flex(tbl1) %>%
    datscience::format_flextable(
      table_note = note,
      table_caption = table_caption,
      ...
    ) %>%
    flextable::bold(., part = "body", j = 1, bold = FALSE) %>%
    flextable::bold(.,
                    i = ~ number_parse(p) < 0.05,
                    j = ~Characteristic,
                    bold = TRUE
    ) %>%
    flextable::compose(.,
                       part = "body",
                       j = ~Characteristic,
                       i = ~ number_parse(as.character(p)) < 0.05,
                       value = flextable::as_paragraph(
                         Characteristic,
                         flextable::as_chunk(ifelse(number_parse(p) <= 0.001,
                                                    " ***",
                                                    ifelse(number_parse(p) < 0.01, " **", " *")
                         ))
                       )
    ) %>%
    flextable::align(.,
                     i = if (anyNA(table_caption)) 1 else (length(table_caption) + 1),
                     part = "header", align = "center"
    ) %>%
    flextable::italic(., j = ~p, part = "header")

  # When Teststatistic included in the table
  if (include_teststat) {
    if (n_grps == 2) header_teststat <- "t / \u03C7\u00B2" else  header_teststat <- "F / \u03C7\u00B2"
    ft <- ft %>%
      # Correct heading for the teststatistic
      flextable::compose(.,
                         part = "header",
                         j = (n_grps + 2),
                         i = if (anyNA(table_caption)) 1 else (length(table_caption) + 1),
                         value = flextable::as_paragraph(header_teststat)) %>%
      # Make Degrees of Freedom italic
      flextable::italic(., j = ~df, part = "header")

  }

  # convert table 1 to data.frame for manual adjustments of the table
  df <- as.data.frame(tbl1)

  if (ref_correction) {

    # When Fishers Test was applied add note
    if (any(grepl("\u0363", df$p))) {
      ft <- ft %>%
        flextable::add_footer_lines(.,
                                    values = "\u0363 Fisher's exact test, expected cell-count \u2264 5."
        )
    }

    # When Welchs COrrection was applied
    if (any(grepl("\u1D47", df$p))) {
      ft <- ft %>%
        flextable::add_footer_lines(.,
                                    value = "\u1D47 Welch's correction for heterogeneity of variances.",
        )
    }
  }

  # Set the N in column header to italic
  #   - get all column names
  names <- df %>% names()
  #   - iterate over all groups
  for(j in c(2:(n_grps+1))){
    # Extract Name an N for the respective group
    group_name <- names[j]
    n <- number_parse(df[1, j])

    # Replace Header with cursive N
    ft <- ft %>%
      flextable::compose(.,
                         i = if (anyNA(table_caption)) 1 else (length(table_caption) + 1),
                         j = j,
                         part = "header",
                         # value = as_paragraph("(",flextable::as_b(N)," = ",as.character(49)")"))
                         value = as_paragraph(
                           group_name, "\n(",
                           flextable::as_i("N"),
                           " = ",
                           as.character(n), ")"
                         )
      )
  }

  return(ft)
}
