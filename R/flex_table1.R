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



#' Creates a Descriptive Bivariate Table1 for Pulbication (Table1 + Flextable)
#' @description A convenience function, that provides and easy wrapper for the two main enginges of the function
#' \itemize{
#' \item \code{\link[table1]{table1}} provides a nice API given a formula to create sociodemographic tables.
#' I basically just advanced the functionality of the p-value function, added the possiblity
#' to correct p-values with either Bonferroni or Sidark, and set some sensible defaults to achieve a nice look
#' \item \code{\link[flextable]{flextable}} which gives all the power to format the table as you please (e.g., conditional formatting ->
#' aadding bold for p values below .05), adding italic headers or notes explaining what was done.
#' }
#' Really all credit should go to these two packages their developers, on full disclosure
#' my function just provides an easy to use API or wrapper around their packages to get
#' a beautiful publication ready bivariate comparison Table 1.
#' @include utility_numberparse.R
#' @param str_formula A string representing a formula, e.g., \code{"~ Sepal.Length + Sepal.Width | Species"}
#' used to construct the \code{\link[table1]{table1}}.
#' @param data The dataset containing the variables for the table1 call (all terms from the str_formula must be present)
#' @param correct Character, currently available are "bonf" for Bonferroni correction or "sidark" for Sidark correction.
#' Provide NA if you dont want any correction. Defaults to "bonf"
#' @param num Integer number of comparisons. If NA will be determined automatically, by the number of terms in the formula
#' @param table_caption Caption for the table, each element of the vector represents
#' a new line. The first line will be bold face. All additional lines are in italic.
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...) or to \code{\link{serialNext}}
#'
#' @return A \code{\link[flextable]{flextable}} object with APA ready correlation table. If a filepath is provided
#' it also creates the respective file (e.g., a word .docx file)
#'
#' @author Bjoern Buedenbender
#' @examples
#' \dontrun{
#' str_formula <- "~ Sepal.Length + Sepal.Width +test | Species"
#' data <- dplyr::filter(iris, Species %in% c("setosa", "versicolor"))
#' data$test <- factor(rep(c("Female", "Male"), 50))
#' table_caption <- c("Table 1", "A test on the Iris Data")
#' flex_table1(str_formula, data = data, table_caption = table_caption)
#' }
#' @export
#' @importFrom stats terms as.formula var.test t.test chisq.test fisher.test
#' @importFrom table1 table1 stats.default stats.apply.rounding t1flex
#' @importFrom flextable bold compose as_paragraph as_chunk align
#' @seealso
#' \code{\link{format_flextable}},
#' \code{\link[flextable]{flextable}}
#' \code{\link[table1]{table1}}
flex_table1 <- function(str_formula,
                        data,
                        correct = "bonf",
                        num = NA,
                        table_caption = NA,
                        ...) {

  # Determine the number of Comparisons
  #   - Use the number of Terms in the formula
  if (is.na(num)) {
    # Remove the grouping variable | as this is not recognized correctly
    s_num <- sub("\\|.*", "", str_formula)
    num <- length(labels(stats::terms(as.formula(s_num))))
  }

  ### Helper Functions for the table1
  # https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
  pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    if (is.numeric(y)) {
      # For numeric check equality of variances
      levene <- stats::var.test(y ~ g)$p.value > .05
      # Either perform a standard t-Test or a Welch Test for Heterogeneity of variances
      p <- stats::t.test(y ~ g, var.equal = levene)$p.value
    } else {
      # For categorical variables, perform a chi-squared test of independence
      tmp <- stats::chisq.test(table(y, g))
      # Or if one expected cell count is below 5a fishers exact test
      if (any(tmp$expected < 5)) {
        p <- stats::fisher.test(table(y, g))$p.value
      } else {
        p <- tmp$p.value
      }
    }

    # Bonferroni Correction
    if (correct == "bonf") p <- p * num
    if (correct == "sidark") p <- 1 - (1 - p)**num

    # Due to Correction replace values bigger > 1 with 1
    if (p > 1) p <- 1

    # Format the p-value, using an UNICODE for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c(sub("<", "\u2264 ", format.pval(p, digits = 3, eps = .001)), "")
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


  tbl1 <- table1::table1(
    form,
    data = data,
    render.continuous = my.render.cont, render.categorical = my.render.cat,
    render.missing = NULL, droplevels = TRUE, overall = FALSE,
    extra.col = list(`p` = pvalue),
    rowlabelhead = "Characteristic"
  )

  note <- NA
  # Add a Note if p-values corrected
  note <- paste(
    "Note. Differences determined by independent sample t-test",
    "(Welch correction applied if necessary) or Pearon's \u03C7\u00B2-test",
    "(Fisher\'s test for expected counts \u2264 5)."
  )
  if (is(correct, "character")) {
    note <- paste(
      note,
      "Reported p-values are",
      if (correct == "bonf") "Bonferroni" else "Sidark",
      "corrected."
    )
  }
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
          "***",
          ifelse(number_parse(p) < 0.01, "**", "*")
        ))
      )
    ) %>%
    flextable::align(.,
      i = if (anyNA(table_caption)) 1 else (length(table_caption) + 1),
      part = "header", align = "center"
    )
  return(ft)
}
