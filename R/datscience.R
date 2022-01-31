########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# General todos
# TODO: - Sort, e.g., all functions required for factoranalyses
# TODO: - INclude a Vignette for all Factor analysis function
# TODO:
#################### Basic Functions / Stand Alone ######################
# Setting up global exports to fix RMD Check problems for unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")



#' Determine Number of Decimals by a Simple Convention (Based on N)
#'
#' @description
#' A simple \href{https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics}{convention}
#' for the determination of the appropriate number of decimals
#' will be applied, e.g.,  \emph{n}
#' \itemize{
#' \item < 100: no decimal
#' \item < 1000: 1 decimals
#' \item > 1001: 2 decimals
#' }
#'
#' @param n Number of observations (e.g., participants in a study)
#'
#' @return Integer the appropriate number of decimals, based on sample size (n)
#'
#' @author Bjoern Buedenbender
#'
#' @examples
#' get_number_of_decimals(n = 153)
#' @export
get_number_of_decimals <- function(n) {
  if (n < 100) {
    nod <- 0
  } else if (n < 1000) {
    nod <- 1
  } else {
    nod <- 2
  }
  return(nod)
}


#' Check if file exists, else append a integer number
#'
#' @description
#' In cases you want to write to a file which you do not want to override if existing.
#' This helper function checks if a file exists, if it does it just appends an integer to the filename
#' Source originally by user spacedman \href{https://stackoverflow.com/a/25429755/7318488}{on stackoverflow}.
#' @param path Character containing the path with fileextensions
#' @param n_digits  (Optional) Integer, number of leadings 0 before the filename. Default is n_digits = 3
#' @param maxruns (Optional) Integer, Default is maxruns = 500.
#' @param ... (Optional) Additional Parameters, not utiliezd in this function, enables passing from passing of previous functions
#' @return Character, a filename that currently does not exists, with indices.
#'
#' @author spacedman stackoverflow (advanced by Bjoern Buedenbender)
#'
#' @importFrom xfun file_ext
#' @importFrom stringr str_replace str_pad
#' @importFrom tools file_path_sans_ext
#' @export
serialNext <- function(path,
                       n_digits = 3,
                       maxruns = 500,
                       ...) {
  # Currently only supports ignore file extension case
  # Check if file exists, if not just return filename
  if (!file.exists(path)) {
    return(path)
  }

  # Extract file extensions
  extension <- xfun::file_ext(path)
  # Extract filename
  filename <- tools::file_path_sans_ext(path)
  # Initialize Counter
  i <- 1

  # Append 3 digit integer number with leading 0
  repeat {
    f <- paste0(paste(filename,
      stringr::str_pad(i, n_digits, pad = "0"),
      sep = "_"
    ), ".", extension)
    if (!file.exists(f)) {
      return(f)
    } else if (i > maxruns) {
      print(paste0("All file names up until: ", basename(f), " are already taken."))
      print("Make sure this is intended. To solve this, you can either:")
      print(paste(" - increase maxruns arguments above:", maxruns))
      print(paste(" - Change (e.g., increase) the n_digits above:", n_digits))

      break
    }
    i <- i + 1
  }
}

#' Format a Flextable in Accordance with APA 7th Standards
#'
#' @description This function takes a flextable object, and applies APA 7th
#' standard style formatting to it. \cr
#' The initial idea for this function stems from
#' \href{https://remi-theriault.com/blog_table.html}{Remi Theriault's Blog}. \cr
#' Reference on what APA style for tables constitutes can be obtained on the
#' \href{https://apastyle.apa.org/style-grammar-guidelines/tables-figures/tables}{apastyle.apa.org website}.
#' @param ft A flextable object, to be formatted in accordance with APA. Required!
#' @param font Default is "Times New Roman", can be changed to your needs (e.g., "Arial")
#' @param fontsize Default is 12, bigger font size is not recommended.
#' @param table_caption Takes a character vector. Recommend are the following elements
#' \itemize{
#' \item Table + number (e.g., "\strong{Table 1}")
#' \item The description, use APA capital case
#' (e.g., "\emph{Sociodemographic Characteristics of the Total Sample}")
#' }
#' @param table_note Takes a character vector. Default is NA (no table note). Every value is a new line, e.g., is c("Note. Explanation of Abbreviations", "* p < .05. ** p < .01. *** p < .001").
#' Please note that you will need to manually set the formatting (i.e., setting italic for p values).
#' @param ... (Optional) Additional Parameters, not utiliezd in this function, enables passing from passing of previous functions
#'
#' @return An APA style formatted flextable object.
#'
#' @author Bjoern Buedenbender / Remi Theriault
#'
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom flextable flextable theme_booktabs hline_top hline_bottom hline_bottom fontsize font height set_table_properties add_header_lines add_footer_lines bold italic
format_flextable <- function(ft, font = "Times New Roman", fontsize = 12,
                             table_caption = c("Table x", "Some Description of the Table"),
                             table_note = NA,
                             ...) {
  nice.borders <- list("width" = 1, color = "black", style = "solid")
  formatted_ft <- ft %>%
    flextable::theme_booktabs() %>%
    flextable::hline_top(part = "head", border = nice.borders) %>%
    flextable::hline_bottom(part = "head", border = nice.borders) %>%
    flextable::hline_top(part = "body", border = nice.borders) %>%
    flextable::hline_bottom(part = "body", border = nice.borders) %>%
    # align(align = "center", part = "all") %>%
    # line_spacing(space = 1.5, part = "all") %>%
    flextable::height(height = 0.55, part = "body") %>%
    # hrule(rule = "exact", part = "all") %>%
    flextable::height(height = 0.55, part = "head") %>%
    flextable::set_table_properties(layout = "autofit")
  # If provided, add table caption
  if ((length(table_caption) == 1 && !is.na(table_caption)) ||
    length(table_caption) > 1) {
    formatted_ft <- formatted_ft %>%
      flextable::add_header_lines(values = rev(table_caption)) %>%
      flextable::bold(part = "header", i = 1) %>%
      flextable::italic(part = "header", i = 2)
  }
  # If provdied, add table note
  if (!is.na(table_note)) {
    formatted_ft <- formatted_ft %>%
      flextable::add_footer_lines(values = table_note)
  }
  formatted_ft <- formatted_ft %>%
    flextable::fontsize(part = "all", size = fontsize) %>%
    flextable::font(part = "all", fontname = font)


  return(formatted_ft)
}


#' Saves a Given Flextable Object
#'
#' @description
#' Takes a flextable object (ft) and a filepath (path + filename + extension, e.g., "results/table1.docx")
#' And saves it. Primarily a utility function used by others in the package. Gives the option not to overwrite a
#' file by utilizing \code{\link{serialNext}} see \code{?datscience::serialNext} for more deatils
#' @param ft A flextable object, to be formatted in accordance with APA. Required!
#' @param filepath Path and filename were the flextable object should be saved, options include the common filetypes
#' .docx (Word), .pptx (Powerpoint), .html (Webpage)
#' @param overwrite (Optional) Boolean, default is FALSE. When overwrite is
#' FALSE and the files already exists, a serialized version of the filename
#' will be created (i.e., appending _001, or _002). Utilizes \code{\link{serialNext}}
#' @author Bjoern Buedenbender
#'
#' @export
#' @importFrom xfun file_ext
#' @importFrom flextable save_as_docx save_as_pptx save_as_html
#'
#' @seealso \code{\link{serialNext}}
save_flextable <- function(ft, filepath, overwrite = FALSE) {
  #### Check if directory exists, if not create it
  if (!file.exists(dirname(filepath))) {
    dir.create(dirname(filepath), recursive = TRUE)
  }

  ### If Overwrite is FALSE, serialize file path with indices
  if (!overwrite) filepath <- serialNext(filepath)
  ### Save flextable
  # Get file type
  filetype <- xfun::file_ext(filepath)
  # Save depending on filetype
  switch(filetype,
    docx = {
      flextable::save_as_docx(ft, path = filepath)
    },
    pptx = {
      flextable::save_as_pptx(ft, path = filepath)
    },
    html = {
      flextable::save_as_html(ft, path = filepath)
    },
    {
      print("The given filetable is not supported by the package")
      print("Try using .docx, .pptx or .html")
    }
  )
}

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

#' Creates APA Tables in Rmarkdown Templates
#' @description
#' Given a data.frame creates an APA Table utilizing knitr::kable
#' This is also possible in Templates for RMarkdown
#'
#' @param df data.frame.
#'
#'
#' @examples
#' my_apa(iris)
#' @return Table
#'
#' @export
#' @importFrom kableExtra kable_styling row_spec column_spec
#' @import knitr xtable
my_apa <- function(df) {
  kableExtra::kable(df,
    format = "html",
    algin = "1",
    booktabs = TRUE
  ) %>%
    kableExtra::kable_styling(full_width = TRUE, position = "left") %>%
    kableExtra::row_spec(0,
      extra_css =
        "border-top:1.5px solid black; border-bottom:1.5px solid black;"
    ) %>%
    kableExtra::row_spec(nrow(df),
      extra_css = "border-bottom:1.5px solid black;"
    ) %>%
    kableExtra::row_spec(0:nrow(df),
      align = "c",
      background = "#FFFFFF"
    ) %>%
    kableExtra::column_spec(1,
      extra_css = "text-align: left;"
    ) %>%
    kableExtra::column_spec(seq_along(df),
      extra_css = "border-right:0;border-top:0;"
    )
}

########################## Advanced Functions ############################

#' Corstars - Correaltions in Console
#' @description  Creates a pretty console correlation table (by Dominik Vogel)
#' method : correlation method. "pearson", "spearman" and "polychoric" are currently
#' supported the results will be displayed directly in console. There is the option
#' to save them in html or latex format or (recommended), to transform them to a
#' \code{\link[flextable]{flextable}} and export it directly to word.
#' labels_rows and labels_cols are character vectors for labeling rows and columns.
#' \href{https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R}{Reference for the original code}.
#' Additionally added the option to investigate polychoric correlation
#' @param x a matrix containing the data
#' @param method correlation method. "pearson", "spearman" or "polychoric" are supported
#' @param removeTriangle remove upper or lower triangle, or FALSE for not removing any triangle
#' @param rmDiag if one triangle of the matrix is removed, should the diagonal be kept = FALSE; or removed = TRUE
#' @param rmLastCol chose if the last column can be removed, to shorten the table if necessary, default = TRUE
#' @param result Print result in Console ("none"), generate HTML file ("html"), generate latex file ("latex")
#' @param labels_rows Labels for the rows (i.e., variable names). Length musst be same as number of variables
#' @param labels_cols Labels for columns. Length musst be same as number of variables - 1
#' @param sig.level Significance level (.1 or .05). If NA is provided, no stars marking the significance will be printed.
#' This helps formatting the decimal places. NA is especially used by the \code{\link{apa_corrTable}} function
#' @param nod Integer. Number of Decimals. Default is nod = 2. In case of -1 a simple convention based
#' on sample size is applied for determination of number of decimal points.
#' See \code{\link{get_number_of_decimals}} or \code{?datscience::get_number_of_decimals}
#' @param caption Caption for the table
#' @param filename File name to save output to
#'
#' @seealso \code{\link{get_number_of_decimals}}
#'
#' @return Correlation table in console or file
#'
#' @author Dominik Vogel (Adapted by Bjoern Buedenbender)
#'
#'
#' @examples
#' \dontrun{
#' # Console output
#' corstars(mtcars,
#'   method = "pearson", removeTriangle = "upper", result = "none",
#'   caption = "Correlations",
#'   sig.level = 0.1,
#'   labels_rows = c(
#'     "(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'     "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'     "(9) am", "(10) gear",
#'     "(11) carb"
#'   ),
#'   labels_cols = 1:10
#' )
#'
#' # HTML output
#' corstars(mtcars,
#'   method = "pearson", removeTriangle = "upper", result = "html",
#'   caption = "Correlations", filename = "corr.html",
#'   sig.level = 0.1,
#'   labels_rows = c(
#'     "(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'     "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'     "(9) am", "(10) gear",
#'     "(11) carb"
#'   ),
#'   labels_cols = 1:10
#' )
#' }
#' @export
#' @import xtable
#' @importFrom Hmisc rcorr
corstars <- function(x,
                     method = c("pearson", "spearman", "polychoric"),
                     removeTriangle = c("upper", "lower", FALSE),
                     rmDiag = c(TRUE, FALSE),
                     rmLastCol = c(TRUE, FALSE),
                     result = c("none", "html", "latex"),
                     labels_rows = colnames(x),
                     labels_cols = labels_rows[1:length(labels_rows)],
                     sig.level = 0.05,
                     nod = 2,
                     caption = c("Correlation"),
                     filename = "") {
  ### TODO: Add overwrite parameters

  # Requesting Namespace
  requireNamespace("Hmisc", quietly = TRUE)
  requireNamespace("xtable", quietly = TRUE)

  # Determine number of decimals, if convention is desired (-1)
  if (nod == -1) nod <- get_number_of_decimals(nrow(x))
  # stopifnot(length(labels_rows) == ncol(x))
  # stopifnot(length(labels_cols) == ncol(x))

  # Prepare data.frame
  x <- as.matrix(x)

  # Prevent warning for no method provided, from vector as input to method
  if (length(method) > 1) {
    method <- method[1]
    # Set sig.level to NA in case of polychoric
    if (method == "polychoric") {
      sig.level = NA
    }
  }

  # Compute correlation matrix
  if (method != "polychoric") {
    correlation_matrix <- Hmisc::rcorr(x, type = method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value

    ## Define notions for significance levels; spacing is important.
    # ifelse(sig.level == 0.1,
    #        mystars <- ifelse(p < .01,"**", ifelse(p < .05, "* ", "  ")),
    #        ifelse(sig.level == 0.05,
    #               mystars <- ifelse(p < .05, "*", " "),""))
    if (!is.na(sig.level)) {
      if (sig.level == .001) {
        mystars <-
          ifelse(p < .001, "*", " ")
      } else if (sig.level == .01) {
        mystars <-
          ifelse(p < .001, "**",
            ifelse(p < .01, "* ", "  ")
          )
      } else {
        mystars <-
          ifelse(p < .001, "*** ",
            ifelse(p < .01, "**  ",
              ifelse(p < .05, "*   ", "    ")
            )
          )
      }
    }
  } else if (method == "polychoric") {
    correlation_matrix <- psych::polychoric(x, global = FALSE, correct = 0)
    R <- correlation_matrix$rho
  } else {
    stop("Please provide a correct method for correlation analysis, chose between either:
        - method = \"pearson\",
        - method = \"spearman\",
        - method = \"polychoric\"")
  }

  ## trunctuate the correlation matrix to two decimal
  if (!is.na(sig.level)) {
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), nod))[, -1]
  }


  ## build a new matrix that includes the correlations with their appropriate stars
  if (method != "polychoric" && !is.na(sig.level)) {
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep = "")
  } else {
    Rnew <- round(R, nod)
  }
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")
  ## remove upper triangle of correlation matrix
  if (removeTriangle[1] == "upper") {
    Rnew <- as.matrix(Rnew)
    if (!is.na(sig.level)) {
      Rnew[upper.tri(Rnew, diag = rmDiag[1])] <- ""
    } else {
      Rnew[upper.tri(Rnew, diag = rmDiag[1])] <- NA
    }
    Rnew <- as.data.frame(Rnew)
    ## remove lower triangle of correlation matrix
  } else if (removeTriangle[1] == "lower") {
    Rnew <- as.matrix(Rnew)
    if (!is.na(sig.level)) {
      Rnew[lower.tri(Rnew, diag = rmDiag[1])] <- ""
    } else {
      Rnew[lower.tri(Rnew, diag = rmDiag[1])] <- NA
    }
    Rnew <- as.data.frame(Rnew)
  } else {
    Rnew <- as.data.frame(as.matrix(Rnew))
  }

  ## remove last column and return the correlation matrix
  if (rmLastCol[1]) {
    Rnew <- cbind(Rnew[1:length(Rnew) - 1])
    colnames(Rnew) <- labels_cols[1:length(labels_cols) - 1]
  }
  rownames(Rnew) <- labels_rows

  if (result[1] == "none") {
    return(Rnew)
  } else {
    if (result[1] == "html") {
      print(xtable::xtable(Rnew, caption = caption),
        type = "html",
        file = filename
      )
    } else {
      print(xtable::xtable(Rnew, caption = caption),
        type = "latex"
      )
    }
  }
}

#' R Package Citations
#' @description
#' Function that allows the creation of a full APA-style table with all citations and informations
#' on the R-packages utilized. \cr
#' The table will contain the following columns (Packagename | Version | Maintainer | Citation) \cr
#' The function writes two files (in a given directory)
#' \itemize{
#' \item The APA style table as \emph{.docx} (default, or as \emph{.csv} if desired)
#' \item A \emph{.bib} file for the correct citations.
#' }
#' The citation column needs to be filled manually by importing the \emph{.bib}
#' file in the reference manager
#' of your choice (e.g., mendeley, endnote, ...) and pasting the citation in the
#' respective column \cr
#' \strong{Note} I highly recommend to cite the main packages used for your
#' analysis in the methods / analyses section of your manuscript. However to
#' give full credit to all packages / package authors, you can created this table
#' and reference it in the appendix. This also increases reproduciblity, as every
#' dependency to run your script becomes transparent.
#' @param outdirectory (Optional) Character vector for the output directory
#' (for the two files, .bib and .csv). Default is "Appendix/" in the current
#' working directory (see with getwd())
#' @param filename (Optional) Character vector. Custom name for the formatted
#' APA-style table, that should end with .docx. A docx file will be provided.
#' If NA, a csv file of the table will be saved.
#' @param overwrite (Optional) Boolean, default is FALSE. When overwrite is
#' FALSE and the files already exists, a serialized version of the filename
#' will be created (i.e., appending _001, or _002). Utilizes \code{\link{serialNext}}
#' @param table_caption Takes a character vector. For reference see see \code{\link{format_flextable}}.\cr
#' Recommend are the following elements
#' \itemize{
#' \item Table + number (e.g., "\strong{Table 1}")
#' \item The description, use APA capital case
#' (e.g., "\emph{Sociodemographic Characteristics of the Total Sample}")
#' }
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...)
#' @return  Creates (respective returns, depending on the arguments specified) the following: \cr
#' \itemize{
#' \item Creates the directory Appendix/ (if no other outcomedirectory is specified)
#' \item In the "Appendix/" dir a \emph{.bib} file is created for the citation
#' \item Either creates an APA 7th style table in a \emph{.docx} or a plain \emph{.csv} table
#' \item Additionally returns the \code{flextable::flextable} object with the APA 7th style table.
#' }
#' @author Bjoern Buedenbender
#'
#'
#' @export
#' @importFrom utils write.csv maintainer packageVersion
#' @importFrom xfun file_ext
#' @importFrom dplyr add_row arrange
#' @importFrom knitr write_bib
#' @importFrom magrittr "%>%"
#' @importFrom pacman p_loaded
#' @importFrom flextable flextable
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Not executed example, as it creates unnecessary files in the package
#' Rcitation_appendix()
#' }
#'
#' @seealso \code{\link[flextable]{flextable}}, \code{\link{serialNext}}, \code{\link{format_flextable}}
Rcitation_appendix <- function(outdirectory = "Appendix",
                               filename = "Appendix - R Packages.docx",
                               overwrite = FALSE,
                               table_caption = c("Table A1", "All R-Packages Utilized and Dependencies"),
                               ...) {

  #### Create a Table with all Important Information
  # Empty container data.frame
  appendix_packages <- data.frame(
    Packagename = character(),
    Version = character(),
    Maintainer = character(),
    Citation = character()
  )
  # Iterate over loadead packages, and append information
  for (pkg in pacman::p_loaded()) {
    appendix_packages <- appendix_packages %>% dplyr::add_row(
      Packagename = pkg,
      Version = as.character(utils::packageVersion(pkg)),
      Maintainer = utils::maintainer(pkg),
      Citation = ""
    )
  }

  ### Check if the directory exists, if not it will be created.
  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }
  # Remove the ending path separator: e.g., /
  if (substrRight(outdirectory, 1) == "/") {
    outdirectory <- substr(outdirectory, 1, nchar(outdirectory) - 1)
  }

  if (!file.exists(outdirectory)) {
    dir.create(outdirectory)
  }
  ### PATH PREPARATION
  # Concate to full filepath
  bib_file <- paste0(outdirectory, "/Bibliography Packages.bib")
  if (is.na(filename)) filename <- "Appendix - R Packages.csv"
  appendix_table <- paste0(outdirectory, "/", filename)
  # Check if the files already exists
  if (!overwrite) {
    bib_file <- serialNext(bib_file)
    appendix_table <- serialNext(appendix_table)
  } else {
    print("Overwrite was set to TRUE.")
    print("Existing files will be replaced, i.e., overwritten with the new version ...")
  }

  # #### Write the Bibliography with all Citations for the Packages
  knitr::write_bib(file = bib_file)

  ### Create the Table
  # Check if APA 7th is desired & create it
  raw_table <- flextable::flextable(appendix_packages %>%
    dplyr::arrange(.data$Packagename))
  output_table <- format_flextable(
    ft = raw_table,
    table_caption = table_caption,
    ...
  )

  ### Table: Write file or return flextable object
  # Extract filetype
  filetype <- xfun::file_ext(appendix_table)
  # Depending on filetype
  if (filetype == "docx") {
    save_flextable(
      ft = output_table, filepath = appendix_table,
      overwrite = overwrite
    )
  } else if (filetype == "csv") {
    utils::write.csv(
      x = appendix_packages,
      file = appendix_table,
      row.names = FALSE
    )
  }
  return(output_table)
}


#' Pretty Scree
#' @description
#' Creates an Screeplot Including a Parallel Analysis (Horn), formatted according to APA
#' 7th style. The original code was developed by John Sakaluk
#' Check the original source: \href{https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/#psych}{at his wordpress blog}.
#' @param parallel an parallel object returned by \code{psych::fa.parallel}
#' @param fa either "pc" or "fa" factor methods are allowed for the parallel analysis
#' @param quant default = .95 the quantile of the simulated values used to plot
#'
#' @return APA Ready Plot of Parallel Analyssis
#'
#' @author John Sakaluk (Wrapped in a Function by Bjoern Buedenbender)
#'
#'
#' @export
#' @import ggplot2
#' @seealso \code{\link[psych]{fa.parallel}}

pretty_scree <- function(parallel, fa, quant = .95) {
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
  p <- ggplot2::ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
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
    ggplot2::scale_shape_manual(values = c(16, 1)) +
    # Add vertical line indicating parallel analysis suggested max # of factors to retain
    ggplot2::geom_vline(xintercept = noi, linetype = "dashed") +
    # Apply our apa-formatting theme
    apatheme
  # Call the plot. Looks pretty!
  return(p)
}


#' Booted Eigenvalues
#' @description
#' Uses \code{\link[boot]{boot}}  to bootstrap Eigenvalues in factor analyses. \cr
#' Eigenvalues can be extracted for PCA as well as EFA \cr
#' Different Factor Methods are available for more details, see \code{\link[psych]{fa}}
#' Allows for extraction of Eigenvalues based on Pearson
#' as well as polychoric correlation matrices. These bootstrapped Eigenvalues can
#' serve as a foundation to construct a scree plot with confidence intervals around
#' the observed eigenvalues.
#'
#' @param df A data.frame
#' @param iterations number of resamples for the bootstrap
#' @param cor either "pearson" or "poly" for polychoric correlations,
#'  defaults to "pearson"
#' @param fa either "pc" for prinicipal component or
#' "fa" for [common] factor analysis, defaults to "pc"
#' @param fm factor method to use, irrelevant for pca.
#' For available factor methods check psych::fa for more details, defaults to minres
#'
#' @return A boot object (\code{boot::boot()}),
#' that contains SE for all Eigenvalues in DF, can be passed to \code{\link{getCIs}}  to create Confidence Intervals
#'
#' @author Bjoern Buedenbender
#'
#' @export
#' @importFrom boot boot
#' @importFrom psych fa polychoric
#' @seealso \code{\link[boot]{boot}}, \code{\link[psych]{fa}}, \cr
#' \code{\link{getCIs}} \code{\link{add_ci_2plot}}
booted_eigenvalues <-
  function(df,
           iterations = 1000,
           cor = "pearson",
           fa = "pc",
           fm = "minres") {
    eigenvalues_extractor <- function(d, i, cor, fa, fm) {
      d2 <- d[i, ]
      if (cor == "pearson") {
        rx <- cor(d2, use = "pairwise")
        nobs <- NA
      } else if (cor == "poly") {
        poly_cor <- psych::polychoric(d2, correct = FALSE)
        rx <- poly_cor$rho
        nobs <- poly_cor$n.obs
      }

      if (fa == "pc") {
        res <- eigen(rx)$values
      } else if (fa == "fa") {
        res <-
          psych::fa(
            rx,
            nfactors = 1,
            rotate = "none",
            fm = fm,
            warnings = FALSE,
            n.obs = nobs
          )$values
      }
      return(res)
    }

    boot_obj <- boot::boot(
      d = df,
      # Helper Function to Extract Eigenvalues
      eigenvalues_extractor,
      # Number of Resamples
      R = iterations,
      # Parameters for the Eigenvalue Extraction
      fa = fa,
      fm = fm,
      cor = cor
    )
    return(boot_obj)
  }


#' Get Boot Strapped CIs
#'
#' @description
#' Determines the CI for a vector of statistics / multiple stats.
#' As \code{\link[boot]{boot.ci}} only returns CI for the first statistic in a boot_object
#' getCIs (given a \code{\link[boot]{boot}} object), creates CI for all statistics.
#' Code by Ben Bolker, Check the
#' original Source: \href{https://stackoverflow.com/a/31818160/7318488}{@@stackoverflow}
#'
#' @param boot_obj boot::boot object
#'
#' @return A data.frame with Confidence intervalls for
#' all statistics, as well as the observed value
#'
#' @author Ben Bolker
#'
#' @export
#' @importFrom boot boot.ci
#' @importFrom utils tail
#'
#' @seealso \code{\link[boot]{boot.ci}}, \code{\link[boot]{boot}}

getCIs <- function(boot_obj) {
  getCI <- function(x, w) {
    b1 <- boot::boot.ci(x, index = w)
    ## extract info for all CI types
    tab <- t(sapply(b1[-(1:3)], function(x) {
      tail(c(x), 2)
    }))
    ## combine with metadata: CI method, index
    tab <- cbind(w, rownames(tab), as.data.frame(tab), x$t0[w])
    colnames(tab) <- c("index", "method", "lwr", "upr", "observed")
    tab
  }
  return(do.call(rbind, lapply(c(1:ncol(boot_obj$t)), getCI, x = boot_obj)))
}


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


#' Create APA Publication Ready Correlation Table and Export to Word or Powerpoint
#'
#' @description
#' The Idea was inspired by a blog post of my colleague Remi Theriault
#' (see \href{https://remi-theriault.com/blog_table.html}{remi-theriault.com})
#' Which utilized the ability of \code{\link[flextable]{flextable}} to be able to get an
#' APA-style formatted table directly from R into .docx (word). One frequent use case
#' is to get a correlation table into word. For the creation of the correlation table
#' this function uses the \code{\link{corstars}} function from this package, which
#' resolves around the code by
#' \href{https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R}{Dominik Vogel}.\cr
#' I built on those idea and created a function that creates a pretty correlation table
#' together with the summary stats of your choice, and returns a flextable, which
#' can be easily exported to word (see also  \code{\link{save_flextable}})\cr
#' \strong{Please note:} This function only considers numeric variables (cols). Other
#' datatypes will be dropped from the data.frame df.
#'
#' @param df Data.frame, mandatory argument. Consider filtering before passing it
#' e.g. with dplyr::select() and dplyr::filter()
#' @param summarystats A vector with the summary stats to be included at the bottom
#' below the correlation. Default is c("mean","sd")
#' Options are one or all of c("mean","sd","median","range","min","max","skew",
#' "kurtosis","se","missing"). The option missing, adds missings per item as additional row
#' (accecpts both "missing" and "missings", spelling in table accordingly).
#' If NA is given, no summarystats will be added.
#' @param method Type of correlation. Options are currently: "pearson", "spearman" and "polychoric"
#' @param rmDiag Should the diagonal in the corr matrix kept (FALSE) or removed (TRUE)
#' @param sig.level How many stars per level of significance, options include
#' .05 .01 or .001. If NA no stars indicating significance will be output. This improves
#' formatting of decimals in the table. Note the default for polychoric is NA
#' @param nod (Optional) Integer or Integer Vector. Number of Decimals.
#' In case of -1 a simple convention based on sample size is applied for determination
#' of number of decimal points. See ?datscience::get_number_of_decimals.
#' You can also provide an Integer vector, if you want different number of decimals
#' for the correlations and the summary stats. The first integer determines nod for
#' correlations, the second for summary stats. E.g., c(2,-1) would give 2 decimals
#' for correlations and apply the convention for summary stats. Default is nod = c(2,-1). \cr
#' See \code{\link{get_number_of_decimals}} or\code{?datscience::get_number_of_decimals}
#' @param filepath (Optional) Path and filename were the APA ready table should
#' be saved, options include the common filetypes .docx (Word), .pptx (Powerpoint),
#' .html (Webpage). Default is filepath = NA. If NA is given, no file will be saved.
#' @param overwrite (Optional) Boolean, default is FALSE. When overwrite is
#' FALSE and the files already exists, a serialized version of the filename
#' will be created (i.e., appending _001, or _002). Utilizes \code{\link{serialNext}}
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...) or to \code{\link{serialNext}}
#'
#' @return A \code{\link[flextable]{flextable}} object with APA ready correlation table.
#'
#' @author Bjoern Buedenbender (Inspired by Remi Theriault)
#'
#' @examples
#' apa_corrTable(mtcars, table_caption = c("Table 2", "Correlations in the mtcars Data Set"))
#' @export
#' @importFrom stats na.omit
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select summarise across everything
#' @importFrom stringr str_to_title
#' @importFrom psych describe
#' @importFrom tibble rownames_to_column
#' @importFrom flextable flextable
#'
#' @seealso \code{\link{get_number_of_decimals}}, \code{\link{format_flextable}},
#' \code{\link{serialNext}}
apa_corrTable <- function(df,
                          summarystats = c("mean", "sd"),
                          method = c("pearson", "spearman", "polychoric"),
                          rmDiag = FALSE,
                          sig.level = 0.05,
                          nod = c(2, -1),
                          filepath = NA,
                          overwrite = FALSE,
                          ...) {
  # Exclude non-numeric cols
  df <- df %>% dplyr::select(where(is.numeric))

  # TODO: Add problematic correlations (as summarystat?, ferketisch)
  # TODO: add NA for sig.level if no stars are supposed to be added

  ### Determine number of decimals (nod)
  # Separate nod for correlations and summary stats?
  if (length(nod) == 1) {
    # Apply convention for number of decimals (nod) if -1
    if (nod == -1) nod <- get_number_of_decimals(nrow(df))
    nod_cor <- nod
    nod_sum <- nod
  } else if (length(nod) == 2) {
    if (nod[1] == -1) nod_cor <- get_number_of_decimals(nrow(df)) else nod_cor <- nod[1]
    if (nod[2] == -1) nod_sum <- get_number_of_decimals(nrow(df)) else nod_sum <- nod[2]
  }

  # Creating Correlation table
  corstars(df,
    rmLastCol = FALSE,
    rmDiag = rmDiag,
    method = method[1],
    sig.level = sig.level,
    nod = nod_cor
  ) -> correlations

  ### Getting Descriptives
  # Determine which summarystats are requested
  psych_sumstats <- c(
    "mean", "sd", "median", "range", "min", "max", "skew",
    "kurtosis", "se"
  )
  for (stat in summarystats) {
    if (any(stat %in% psych_sumstats)) {
      correlations[stringr::str_to_title(stat), ] <- round(psych::describe(df)[[stat]], nod_sum)
    } else if (stat == "missing" || stat == "missings") {
      correlations[stringr::str_to_title(stat), ] <- df %>%
        dplyr::summarise(dplyr::across(
          dplyr::everything(),
          ~ sum(is.na(.))
        ))
    }
  }

  ### Flextable
  flextable::flextable(correlations %>%
    tibble::rownames_to_column(" ")) -> corr_table

  # Formatting of the table by datscience::format_flextable(), inspired by Remi Theriault
  corr_table <- format_flextable(ft = corr_table, ...)

  # Saving the Table
  # Check if a correct file path is provided
  if (!is.na(filepath) && filepath != "") {
    save_flextable(ft = corr_table, filepath = filepath, overwrite = overwrite)
  }

  return(corr_table)
}


#' Create an APA Formatted Table of Factor Loadings
#' @description Extracts factor loadings from factor analyses (either PCA or EFA) conducthed
#' with the \href{https://cran.r-project.org/web/packages/psychTools/vignettes/factor.pdf}{psych package},
#' and returns a APA 7th formatted table with the factor loadings and additional metrics (Communality, Uniqueness, Complexity).
#' If desired this table can directly be exported to a Word file (.docx).
#' @param fa_object Either an \code{\link[psych]{fa}} or \code{\link[psych]{principal}}, from
#' which the factor loadings are to be extracted
#' @param filepath (Optional) Path and filename were the APA ready table should
#' be saved, options include the common filetypes .docx (Word), .pptx (Powerpoint),
#' .html (Webpage). Default is filepath = NA. If NA is given, no file will be saved.
#' @param table_caption Takes a character vector. Recommend are the following elements
#' \itemize{
#' \item Table + number (e.g., "\strong{Table 3}")
#' \item The description, use APA capital case
#' (e.g., "\emph{PCA / EFA Factor Loadings}")
#' }
#' @param nod (Optional) Integer or Integer Vector. Number of Decimals.
#' In case of -1 a simple convention based on sample size is applied for determination
#' of number of decimal points. See ?datscience::get_number_of_decimals.
#' You can also provide an Integer vector, if you want different number of decimals
#' for the correlations and the summary stats. The first integer determines nod for
#' factor loadings, the second for additional stats. E.g., c(2,-1) would give 2 decimals
#' for factor loadings and apply the convention for Complexity, Uniqueness and Communality.
#' Default is nod = c(3,2). \cr
#' See \code{\link{get_number_of_decimals}} or\code{?datscience::get_number_of_decimals}
#' @param overwrite (Optional) Boolean, default is FALSE. When overwrite is
#' FALSE and the files already exists, a serialized version of the filename
#' will be created (i.e., appending _001, or _002). Utilizes \code{\link{serialNext}}
#' @param n.obs (Optional), Only required if you want to apply the convention to number
#' of decimals, and did calculate the factor analysis (PCA, EFA) directly on a correlation
#' or covariance matrix.
#' @param cutoff (Optional), Integer, determines the minimum threshold, values below are omitted.
#' Default is cutoff = 0.3.
#' @param ... (Optional), Additional arguments that can be passed to \code{\link{format_flextable}}
#' (e.g., fontsize, font ...) or to \code{\link{serialNext}}
#'
#' @return A \code{\link[flextable]{flextable}} object with APA ready correlation table. If a filepath is provided
#' it also creates the respective file (e.g., a word .docx file)
#'
#' @author Bjoern Buedenbender
#' @examples
#' datscience::apa_factorLoadings(psych::fa(mtcars, nfactors = 2))
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom psych fa.sort
#' @importFrom dplyr mutate across everything if_else bind_cols
#' @importFrom tibble rownames_to_column
#' @importFrom flextable flextable
#' @seealso \code{\link{get_number_of_decimals}}, \code{\link{format_flextable}},
#' \code{\link{serialNext}}
apa_factorLoadings <- function(fa_object, filepath = NA,
                               table_caption = c(
                                 "Table X",
                                 "PCA / EFA Factor Loadings"
                               ),
                               overwrite = FALSE,
                               nod = c(3, 2),
                               n.obs = NA,
                               cutoff = 0.3,
                               ...) {

  ### Determine number of decimals (nod)
  # Separate nod for factor loadings and additional stats?

  # Check if n is available
  if (!is.na(fa_object$n.obs)) n <- fa_object$n.obs else n <- n.obs

  if (length(nod) == 1) {
    # Check if n is available
    # Apply convention for number of decimals (nod) if -1
    if (nod == -1 && !is.na(n)) {
      nod <- get_number_of_decimals(n)
    }
    nod_loadings <- nod
    nod_additional <- nod
  } else if (length(nod) == 2) {
    if (nod[1] == -1 && !is.na(n)) nod_loadings <- get_number_of_decimals(n) else nod_loadings <- nod[1]
    if (nod[2] == -1 && !is.na(n)) nod_additional <- get_number_of_decimals(n) else nod_additional <- nod[2]
  } else {
    print("There was a problem with the desired number of decimals (nod). If you
          wanted to apply the convention, either nod = -1 or nod = c(-1,-1) make sure
          that you provide the number of observations (n.obs)")
  }

  # Create Table / Extract Factor Loadings
  pc_loadings <- fa_object %>%
    psych::fa.sort() %>%
    .[["loadings"]] %>%
    round(nod_loadings) %>%
    unclass() %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else((. < cutoff), "", as.character(.))
    )) %>%
    dplyr::bind_cols(
      Communality = fa_object$communality,
      Uniqueness = fa_object$uniquenesses,
      Complexity = fa_object$complexity
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, nod_additional)) %>%
    tibble::rownames_to_column("item")

  # Create APA Flextable
  ft <- format_flextable(flextable::flextable(pc_loadings),
    table_caption = table_caption,
    ...
  )

  # Saving the Table
  # Check if a correct file path is provided
  if (!is.na(filepath) && filepath != "") {
    save_flextable(ft = ft, filepath = filepath, overwrite = overwrite)
  }

  return(ft)
}
