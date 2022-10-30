########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
# Setting up global exports to fix RMD Check problems for
# unexportet namespaces (e.g. where())
# Work around due to package building trouble
#' @importFrom utils globalVariables
utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' Format a Flextable in Accordance with APA 7th Standards
#'
#' @description This function takes a flextable object, and applies APA 7th
#' standard style formatting to it. \cr
#' The initial idea for this function stems from
#' \href{https://remi-theriault.com/blog_table.html}{Remi Theriault's Blog}. \cr
#' Reference on what APA style for tables constitutes can be obtained on the
#' \href{https://apastyle.apa.org/style-grammar-guidelines/tables-figures/tables}{apastyle.apa.org website}.
#' See also the very neat packages \code{\link[flextable]{flextable}} and \code{\link[officer]{read_docx}},
#' by David Gohel and Colleagues, which enables us to get our publication ready tables directly
#' into MS Word.
#' @param ft A flextable object (prefered) or data.frame, to be formatted in accordance with APA. Required!
#' @param font Default is "Times New Roman", can be changed to your needs (e.g., "Arial")
#' @param fontsize Default is 12, bigger font size is not recommended.
#' @param table_caption Takes a character vector. Note: when provided NA no caption will
#' be placed! Recommend are the following elements
#' \itemize{
#' \item Table + number (e.g., "\strong{Table 1}")
#' \item The description, use APA capital case
#' (e.g., "\emph{Sociodemographic Characteristics of the Total Sample}")
#' }
#' @param table_note Takes a character vector. Will automatically prepend Note if not present.
#' Default is NA (no table note). e.g., is c("Explanation of Abbreviations",
#' "* p < .05. ** p < .01. *** p < .001").
#' Please note that you will need to manually set the formatting (i.e., setting italic for p values).
#' @param italize_stats Boolean, default is TRUE. Searches the header for common statistic symbols (e.g. p N SD M) and
#' sets font to italic
#' @param linespacing Points of Linespacing defaults to 1.25, APA would be 1.5 or 2
#' @param ... (Optional) Additional Parameters, not utiliezd in this function, enables passing from passing of previous functions
#'
#' @return An APA style formatted flextable object.
#'
#' @author Bjoern Buedenbender / Remi Theriault
#'
#' @export
#' @importFrom magrittr "%>%"
#' @import flextable
#' @seealso
#' \code{\link[flextable]{flextable}}
format_flextable <- function(ft, font = "Times New Roman", fontsize = 12,
                             table_caption = c("Table x", "Some Description of the Table"),
                             table_note = NA,
                             italize_stats = TRUE,
                             linespacing = 1.25,
                             ...) {
  # Construct Borders
  apa.border <- list("width" = 1, color = "black", style = "solid")
  invis.borders <- list("width" = 0, color = "black", style = "solid")

  # If not a flextable try to convert to one
  if (!is(ft, "flextable")) {
    tryCatch(
      ft <- flextable::flextable(ft),
      error = function(c) stop("Error could not convert to a flextable object!")
    )
  }

  # Create Flextable
  formatted_ft <- ft %>%
    flextable::theme_booktabs() %>%
    flextable::hline_top(., part = "head", border = apa.border) %>%
    flextable::hline_bottom(., part = "head", border = apa.border) %>%
    flextable::hline_top(., part = "body", border = apa.border) %>%
    flextable::hline_bottom(., part = "body", border = apa.border) %>%
    flextable::align(., align = "center", part = "head") %>%
    flextable::height(., height = 0.55, part = "body") %>%
    # hrule(rule = "exact", part = "all") %>%
    flextable::height(., height = 0.55, part = "head") %>%
    flextable::set_table_properties(., layout = "autofit")
  # If provided, add table caption
  if (is(table_caption, "character")) {
    formatted_ft <- formatted_ft %>%
      flextable::add_header_lines(., values = rev(table_caption)) %>%
      flextable::bold(., part = "header", i = 1) %>%
      flextable::italic(., part = "header", i = c(2:length(table_caption))) %>%
      flextable::align(., part = "header", i = c(1:length(table_caption)), align = "left") %>%
      flextable::border(., part = "head", i = c(1:length(table_caption)), border = invis.borders)
  }

  # If provided, add table note
  if (!anyNA(table_note)) {
    formatted_ft <- formatted_ft %>%
      flextable::add_footer_lines(., values = "") %>%
      flextable::compose(., i = 1, j = 1, value = as_paragraph(as_i("Note. "), table_note), part = "footer")
  }

  formatted_ft <- formatted_ft %>%
    flextable::fontsize(., part = "all", size = fontsize) %>%
    flextable::font(., part = "all", fontname = font) %>%
    flextable::line_spacing(., space = linespacing, part = "all")


  return(formatted_ft)
}
