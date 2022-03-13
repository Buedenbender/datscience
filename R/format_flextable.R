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
