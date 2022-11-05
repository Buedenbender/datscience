########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)

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
#' give full credit to all packages / package authors, you can create this table
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
    appendix_packages <- appendix_packages |>  dplyr::add_row(
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
  suppressWarnings(knitr::write_bib(file = bib_file))

  ### Create the Table
  # Check if APA 7th is desired & create it
  raw_table <- flextable::flextable(appendix_packages |>
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
