########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)

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
