
#' Rename specific columns of a data.frame
#'
#' Replace the column names of a data.frame entrywise for each element
#' a given vector (old) with the element of another vector (new)
#'
#' @param df data.frame.
#' @param old Character vector.
#' @param new Character vector.
#'
#' @return df data.frame
#'
#' @examples
#' iris.renamed <- cols_rename(iris, "Sepal.Length", "Sepal Length")
#' iris.renamed2 <- cols_rename(
#'   iris,
#'   c("Sepal.Length", "Sepal.Width"),
#'   c("SLength", "SWidth")
#' )
#' @export
cols_rename <- function(df, old, new) {
  if (!is.null(new)) {
    for (i in seq_along(old)) {
      names(df)[names(df) == old[i]] <- new[i]
    }
  }
  return(df)
}


#' Searches Column Names Starting with an [Reg]Expression
#'
#' Prints out all Cols that start with a given string
#' no need to at .. to the regex. Helpful for very long data.frames
#'
#' @param df data.frame.
#' @param regex Character vector.
#'
#'
#' @examples
#' colstartsw(iris, "Sepal")
#' @export
colstartsw <- function(regex = "", df) {
  s_phrase <- paste0("^", regex, "..")
  print(names(df)[grepl(s_phrase, names(df))])
}
