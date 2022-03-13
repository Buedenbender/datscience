########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)

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
