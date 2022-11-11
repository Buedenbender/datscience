
# Simulating Data for the Tests -------------------------------------------
# Creating bivariate dataset
biv_str_formula <- "~ Sepal.Length + Sepal.Width +test | Species"
biv_data <- dplyr::filter(iris, Species %in% c("setosa", "versicolor"))
biv_data$test <- factor(rep(c("Female", "Male"), 50))
biv_table_caption <- c("Table 1", "A test on the Iris Data")
bivariate <- flex_table1(biv_str_formula, data = biv_data,
                         table_caption = biv_table_caption)

bivariate_nocorrection <- flex_table1(biv_str_formula, data = biv_data,
                         table_caption = biv_table_caption,correct = NA)

bivariate_corrected <- flex_table1(biv_str_formula, data = biv_data,
                         table_caption = biv_table_caption,correct = "sida")

# Creating Multivariate  Groups (ANOVA)
mult_str_formula <- "~ Sepal.Length + Sepal.Width + Gender_example | Species"
mult_data <- dplyr::filter(iris, Species %in% c("setosa", "versicolor"))
mult_data <- iris
mult_data$Gender_example <- factor(rep(c("Female", "Male"), nrow(mult_data)/2))
mult_table_caption <- c("Table 2", "A test on the Iris Data")
multiple_comp <- flex_table1(mult_str_formula, data = mult_data,
                             table_caption = mult_table_caption)
multiple_comp_corrected <- flex_table1(mult_str_formula, data = mult_data,
                             table_caption = mult_table_caption,
                             correct = "bonf")




# Testing Section ---------------------------------------------------------
test_that("flex_table1 inherits from flextable", {
  expect_s3_class(bivariate, "flextable")
  expect_s3_class(multiple_comp, "flextable")
})

test_that("flex_table1 does not correct if F is passed", {
  expect_identical(any(grepl("Bonf",bivariate$footer)), FALSE)
  expect_identical(any(grepl("Bonf",multiple_comp$footer)), FALSE)
})

test_that("flex_table1 does not correct if F is passed", {
  expect_identical(any(grepl("Reported p-values are (Sidark|Bonferroni|.*) corrected",
                             bivariate_corrected$footer)), TRUE)
  expect_identical(any(grepl("Reported p-values are (Sidark|Bonferroni|.*) corrected",
                             multiple_comp_corrected$footer)), TRUE)
})

test_that("flex_table1 warns or stops when formula is not specified correctly",{
  expect_warning(flex_table1(gsub("~","",biv_str_formula),
                             data = biv_data,
  ))
  expect_error(flex_table1(gsub("\\|","",biv_str_formula),
                             data = biv_data,
  ))
})

# Testing PR
fun.function <- function() {
  print("This is a test!! :)")
}
fun.function()
