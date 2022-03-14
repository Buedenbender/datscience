test_that("spss_swap stops with an error for invalid argument types", {
  # Creating a test flextable
  expect_error(boxplot_t_test(123, c("mpg", "hp"), group = "am"), "Invalid argument type")
})
