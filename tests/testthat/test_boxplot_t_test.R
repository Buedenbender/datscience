test_that("boxplot_t_test stops with an error for invalid argument types", {
  # Creating a test flextable
  expect_error(boxplot_t_test(sample(100,1), c("mpg", "hp"), group = "am"), "Invalid argument type")
  expect_error(boxplot_t_test(mtcars, sample(100,1), group = "am"), "Invalid argument type")
  expect_error(boxplot_t_test(mtcars, "mpg", group = sample(100,1)), "Invalid argument type")
})
