test_that("spss_swap stops with an error for invalid argument types", {
  # Creating a test flextable
  expect_error(spss_swap(mtcars), "Invalid argument type")
})
