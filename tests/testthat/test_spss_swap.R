# Testing the Input -------------------------------------------------------
test_that("spss_swap stops with an error for invalid argument types", {
  expect_warning(spss_swap(mtcars), "Invalid argument type")
  expect_error(spss_swap(),"Need to specify the mandatory argument")
})


# Testing Output ----------------------------------------------------------
# to be written

