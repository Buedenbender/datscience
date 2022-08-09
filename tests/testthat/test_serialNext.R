# Testing Inputs ----------------------------------------------------------
test_that("serialNext stops with an error for invalid argument types", {
  # verifying that mandatory arguments are present
  expect_error(serialNext(), "Need to specify the mandatory argument")

  # verifying the correct dtypes are supplied
  expect_error(serialNext(path=123), "Invalid argument type")
  expect_error(serialNext(path="test/", n_digits = "18"), "Invalid argument type")
  expect_error(serialNext(path="test/", maxruns = "21"), "Invalid argument type")
})


# Testing Outputs ---------------------------------------------------------
test_that("serialNext returns the correct output", {
    # Returned as character
    expect_identical(is(serialNext(path="test/"),"character"),TRUE)
})


