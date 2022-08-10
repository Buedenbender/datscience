
# Testing Inputs ----------------------------------------------------------
test_that("serialNext stops with an error for invalid argument types", {
  # verifying that mandatory arguments are present
  expect_error(independent_sample_means(), "Need to specify the mandatory argument")
  expect_error(independent_sample_means(mtcars), "Need to specify the mandatory argument")
  expect_error(independent_sample_means(mtcars, dv = "disp"), "Need to specify the mandatory argument")
  expect_error(independent_sample_means(mtcars, iv = "vs"), "Need to specify the mandatory argument")

  # verifying the correct dtypes are supplied
  inappropriate_types <- list(21,c("vector","is","not","allowed"))
  for (dtype in inappropriate_types) {
    # Testing data argument
    expect_error(
      independent_sample_means(
        data = dtype, dv = "disp", iv = "vs",
        verbose = F, stepwise = F
      ),
      "Invalid argument type"
    )
    # Testing dv argument
    expect_error(
      independent_sample_means(
        data = mtcars, dv = dtype, iv = "vs",
        verbose = F, stepwise = F
      ),
      "Invalid argument type"
    )
    # Testing iv argument
    expect_error(
      independent_sample_means(
        data = mtcars, dv = "disp", iv = dtype,
        verbose = F, stepwise = F
      ),
      "Invalid argument type"
    )
    # Testing alternative argument
    expect_error(
      independent_sample_means(
        data = mtcars, dv = "disp", iv = "vs",
        alternative = dtype,
        verbose = F, stepwise = F
      ),
      "Invalid argument type"
    )
  }

  #   - Testing data argument, character supplied
  expect_error(
    independent_sample_means(
      data = "data.frame", dv = "disp", iv = "vs",
      verbose = F, stepwise = F
    ),
    "Invalid argument type"
  )


  # expect_error(serialNext(path=123), "Invalid argument type")
  # expect_error(serialNext(path="test/", n_digits = "18"), "Invalid argument type")
  # expect_error(serialNext(path="test/", maxruns = "21"), "Invalid argument type")
})

# # Testing Output ----------------------------------------------------------
# test_that("independent_sample_means does not correct if F is passed", {
#   expect_identical(any(grepl("Reported p-values are (Sidark|Bonferroni|.*) corrected",
#                              bivariate_corrected$footer)), TRUE)
#   expect_identical(any(grepl("Reported p-values are (Sidark|Bonferroni|.*) corrected",
#                              multiple_comp_corrected$footer)), TRUE)
# })
#
# test_that("independent_sample_means inherits from flextable", {
#   expect_s3_class(bivariate, "flextable")
#   expect_s3_class(multiple_comp, "flextable")
# })
#
# test_that("independent_sample_means does not correct if F is passed", {
#   expect_identical(any(grepl("Bonf",bivariate$footer)), FALSE)
#   expect_identical(any(grepl("Bonf",multiple_comp$footer)), FALSE)
# })
