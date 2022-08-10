
# Testing Inputs ----------------------------------------------------------
test_that("pretty_cm stops with an error for invalid argument types", {
  # simulating data for tests
  set.seed(23)
  pred <- factor(sample(c("dog", "cat"), 100, replace = TRUE))
  ref <- factor(sample(c("dog", "cat"), 100, replace = TRUE))
  cm <- caret::confusionMatrix(pred, ref)

  # verifying that mandatory arguments are present
  expect_error(pretty_cm(), "Need to specify the mandatory argument")

    # verifying the correct dtypes are supplied
  inappropriate_types <- list(21,"character",c("vector","is","not","allowed"), TRUE)
  for (dtype in inappropriate_types) {
    # Testing cm argument
    expect_error(pretty_cm(dtype),"Invalid argument type")
    # Testing numeric arguments: midpoint
    if(!is(dtype,"numeric")){
      expect_error(pretty_cm(cm, midpoint = dtype),"Invalid argument type")
    }
    # Testing logical arguments: plot, hideZero
    if(!is(dtype,"logical")){
      expect_error(pretty_cm(cm, plot = dtype),"Invalid argument type")
      expect_error(pretty_cm(cm, hideZero = dtype),"Invalid argument type")
    }

  }



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
