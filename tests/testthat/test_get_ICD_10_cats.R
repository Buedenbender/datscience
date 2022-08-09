# Testing Inputs ----------------------------------------------------------
test_that("get_ICD_10_cats stops with an error for invalid argument types", {
  # Creating a test flextable
  expect_error(get_ICD_10_cats(123), "Invalid argument type")
})


# Testing Outputs ---------------------------------------------------------
test_that("get_ICD_10_cats returns the correct output", {
  # Test for both languages and erroneous input to lang
  for(lang in c("eng","de","nonsense")){
    # All 11 categories in the F-Chapter of the ICD 10
    expect_identical(length(get_ICD_10_cats(lang)), 11L)
    # Returned as vectors
    expect_identical(is(get_ICD_10_cats(lang),"vector"),TRUE)
  }
})


