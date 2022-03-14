test_that("get_number_of_decimals returns an positive integer (natural number)
when given a positive integer (natural number, e.g., sample size)", {
  expect_equal(get_number_of_decimals(sample.int(99,1)), 0)
  expect_equal(get_number_of_decimals(sample(c(101:999),1)), 1)
  expect_equal(get_number_of_decimals(sample(c(1001:9999),1)), 2)
})
