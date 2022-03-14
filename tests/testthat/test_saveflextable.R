test_that("save_flextable throws a warnings given incorrect formats for arguments", {
  # Creating a test flextable
  ft <- flextable::flextable(data.frame(A=c(1:10)))
  expect_warning(save_flextable(ft,"abcde.xls"), "The given file format is not")
})


test_that("save_flextable stops with an error for invalid argument types", {
  # Creating a test flextable
  ft <- flextable::flextable(data.frame(A=c(1:10)))
  expect_error(save_flextable(ft,"test.docx",overwrite="asd"), "Invalid argument type")
  expect_error(save_flextable(ft,filepath=123), "Invalid argument type")
  expect_error(save_flextable("WOW","test.docx",overwrite="asd"), "Invalid argument type")
})
