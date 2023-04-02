test_that("forgot() works", {
  forgot_ex1 <- forgot::forgot("stringr")

  expect_s3_class(forgot_ex1, "tbl_df")

  expect_true("function_name" %in% names(forgot_ex1) )

  expect_true(nrow(forgot_ex1)>1)
})
