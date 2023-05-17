test_that("forgot() works", {
  forgot_ex1 <- forgot::forgot("stringr")

  expect_s3_class(forgot_ex1, "tbl_df")

  expect_true("function_name" %in% names(forgot_ex1) )

  expect_true(nrow(forgot_ex1)>1)
})

test_that("forgot2() works", {
  forgot2_ex1 <- forgot::forgot2("stringr")
  expect_s3_class(forgot2_ex1, "tbl_df")
  expect_true(
    all(
      names(forgot2_ex1) %in% c("function_name", "title")
    )
  )
})

test_that("forgot_fx() works", {
  forgot_fx_ex1 <- forgot::forgot_fx("dplyr", "count", "usage", print = F)
  expect_s3_class(forgot_fx_ex1, "tbl_df")
  expect_true(
    names(forgot_fx_ex1) %in% c("usage")
  )
  forgot_fx_ex2 <- forgot::forgot_fx("dplyr", "count", print = F)
  expect_s3_class(forgot_fx_ex2, "tbl_df")
  expect_true(
    all(
      names(forgot_fx_ex2) %in% names(
        forgot::forgot("dplyr", "count")
      )
    )
  )
})
