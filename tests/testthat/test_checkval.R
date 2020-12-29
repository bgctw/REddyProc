#require(testthat)
context("checkval")

df <- data.frame(id = c("a1","a2","a3"), numval = c(4,NA,8))

test_that("fCheckColNum good case",{
  REddyProc:::fCheckColNum(df, "numval", "test_fCheckColNum")
})

test_that("fCheckColNum bad case",{
  dfb <- df; dfb$numval[3] <- "stringval"
  expect_error(
    REddyProc:::fCheckColNum(dfb, "numval", "test_fCheckColNum"),
    "'stringval'"
  )
})

test_that("fCheckColNum testing non-existing column",{
  expect_warning(
    REddyProc:::fCheckColNum(df, c("col1","col2"), "test_fCheckColNum"),
    "col1,col2"
  )
})

test_that("fCheckColNum none dummy column",{
  # columne 'none' is not tested
  REddyProc:::fCheckColNum(df, c("numval","none"), "test_fCheckColNum")
})

test_that("fCheckColNum zero-length vector",{
  REddyProc:::fCheckColNum(df, c(), "test_fCheckColNum")
})

