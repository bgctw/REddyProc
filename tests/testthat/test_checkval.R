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

test_that("fKeepColumnAttributes",{
  df1 <- Example_DETha98
  # subset rows
  df2 <- fKeepColumnAttributes(df1, function(x) filter(x, between(.data$DoY, 1,5)))
  expect_equal(sapply(df2, attributes), sapply(df1, attributes))
  # delete one column
  df2 <- fKeepColumnAttributes(df1, function(x) select(x, !"DoY"))
  expect_equal(sapply(df2, attributes), sapply(df1[names(df2)], attributes))
  # add one column
  df2 <- fKeepColumnAttributes(df1, function(x) mutate(x, newcol = 100*.data$DoY))
  expect_equal(sapply(df2[names(df1)], attributes), sapply(df1, attributes))
})
