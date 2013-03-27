#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fCheck... functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context("fCheckValue...")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("fCheckValString",{
  #allowed entries
  expect_true( fCheckValString("a") )
  expect_true( fCheckValString(NA_character_) )
  #false entries
  expect_false( fCheckValString(NA) )
  expect_false( fCheckValString(NULL) ) #length() == 0
  expect_false( fCheckValString(1) ) #is.character()
  expect_false( fCheckValString("") ) #empty string, nzchar()
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("fCheckValNumeric",{
  #allowed entries
  expect_true( fCheckValNum(1) )
  expect_true( fCheckValNum(NA_real_) )
  #false entries
  expect_false( fCheckValNum(NA) )
  expect_false( fCheckValNum(NULL) ) #length() == 0
  expect_false( fCheckValNum("a") ) #is.numeric()
})
