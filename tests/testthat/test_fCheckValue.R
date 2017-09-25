#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fCheck... functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context('fCheckValue...')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that('fCheckValString',{
  #allowed entries
  expect_true( fCheckValString('a') )
  expect_true( fCheckValString(NA_character_) )
  expect_true( fCheckValString(NA) )
  #false entries
  expect_false( fCheckValString(NULL) ) #length() == 0
  expect_false( fCheckValString(1) ) #is.character()
  expect_false( fCheckValString('') ) #empty string, nzchar()
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that('fCheckValNumeric',{
  #allowed entries
  expect_true( fCheckValNum(1) )
  expect_true( fCheckValNum(NA_real_) )
  expect_true( fCheckValNum(NA) )
  #false entries
  expect_false( fCheckValNum(NULL) ) #length() == 0
  expect_false( fCheckValNum('a') ) #is.numeric()
})
