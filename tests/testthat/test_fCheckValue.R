#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fCheck... functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context('fCheckValue...')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that('fCheckValStringing',{
  #allowed entries
  expect_true( REddyProc:::fCheckValString('a') )
  expect_true( REddyProc:::fCheckValString(NA_character_) )
  expect_true( REddyProc:::fCheckValString(NA) )
  #false entries
  expect_false( REddyProc:::fCheckValString(NULL) ) #length() == 0
  expect_false( REddyProc:::fCheckValString(1) ) #is.character()
  expect_false( REddyProc:::fCheckValString('') ) #empty string, nzchar()
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that('fCheckValNumeric',{
  #allowed entries
  expect_true( REddyProc:::fCheckValNum(1) )
  expect_true( REddyProc:::fCheckValNum(NA_real_) )
  expect_true( REddyProc:::fCheckValNum(NA) )
  #false entries
  expect_false( REddyProc:::fCheckValNum(NULL) ) #length() == 0
  expect_false( REddyProc:::fCheckValNum('a') ) #is.numeric()
})
