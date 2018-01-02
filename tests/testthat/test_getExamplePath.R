# Author: TW
#require(testthat)
context("getExamplePath")

# TRUE during testthat::check()
isNotOnCRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

test_that("NOT_CRAN true",{
  skip_on_cran()
  formerEnv <- Sys.getenv("NOT_CRAN")
  tryCatch({
    Sys.setenv(NOT_CRAN = "true")
    exDir <- getREddyProcExampleDir()
    # dirname on a directory returns the parent directory
    expect_equal( file.path(dirname(tempdir()),"REddyProcExamples"), exDir)
    fName <- "readme.txt"
    fPath <- file.path(exDir,fName)
    # first make sure example is not there
    unlink(fPath)
    # get path without trying to download
    exPath <- getExamplePath( fName, FALSE )
    expect_equal( exPath, character(0))
    # get path with trying to download
    exPath <- getExamplePath( fName, TRUE )
    expect_equal( exPath, fPath)
  }
  , finally = Sys.setenv(NOT_CRAN = formerEnv) )
})

test_that("NOT_CRAN undefined",{
  skip_on_cran()
  formerEnv <- Sys.getenv("NOT_CRAN")
  tryCatch({
    Sys.setenv(NOT_CRAN = "")
    exDir <- getREddyProcExampleDir()
    expect_equal( file.path(tempdir(),"REddyProcExamples"), exDir)
    fName <- "readme.txt"
    fPath <- file.path(exDir,fName)
    # first make sure example is not there
    unlink(fPath)
    # get path without trying to download
    exPath <- getExamplePath( fName, FALSE )
    expect_equal( exPath, character(0))
    # get path with trying to download
    exPath <- getExamplePath( fName, TRUE )
    expect_equal( exPath, fPath)
  }
  , finally = Sys.setenv(NOT_CRAN = formerEnv) )
})

