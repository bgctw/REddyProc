# Author: TW
#require(testthat)
context("getExamplePath")

# TRUE during testthat::check()
isNotOnCRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

test_that("NOT_CRAN",{
  skip_on_cran()
  formerEnv <- Sys.getenv("NOT_CRAN")
  tryCatch({
    Sys.setenv(NOT_CRAN = "true")
    exDir <- REddyProc:::.getExampleDir()
    expect_equal( grep("REddyProc/REddyProcExamples", exDir), 1L)
    fName <- "Example_DE-Tha.1996.1998.hourly_selVars.nc"
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
    exDir <- REddyProc:::.getExampleDir()
    # temp directory does not contain REddyProc
    expect_equal( grep("REddyProc/REddyProcExamples", exDir), integer(0))
    fName <- "Example_DE-Tha.1996.1998.hourly_selVars.nc"
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

