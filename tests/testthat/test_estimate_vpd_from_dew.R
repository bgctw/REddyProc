# Author: TW
.tmp.f <- function(){
  library(testthat)
  library(dplyr)
}

# make big artificial gap in VPD, pretend to have filled Tair
df_ex <- Example_DETha98 %>%
  fConvertTimeToPosix('YDH', Year = 'Year',Day = 'DoY', Hour = 'Hour') %>%
  mutate(VPDorig = .data$VPD, Tair_f = .data$Tair)
i_artgap <- 3000:8000
df_ex$VPD[i_artgap] <- NA

test_that("artificial gap in Tharandt example",{
  df <- df_ex
  VPDfromDew <- estimate_vpd_from_dew(df)
  #summary(VPDfromDew)
  expect_true(all(!is.finite(df$Tair_f) | is.finite(VPDfromDew)))
  lm1 <- lm(df$VPDorig ~ 0 +VPDfromDew)
  expect_equivalent(coef(lm1), 1, tolerance = 0.1, scale = 1)
})

test_that("error or warning to too few VPD or Tair",{
  df <- df_ex;
  df$Tair[1:(nrow(df)*9/10 + 1)] <- NA
  expect_warning(
    VPDfromDew <- estimate_vpd_from_dew(df),
    "might be inaccurate"
  )
  df <- df_ex;
  df$Tair[-1] <- NA
  expect_error(
    VPDfromDew <- estimate_vpd_from_dew(df),
    "Need finite VPD"
  )
})

test_that("error or missing columns",{
  df <- df_ex;
  df$Tair_f <- NULL # drop columns
  expect_error(
    VPDfromDew <- estimate_vpd_from_dew(df),
    "Tair_f"
  )
})

test_that("class method",{
  EProc <- sEddyProc$new("DE-Tha", df_ex, ColNames = c('Tair','VPD', 'Tair_f'))
  EProc$sTEMP$VPD_f <- EProc$sDATA$VPD # pretend to have gapfilled
  #EProc$trace(sFillVPDFromDew, browser)
  EProc$sFillVPDFromDew()
  expect_true(sum(is.na(EProc$sDATA$VPD_f)) < 100)
  VPD_f <- EProc$sTEMP$VPD_f
  lm1 <- lm(df_ex$VPDorig ~ 0 +VPD_f)
  expect_equivalent(coef(lm1), 1, tolerance = 0.1, scale = 1)
})

test_that("class method warning on missing VPD_f",{
  EProc <- sEddyProc$new("DE-Tha", df_ex, ColNames = c('Tair','VPD', 'Tair_f'))
  expect_warning(
    EProc$sFillVPDFromDew()
  )
  expect_true(sum(is.na(EProc$sDATA$VPD_f)) < 100)
  VPD_f <- EProc$sTEMP$VPD_f
  lm1 <- lm(df_ex$VPDorig ~ 0 +VPD_f)
  expect_equivalent(coef(lm1), 1, tolerance = 0.1, scale = 1)
})

