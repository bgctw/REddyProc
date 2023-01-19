

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Variable check functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckValString <- function(
  ##description<<
  ## Check if variable is a non-empty character string
  Value.s               ##<< Value to be checked if string
  )
  ##author<<
  ## AMM
  ##details<<
  ## See test_CheckValue.R for more details.
{
  if ( length(Value.s) == 0) {
    FALSE
  } else if (!is.na(Value.s) && (!is.character(Value.s) || !nzchar(Value.s)) ) {
    FALSE
  } else {
    TRUE
  }
  ##value<<
  ## Boolean value if true of false.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckValNum <- function(
  ##description<<
  ## Check if variable is a numeric
  Value.n               ##<< Value to be checked if numeric (but can also be NA of any type)
)
  ##author<<
  ## AMM
  ##details<<
  ## See test_CheckValue.R for more details.
{
  if ( length(Value.n) == 0) {
    FALSE
  } else if (!is.na(Value.n) && !is.numeric(Value.n) ) {
    FALSE
  } else {
    TRUE
  }
  ##value<<
  ## Boolean value if true of false.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckColNames <- function(
  ##description<<
  ## Check if specified columns exists in data frame
  Data.F                ##<< Data frame
  , ColNames.V.s         ##<< Vector of variables to be checked
  , CallFunction.s = ''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  # TEST: Data.F <- Date.F.x; ColNames.V.s <- c('Year.n', 'none', 'Month.n', 'test'); CallFunction.s <- 'Dummy'
{
  #Exclude dummy 'none'
  NoneCols.b <- ColNames.V.s %in% 'none'
  #Check if specified columns exist in data frame
  NameCols.b <- ColNames.V.s[!NoneCols.b] %in% colnames(Data.F)
  if (!all(NameCols.b) ) {
    ColNames.s <- paste(ColNames.V.s[!NoneCols.b][!NameCols.b], collapse = ', ', sep = '')
    stop(CallFunction.s, ':::fCheckColNames::: Missing specified columns in dataset: ', ColNames.s, '!')
  }

  ##value<<
  ## Function stops on errors.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckColNum <- function(
  ##description<<
  ## Check if specified columns are numeric
  Data.F                ##<< Data frame
  , ColNames.V.s         ##<< Vector of variables to be checked, with 'none' as dummy
  , CallFunction.s = ''    ##<< Name of function called from
  , isWarnMissing = TRUE  ##<< set to FALSE to avoid warning when columns to
  ##  to check are missing
) {
  ##author<< TW
  #Exclude dummy 'none' from checking
  ColNames.V.s <- ColNames.V.s[ColNames.V.s != "none"]
  #Exclude columns not present in dataset from checking
  iMissing <- which(!(ColNames.V.s %in% names(Data.F)))
  colNamesCheck <- if (length(iMissing)) {
    if (isTRUE(isWarnMissing)) warning(
      "missing columns ", paste(ColNames.V.s[iMissing], collapse = ","))
    ColNames.V.s[-iMissing]
  } else ColNames.V.s
  isNotNumeric <- map_lgl(colNamesCheck, ~!is.numeric(Data.F[[.]]))
  if (sum(isNotNumeric)) {
    # report the first occurence of a nonnumeric
    colNameFirst <- colNamesCheck[isNotNumeric][1]
    x <- Data.F[[colNameFirst]]
    xn <- suppressWarnings(as.numeric(x))
    indexFirst <- which(!is.na(x) & is.na(xn))[1]
    stop(CallFunction.s, ":::fCheckColNum::: Detected following columns in ",
         "dataset to be non numeric: ",
         paste(colNamesCheck[isNotNumeric], collapse = ","), '! ',
         "First occurence of non-numeric value at column '",colNameFirst,
         "' at row ", indexFirst, " is '",x[indexFirst],"'.")
  }
  ##value<<
  ## Function stops on errors.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckOutsideRange <- function(
  ##description<<
  ## Check if specified variable is outside of provided boundaries
  Data.F                ##<< Data frame
  , VarName.s            ##<< Variable (column) name
  , Condition.V.s        ##<< Logical condition for outside values
  , CallFunction.s = ''   ##<< Name of function called from
)
  ##author<<
  ## AMM
  ##details<<
  ## Example of condition structure: c(' <= ', 0) or c(' <= ', 0, '|', '>', 20)
  ## Allowed relational operators: < <= == >= > !=
  ## Allowed logical operators: & |
  # TEST: Data.F <- Date.F.x; VarName.s <- 'Rg';  CallFunction.s <- 'test'; Condition.V.s <- c(' <= ', 0, '|', '>', 20); Condition.V.s <- c(' <= ', 0)
{
  fCheckColNames(Data.F, VarName.s, paste(CallFunction.s, 'fCheckOutsideRange', sep = ':::'))
  fCheckColNum(Data.F, VarName.s, paste(CallFunction.s, 'fCheckOutsideRange',  sep = ':::'))
  Var.V.n <- Data.F[, VarName.s]

  # Check condition
  CondText.s <- if (length(Condition.V.s) == 2 && Condition.V.s[1]  %in% c('<', ' <= ', ' == ', ' >= ', '>', ' != ') && nzchar(Condition.V.s[2]) ) {
    # One condition
    paste('Var.V.n ', Condition.V.s[1], ' ', Condition.V.s[2], ' & !is.na(Var.V.n)', sep = '')
  } else if (length(Condition.V.s) == 5 && all(Condition.V.s[c(1, 4)]  %in% c('<', ' <= ', ' == ', ' >= ', '>', ' != '))
             && all(nzchar(Condition.V.s[2]), nzchar(Condition.V.s[5])) && (Condition.V.s[3] %in% c('|', '&')) ) {
    # Two conditions
    paste('(Var.V.n ', Condition.V.s[1], ' ', Condition.V.s[2], '  ', Condition.V.s[3], ' Var.V.n ', Condition.V.s[4], ' ', Condition.V.s[5], ') & !is.na(Var.V.n)', sep = '')
  } else {
    stop(CallFunction.s, ':::fCheckOutsideRange::: Incorrect condition definition: ', paste(Condition.V.s, collapse = ' '), '!')
  }

  # Warning message
  Outside.b <- eval(parse(text = CondText.s))
  Outside.n <- sum(Outside.b)
  if (Outside.n > 0)
    warning(CallFunction.s, ':::fCheckOutsideRange::: Variable outside (plausible) range in ', Outside.n,
            ' cases! Invalid values with \'', VarName.s, ' ',
            paste(Condition.V.s, collapse = ' '), '\': ', paste(format(Var.V.n[Outside.b][1:(min(Outside.n, 50))], digits = 2), collapse = ', '), ' ...')

  return(invisible(NULL))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckColPlausibility <- function(
  ##description<<
  ## Check plausibility of common (eddy) variables
  Data.F                ##<< Data frame
  , VarName.V.s          ##<< Variable (column) names
  , CallFunction.s = ''   ##<< Name of function called from
)
  ##author<<
  ## AMM
  # TEST: VarName.V.s <- c('Rg_s'); v.i <- 1
{
  # Check column names
  SubCallFunc.s <- paste(CallFunction.s, 'fCheckColPlausibility', sep = ':::')
  fCheckColNames(Data.F, VarName.V.s, SubCallFunc.s)
  # Strip variable name to before dot '.' (because quality flag setting after dot)
  VarName.V.s <- sub('[.]. * ', '', VarName.V.s)

  ##details<<
  ## Variables CONTAINing the following abbreviations are checked for plausibility
  # Separated checks for upper and lower limit to have separate warnings
  for (v.i in 1:length(VarName.V.s)) {
    ## 'Rg' - global radiation, W m-2
    if (grepl('Rg', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 1200), SubCallFunc.s)
    }
	## 'PotRad' - potential global radiation, W m-2
	if (grepl('PotRad', VarName.V.s[v.i]) )
	{
		fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0), SubCallFunc.s)
		fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 3000), SubCallFunc.s)	#TODO plausible upper bound
	}
	## 'PPFD' or 'ppfd' - photosynthetic active radiation, umol m-2 s-1
    if (grepl('PPFD', VarName.V.s[v.i], ignore.case = TRUE) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 2500), SubCallFunc.s)
    }
    ## 'PAR' or 'par' - photosynthetic active radiation, umol m-2 s-1
    if (grepl('PAR', VarName.V.s[v.i], ignore.case = TRUE) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 2500), SubCallFunc.s)
    }
    ## 'Ta' - air temperature in degree Celsius
    if (grepl('Ta', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', -70), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 60), SubCallFunc.s)
    }
    ## 'Ts' - soil temperature in degree Celsius
    if (grepl('Ts', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', -20), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 80), SubCallFunc.s)
    }
    ## 'VPD' - vapour pressure deficit in hPa
    if (grepl('VPD', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0), SubCallFunc.s)
      #twutz 2301: at dry sites (ES-Abr) regularly high VPD
      #fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 50), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 65), SubCallFunc.s)
    }
    ## 'Rh' - relative humidity in %
    if (grepl('Rh', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', -10), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 110), SubCallFunc.s)
    }
    ## 'NEE' - in umol CO2 m-2 s-1 oder g C m-2 day-1
    if (grepl('NEE', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', -50), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 100), SubCallFunc.s)
    }
    ## 'ustar' - in m s-1
    if (grepl('ustar', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', -1), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 50), SubCallFunc.s)
    }
    ## 'E_0' - in degK
    if (grepl('E_0', VarName.V.s[v.i]) )
    {
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0), SubCallFunc.s)
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('>', 600), SubCallFunc.s)
    }
    ## FLUXNET _fqc, 0: data are original, 1: gapfilled high quality, 2: gapfilled medium quality, 3: gapfilled low quality
    if (grepl('_fqc', VarName.V.s[v.i]) && !grepl('_fqcOK', VarName.V.s[v.i], ignore.case = TRUE) ) # 0 is best
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0, '|', '>', 3), SubCallFunc.s)
    ## FLUXNET _fqcOK: 1 = if data are orginal or high quality gapfilled (_fqc was 0 or 1), O = otherwise
    if (grepl('_fqcOK', VarName.V.s[v.i], ignore.case = TRUE) ) # 1 (= 100%) is best
      fCheckOutsideRange(Data.F, VarName.V.s[v.i], c('<', 0, '|', '>', 1), SubCallFunc.s)
  }

  ##value<<
  ## Function produces warnings if variable outside range.
}

