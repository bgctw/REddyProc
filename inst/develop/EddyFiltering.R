#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for ustar filtering +++
#+++ Various ustar and other filter methods +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TEST: sDATA <- EPTha.C$sDATA; sINFO <- EPTha.C$sINFO; sTEMP <- EPTha.C$sTEMP;

# Jsut to give you an idea of the next steps, here the development steps described in the DevelopmentNotes.docx
# 
# The following step are necessary when implementing a new functionality such as ustar filtering into the REddyProc package:
# 1.	Implement the new routine(s) in a script under inst/develop
# 2.	Test the code with a test script e.g. /inst/develop/testEddyProc.R
# 3.	Write unit test e.g. in test_sEddyProc.R for R5 class methods
# 4.	When almost final, move script with function to /R
# 5.	Add example for routine to official example code sEddyProc.example in Eddy.R
# 6.	Generate (automated) documentation and compile package with script genRpackage.R
# 7.	Load package into R, check function documentation and test official example code
# 8.	Implement new functionality into online tool
# 9.	When final, submit new package version to R-Forge
# 10.	Document new functionality on webpage


sEddyProc$methods(
  sUstarMM = function(
    ##title<<
    ## sEddyProc$sUstarMM - Ustar filtering after Mirco
    ##description<<
    ## Function description, only dummy so far!!!
    FluxVar.s='NEE'        ##<< Variable of net ecosystem fluxes
    ,TempVar.s='Tair'      ##<< Air temperature variable (degC)
    ,UstarVar.s='Ustar'    ##<< Friction velocity ustar (ms-1)
    ,IndVar.s='none'       ##<< Indicator variable for splitting the time periods (e.g. phenology)
    ,BinsTemp.n=6          ##<< Number of bins for temperature
    ,BinsUstar.n=20        ##<< Number of bins for ustar
  )
  ##author<<
  ## MM, AMM
  #TEST: FluxVar.s <- 'NEE'; TempVar.s <- 'Tair'; UstarVar.s <- 'Ustar'; IndVar.s <- 'none'
{
    'Same as function description (used for R5 class description)'
    
    # Check column names (with 'none' as dummy)
    # (Numeric type and plausibility have been checked on initialization of sEddyProc)
    fCheckColNames(sDATA, c(FluxVar.s, TempVar.s, UstarVar.s, IndVar.s), 'sUstarMM')
    
    # Normal comments
    NEE.V.n <- sDATA[,FluxVar.s]
    Ustar.V.n <- sDATA[,UstarVar.s]
    Year.V.h <- as.numeric(format(sDATA$sDateTime, '%Y'))
    Month.V.h <- as.numeric(format(sDATA$sDateTime, '%m'))
    
    # Attention, not single years but several years
    ##details<<
    ## Ustar threshold calculated separately per year and then ...
    if( IndVar.s=='none' ) {
      for( Year.i in sINFO$Y.START:sINFO$Y.END ) {
        UstarThres_dummy.n <- quantile(Ustar.V.n[Year.V.h==Year.i & Month.V.h %in% c(1:3)], probs=0.25, na.rm=T)
      }
    }
    
    ##details<<
    ## Attention: So far only dummy ustar filter, takes the 25th percentile of the first quarter!!!
    QFustar.V.n <- ifelse(Ustar.V.n > UstarThres_dummy.n, 0, 1)
    attr(QFustar.V.n, 'varnames') <- 'UstarMM_fqc'
    attr(QFustar.V.n, 'units') <- '-'

    # Add to results data frame
    sTEMP$UstarMM_fqc <<- QFustar.V.n
    message('Dummy ustar filtering for \'', FluxVar.s, '\' resulted in a threshold of ', UstarThres_dummy.n, '.')

    return(invisible(QFustar.V.n))
    ##value<< 
    ## Vector with quality flag  
  })
