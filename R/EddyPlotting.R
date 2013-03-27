#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for plotting +++
#+++ fingerprint, half-hourly means, half-hourly fluxes, and daily sums +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# TEST: sDATA <- EPTha.C$sDATA; sID <- EPTha.C$sID; sINFO <- EPTha.C$sINFO; 
# TEST: sDATA <- EPThaNC.C$sDATA; sID <- EPThaNC.C$sID; sINFO <- EPThaNC.C$sINFO;
# TEST: Var.s <- 'NEE';  Format.s <- 'pdf'; Dir.s <- 'plots'; QFvar.s <- 'none'; QFvalue.n <- NA;
# TEST: Var.s <- 'NEE_f'; VarUnc.s <- 'NEE_fsd';
# TEST: Year.i <- 1998; Month.i <- 10; Name.s <- 'Test'

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Internal helper functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sEddyProc$methods(  
  sxSetTitle = function(
    ##title<<
    ## sEddyProc - Internal function
    ##description<<
    ## Set title of plot.
    Var.s               ##<< Variable to plot
    ,QFvar.s            ##<< Quality flag of variable to be filled
    ,QFvalue.n          ##<< Value of quality flag for data to plot
    ,Name.s             ##<< Name of plot
  )
    ##author<<
    ## KS, AMM
  {
    'Set title of plot'
    # Set title depending on quality flag
    if (QFvar.s != 'none') {
      Title.s <- paste(Name.s, ' of ', Var.s, ' with ', QFvar.s, '=', round(QFvalue.n, digits=3), '\n for ', sID, sep='')
    } else {
      Title.s <- paste(Name.s, ' of ', Var.s, '\n', 'for ', sID, sep='')
    }
    
    return(Title.s)
    ##value<< 
    ## String with plot title
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(  
  sxOpenPlot = function(
    ##title<<
    ## sEddyProc - Internal function
    ##description<<
    ## Open graphics device.
    Var.s               ##<< Variable to plot
    ,QFvar.s            ##<< Quality flag of variable to be filled
    ,QFvalue.n          ##<< Value of quality flag for data to plot
    ,PlotType.s         ##<< Internal plot type
    ,WInch.n            ##<< Width in inch
    ,HInch.n            ##<< Height in inch
    ,Format.s           ##<< Graphics format ('png' or 'pdf')
    ,Dir.s              ##<< Directory for plotting
    ,CallFunction.s=''  ##<< Name of function called from
  )
  ##author<<
  ## KS, AMM
{ 
    'Open graphics device.'
    # Check variable to fill and apply quality flag
    SubCallFunc.s <- paste(CallFunction.s, 'fOpenPlot', sep=':::')
    
    #Set file name
    FileName.s <- 
      if (QFvar.s != 'none') {
        paste(sID, '_', sINFO$Y.NAME, '_', Var.s, '_', PlotType.s, '_', QFvar.s, '=', round(QFvalue.n, digits=3), sep='')
      } else {
        paste(sID, '_', sINFO$Y.NAME, '_', Var.s, '_', PlotType.s, sep='')
      }
    PlotFile.s <- fSetFile(paste(FileName.s, '.', Format.s, sep=''), Dir.s, F, SubCallFunc.s)
    
    # Prepare the name and open the plot output file
    if (Format.s == 'png') {
      png(file=PlotFile.s, units='px', pointsize=12, width=WInch.n*40, height=HInch.n*40)
    } else if (Format.s == 'pdf') {
      pdf(file=PlotFile.s, width=WInch.n, height=HInch.n)
    } else
      stop(SubCallFunc.s, '::: Format.s not valid: ', Format.s)
    
    PlotFile.s
    ##value<<
    ## Name of opened graphics device
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(  
  sxClosePlot = function(
    ##title<<
    ## sEddyProc - Internal function
    ##description<<
    ## Close screens and save graphics device to file.
    PlotFile.s          ##<< Name of opened graphics device
  )
    ##author<<
    ## KS, AMM
  { 
    'Close screens and save graphics device to file'
    # Close screen
    close.screen( all=TRUE )
    # Save graphics file
    dev.off()
    message(paste('Saved plot to:', PlotFile.s))
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Fingerprint
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotFingerprintY = function(
    ##title<<
    ## sEddyProc$sPlotFingerprintY - Plot fingerprint of specified year
    ##description<<
    ## The fingerprint for a single year is plotted to the current device, scaled to all data.
    Var.s               ##<< Variable to plot
    ,QFvar.s            ##<< Quality flag of variable to be filled
    ,QFvalue.n          ##<< Value of quality flag for data to plot
    ,Year.i             ##<< Year to plot
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotFingerprintY('NEE', 'none', NA, 1998); sPlotFingerprintY('NEE_f', 'NEE_fqc', 1, 1998)
  {
    'Plot fingerprint of specified year'
    # Set plot contents
    Plot.V.n <- fSetQF(sDATA, Var.s, QFvar.s, QFvalue.n, 'sPlotFingerprintY')
    t.b <- (Year.i == as.numeric(format(sDATA$sDateTime, '%Y')))
    
    # Calculate plot parameters
    XAxis.V.n <- seq(0, 24, by=2)
    YAxis.V.n <- seq(15, 345, by=30)
    fJetColors <- colorRampPalette(c('#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F', 'yellow', '#FF7F00', 'red', '#7F0000'))
    Jet.n <- 50 
    # Scale to all data
    YMin.n <- min(Plot.V.n, na.rm=T)
    YMax.n <- max(Plot.V.n, na.rm=T)
    
    # Plot fingerprint
    if (Year.i <= sINFO$Y.END) { #Plot single years
      # Daily sequence of DoY
      DoY.V.d  <- c(0:max(as.numeric(format(sDATA$sDateTime, '%j'))[t.b], na.rm=T))    
      # Plot
      par(mai=c(0.7, 0.7, 0.7, 0.4)) #Set margin
      image(seq(0, 24, by=0.5), DoY.V.d, matrix(Plot.V.n[t.b], nrow=48), zlim=c(YMin.n,YMax.n), col=fJetColors(Jet.n),
            axes=F, xlab='', ylab='', main=Year.i)
      axis(1, at=XAxis.V.n, cex.axis=1.0, tck=0.03, col.axis='blue')
      axis(2, at=YAxis.V.n, cex.axis=1.0, tck=0.03, labels=month.abb, padj=1, col.axis ='dark violet') 
      box()
      
    } else { #Plot legend and title
      Title.s <- sxSetTitle(Var.s, QFvar.s, QFvalue.n, 'Fingerprint')
      Seq.V.n <- seq(YMin.n, YMax.n, by=(YMax.n-YMin.n)/(Jet.n-1))
      par(mai=c(3,1,3,1))
      image(Seq.V.n, c(0,1), matrix(Seq.V.n, ncol=1), col=fJetColors(Jet.n), zlim=c(YMin.n,YMax.n),
            xlab=Var.s, yaxt='n', ylab='', main=Title.s)
      box()
    }
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotFingerprint = function(
    ##title<<
    ## sEddyProc$sPlotFingerprint - Image with fingerprints of each year
    ##description<<
    ## Generates image in specified format ('pdf' or 'png') with fingerprint, see also \code{\link{sPlotFingerprintY}}.
    Var.s               ##<< Variable to plot
    ,QFvar.s='none'     ##<< Quality flag of variable to be filled
    ,QFvalue.n=NA       ##<< Value of quality flag for data to plot
    ,Format.s='pdf'     ##<< Graphics file format ('pdf' or 'png')
    ,Dir.s='plots'      ##<< Directory for plotting
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotFingerprint('NEE'); sPlotFingerprint('NEE_f', 'NEE_fqc', 1)
  {
    'Image with fingerprints of each year'
    # Calculate number of screens and width and heigth
    Screens.n <- (sINFO$Y.NUMS +3) %/% 3
    WInch.n <- 15 #Needs to be this big to have enough space for margins
    HInch.n <- WInch.n/2 * Screens.n
    
    # Open plot
    PlotType.s <- 'FP'
    PlotFile.s <- sxOpenPlot(Var.s, QFvar.s, QFvalue.n, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, 'sPlotFingerprint')
    
    tryCatch({
      #Split into Screens.n screens with 3 columns
      split.screen(c(Screens.n,3))
      
      for( Year.i in sINFO$Y.START:(sINFO$Y.END+1) ) {
        screen(Year.i-sINFO$Y.START+1)
        sPlotFingerprintY(Var.s, QFvar.s, QFvalue.n, Year.i)
      }
      
      # Close plot
    }, finally=sxClosePlot(PlotFile.s))
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Half-hourly means
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotHHMeansM = function(
    ##title<<
    ## sEddyProc$sPlotHHMeansM - Plot half-hourly means of specified months
    ##description<<
    ## The half-hourly means of a single months (averaged over all years) are potted to the current device, scaled to all data.
    Var.s               ##<< Variable to plot
    ,QFvar.s            ##<< Quality flag of variable to be filled
    ,QFvalue.n          ##<< Value of quality flag for data to plot
    ,Month.i            ##<< Month to plot
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotHHMeansM('NEE', 'none', NA, 10)
  {
    'Plot half-hourly means of specified months'
    # Set plot contents
    # Half-hourly means 
    Plot.V.n <- fSetQF(sDATA, Var.s, QFvar.s, QFvalue.n, 'sPlotHHMeansM')
    Month.V.d <- matrix(as.numeric(format(sDATA$sDateTime, '%m')), ncol=48, byrow=T)[,1]
    Plot.M.n <- matrix(Plot.V.n, ncol=48, byrow=T)
    Mean.M.m <- as.matrix(aggregate(Plot.M.n, by=list(Month.V.d), FUN=mean, simplify=T, na.rm=T)[,-1])
    
    # Scale to all data
    YMin.n <- min(Mean.M.m, na.rm=T)
    YMax.n <- max(Mean.M.m, na.rm=T)   
    # Axis settings
    XAxis.V.n <- seq(0, 24, by=2)
    
    # Plot half-hourly means
    par(mai=c(0.7, 0.7, 0.7, 0.4)) #Set margin
    if( !sum(!is.na(Mean.M.m[Month.i,]))==0 ) {
      # Plot
      plot(as.vector(Mean.M.m[Month.i,]) ~ seq(0.0, 23.5, by=0.5), ylim=c(YMin.n,YMax.n),
           type='o', lty='solid', lwd=1, col='dark green', pch=20, cex=1,
           axes=F, xlab='', ylab='', main=month.name[Month.i])
      abline(h=0, col='grey')
      axis(1, at=XAxis.V.n, cex.axis=0.9, col.axis='blue')
      axis(2, cex.axis=1.0) 
      box()
    } else {
      # Plot empty box
      plot(rep(0,length(HMean.V.h)) ~ seq(0.0, 23.5, by=0.5), type='n', axes=F, main=month.name[Month.i])
      axis(1, at=XAxis.V.n, cex.axis=1.0, col.axis='blue')
      box()
      warning('sPlotHHMeansM::: No data available for month: ', month.name[Month.i])
    }
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotHHMeans = function(
    ##title<<  
    ## sEddyProc$sPlotHHMeans - Image with half-hourly means of each month
    ##description<<
    ## Generates image in specified format ('pdf' or 'png') with half-hourly means, see also \code{\link{sPlotHHMeansM}}.
    Var.s               ##<< Variable to plot
    ,QFvar.s='none'     ##<< Quality flag of variable to be filled
    ,QFvalue.n=NA       ##<< Value of quality flag for data to plot
    ,Format.s='pdf'     ##<< Graphics file format ('pdf' or 'png')
    ,Dir.s='plots'      ##<< Directory for plotting
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotHHMeans('NEE')
  {
    'Image with half-hourly means of each month'
    # Open plot
    PlotType.s <- 'HM'
    WInch.n <- 15
    HInch.n <- WInch.n/3 * 5
    PlotFile.s <- sxOpenPlot(Var.s, QFvar.s, QFvalue.n, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, 'sPlotHHMeans')
    
    tryCatch({  
      # Slpit the printing area in 5 lines and 3 rows
      split.screen(c(5,1))
      split.screen(c(3,1), screen=1)
      split.screen(c(1,3), screen=2)
      split.screen(c(1,3), screen=3)
      split.screen(c(1,3), screen=4)
      split.screen(c(1,3), screen=5)
      
      # Set title of plot
      screen(6) 
      mtext(sxSetTitle(Var.s, QFvar.s, QFvalue.n, 'Half-hourly means'), line=-3, side=3, cex=2.0)
      screen(8)
      mtext(sINFO$Y.NAME, line=1, side=3, cex=2.0)
      
      # Loop over all months
      for (Month.i in 1:12){
        screen(Month.i+8)
        sPlotHHMeansM(Var.s, QFvar.s, QFvalue.n, Month.i)
      }
      
      # Close plot  
    }, finally=sxClosePlot(PlotFile.s))
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Yearly half-hourly fluxes with daily means
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotHHFluxesY = function(
    ##title<<  
    ## sEddyProc$sPlotHHFluxesY -  Plot half-hourly fluxes of specified year
    ##description<<
    ## The half-hourly fluxes for a single year are plotted to the current device, scaled to all data.
    Var.s               ##<< Variable to plot
    ,QFvar.s            ##<< Quality flag of variable to be filled
    ,QFvalue.n          ##<< Value of quality flag for data to plot
    ,Year.i             ##<< Year to plot
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotHHFluxesY('NEE', 'none', NA, 1998)
  {
    'Plot half-hourly fluxes of specified year'
    # Set plot contents
    Plot.V.n <- fSetQF(sDATA, Var.s, QFvar.s, QFvalue.n, 'sPlotHHFluxesY')
    t.b <- (Year.i == as.numeric(format(sDATA$sDateTime, '%Y')))
    DMean.V.n <- rep(apply(matrix(Plot.V.n[t.b], nrow=48), 2, mean, na.rm=T), each=48)
    #Counter for half-hours
    hh.i <- (c(1:sum(t.b))-1) / sum(t.b)
    
    #Scale to all data
    YMin.n <- min(Plot.V.n, na.rm=T)
    YMax.n <- max(Plot.V.n, na.rm=T)
    # Axis settings
    XAxis.V.n <- seq((15*48)/sum(t.b), 345*48/sum(t.b), by=30*48/sum(t.b))
    
    # Plot half-hourly fluxes
    par(mai=c(0.7, 0.7, 0.7, 0.4)) #Set margin
    if( !sum(!is.na(Plot.V.n[t.b]))==0 ) {
      # Plot
      plot(Plot.V.n[t.b] ~ hh.i, ylim=c(YMin.n,YMax.n), col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3, 
           axes=F, xlab='', ylab='', main=Year.i)    
      abline(h=0, col='grey')
      lines(DMean.V.n ~ hh.i, lty='solid', lwd=1, col='red', pch=0, cex=1)
      axis(1, at=XAxis.V.n, cex.axis=1.0, col.axis='dark violet', labels=month.abb)
      axis(2, cex.axis=1.0) 
      box() 
    } else {
      # Plot empty box
      plot(rep(0,length(Plot.V.n[t.b])) ~ hh.i, type='n', axes=F, main=Year.i)
      axis(1, at=XAxis.V.n, cex.axis=1.0, col.axis='dark violet', labels=month.abb)
      box()
      warning('sPlotHHFluxesY::: No data available in year: ', Year.i)
    }
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotHHFluxes = function(
    ##title<<  
    ## sEddyProc$sPlotHHFluxes - Image with half-hourly fluxes for each year
    ##description<< 
    ## Generates image in specified format ('pdf' or 'png') with half-hourly fluxes and their daily means,
    ## see also \code{\link{sPlotHHFluxesY}}.
    Var.s               ##<< (Filled) variable to plot
    ,QFvar.s='none'     ##<< Quality flag of variable to be filled
    ,QFvalue.n=NA       ##<< Value of quality flag for data to plot
    ,Format.s='pdf'     ##<< Graphics file format ('pdf' or 'png')
    ,Dir.s='plots'      ##<< Directory for plotting
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotHHFluxes('NEE')   
  {
    'Image with half-hourly fluxes for each year'
    # Open plot
    PlotType.s <- 'Flux'
    WInch.n <- 15
    HInch.n <- WInch.n/3 * (sINFO$Y.NUMS+1)
    PlotFile.s <- sxOpenPlot(Var.s, QFvar.s, QFvalue.n, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, 'sPlotHHFluxes')
    
    tryCatch({
      # Split the screen
      split.screen(c(sINFO$Y.NUMS + 1, 1))
      split.screen(c(3,1), screen=1)
      
      # Set title of plot
      screen(sINFO$Y.NUMS + 3)
      mtext(sxSetTitle(Var.s, QFvar.s, QFvalue.n, 'Half-hourly fluxes and daily means'), line=-3, side=3, cex=2.0)
      
      # Loop over all years
      for( Year.i in sINFO$Y.START:sINFO$Y.END ) {
        screen(Year.i-sINFO$Y.START+1 + 1)
        sPlotHHFluxesY(Var.s, QFvar.s, QFvalue.n, Year.i)
      }
      
      # Close plot
    }, finally=sxClosePlot(PlotFile.s))
  }) 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Daily sums with and without uncertainties
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotDailySumsY = function(
    ##title<<
    ## sEddyProc$sPlotDailySumsY - Plot daily sum of specified year
    ##description<<
    ## The daily sums for a single year are plotted to the current device, scaled to all data.
    ## The daily sums are only calculated for days with complete data.
    Var.s               ##<< (Filled) variable to plot
    ,VarUnc.s           ##<< Uncertainty estimates for variable
    ,Year.i             ##<< Year to plot
  )
    ##author<<
    ## KS, AMM
    # TEST: sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
  {
    'Plot daily sum of specified year'
    # Set plot contents
    Plot.V.n <- fSetQF(sDATA, Var.s, 'none', NA, 'sPlotDailySumsY')
    if (VarUnc.s != 'none') {
      PlotSD.V.n <- fSetQF(sDATA, VarUnc.s, 'none', NA, 'sPlotDailySumsY')
    } else { # Set uncertainties to Zero
      PlotSD.V.n <- (rep(0, length(Plot.V.n)))
    }
    # Uncertainty only used where data
    PlotSD.V.n <- ifelse(is.na(Plot.V.n), NA, PlotSD.V.n)
    # If there is data but no uncertainty estimates, an empty box will be plotted
    CountMissingUnc.n <- sum(!is.na(Plot.V.n) & is.na(PlotSD.V.n))
    
    # Set daily sums
    DYear.V.d <- matrix(as.numeric(format(sDATA$sDateTime, '%Y')), nrow=48)[1,]
    DoY.V.d  <- matrix(as.numeric(format(sDATA$sDateTime, '%j')) , nrow=48)[1,]
    DSum.V.d <- 0.5 * apply(matrix(Plot.V.n, nrow=48), 2, sum)
    fSumOfSquares <- function(x, ...) {sum(x^2, ...)}
    DUnc.V.d <- 0.5 * sqrt(apply(matrix(PlotSD.V.n, nrow=48), 2, fSumOfSquares))
    y.b <- (Year.i == DYear.V.d)
    
    # Scale to all data
    YMin.n <- min(DSum.V.d-DUnc.V.d, na.rm=T)
    YMax.n <- max(DSum.V.d+DUnc.V.d, na.rm=T)
    # Axis settings
    XAxis.V.n <- seq(15, 345, by=30)
    
    # Plot daily sums
    par(mai=c(0.7, 0.7, 0.7, 0.4)) #Set margin
    if( !sum(!is.na(DSum.V.d[y.b])) == 0 && CountMissingUnc.n == 0 ) {
      # Plot
      plot(DSum.V.d[y.b] ~ DoY.V.d[y.b], type='n', ylim=c(YMin.n,YMax.n),
           axes=F, xlab='', ylab='', main=Year.i)
      if (VarUnc.s != 'none')
        polygon(c(DoY.V.d[y.b], rev(DoY.V.d[y.b])), c(DSum.V.d[y.b]+DUnc.V.d[y.b], rev(DSum.V.d[y.b]-DUnc.V.d[y.b])), 
                col='dark grey', border=NA)
      abline(h=0, col='grey')
      lines(DSum.V.d[y.b], lty='solid', lwd=1, col='dark green')
      points(DSum.V.d[y.b], pch=20, cex=0.7, col='dark green')
      axis(1, at=XAxis.V.n, cex.axis=1.0, labels=month.abb, col.axis='dark violet')
      axis(2, cex.axis=1.0) 
      box() 
    } else { 
      # Plot empty box
      plot(rep(0, length(DSum.V.d[y.b])) ~ DoY.V.d[y.b], type='n', axes=F, main=Year.i)
      axis(1, at=XAxis.V.n, cex.axis=1.0, labels=month.abb, col.axis='dark violet')
      box()
      if (CountMissingUnc.n != 0) {
        warning('sPlotDailySumsY::: Uncertainty estimates missing for ', CountMissingUnc.n, ' data points of ', Var.s, 
                ' in year: ', Year.i, 'This will cause an empty plot!')
      } else {
        warning('sPlotDailySumsY::: Missing data in year: ', Year.i)
      }
    }
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPlotDailySums = function(
    ##title<<  
    ## sEddyProc$sPlotDailySums - Image with daily sums of each year
    ##description<<
    ## Generates image in specified format ('pdf' or 'png') with daily sums, see also \code{\link{sPlotDailySumsY}}.
    Var.s               ##<< (Filled) variable to plot
    ,VarUnc.s='none'    ##<< Uncertainty estimates for variable
    ,Format.s='pdf'     ##<< Graphics file format ('pdf' or 'png')
    ,Dir.s='plots'      ##<< Directory for plotting
  )
    ##author<<
    ## KS, AMM  
    # TEST: sPlotDailySums('NEE'); sPlotDailySums('NEE_f','NEE_fsd')
  {
    'Image with daily sums of each year'
    # Open plot
    PlotType.s <- if (VarUnc.s == 'none') 'DSum' else 'DSumU'
    WInch.n <- 15
    HInch.n <- WInch.n/3 * (sINFO$Y.NUMS+1)
    PlotFile.s <- sxOpenPlot(Var.s, 'none', NA, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, 'sPlotDailySums')
    
    tryCatch({
      # Split screen 
      split.screen(c(sINFO$Y.NUMS + 1, 1))
      split.screen(c(3,1), screen=1)
      
      # Set title of plot
      screen(sINFO$Y.NUMS + 3)
      if (VarUnc.s == 'none') {
        mtext(sxSetTitle(Var.s, 'none', NA, 'Daily sums'), line=-3, side=3, cex=2.0)
      } else {
        mtext(sxSetTitle(Var.s, 'none', NA, 'Daily sums with uncertainties'), line=1, side=3, cex=2.0)
      }
      
      # Loop over all years
      for( Year.i in sINFO$Y.START:sINFO$Y.END ) {
        screen(Year.i-sINFO$Y.START+1 + 1)
        sPlotDailySumsY(Var.s, VarUnc.s, Year.i)
      }
      
      # Close plot
    }, finally=sxClosePlot(PlotFile.s))
  }) 
