#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for plotting +++
#+++ fingerprint, diurnal cycle, half-hourly fluxes, and daily sums +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# TEST: sDATA <- EPTha.C$sDATA; sID <- EPTha.C$sID; sINFO <- EPTha.C$sINFO; sTEMP <- EPTha.C$sTEMP;
# TEST: sDATA <- EPThaNC.C$sDATA; sID <- EPThaNC.C$sID; sINFO <- EPThaNC.C$sINFO; sTEMP <- EPThaNC.C$sTEMP;
# TEST: sDATA <- EPThaNCsub.C$sDATA; sID <- EPThaNCsub.C$sID; sINFO <- EPThaNCsub.C$sINFO; sTEMP <- EPThaNCsub.C$sTEMP;
# TEST: Var.s <- 'NEE';  Format.s <- 'pdf'; Dir.s <- 'plots'; QFVar.s <- 'none'; QFValue.n <- NA; Legend.b <- T
# TEST: Var.s <- 'NEE_f'; VarUnc.s <- 'NEE_fsd';
# TEST: Year.i <- 1998; Month.i <- 10; Name.s <- 'Test'

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Internal helper functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.sEddyProc_sxSetTitle <- function(
    ##title<<
    ## sEddyProc - Internal function
    ##description<<
    ## Set title of plot.
    Var.s               ##<< Variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Name.s             ##<< Name of plot
	  , unit.s =		##<< unit string, defaults to attribute of the variable
        attr(cbind(sDATA, sTEMP)[, Var.s], 'units')
) {
    ##author<<
    ## KS, AMM
    'Set title of plot'
    # Check for unit of variable
    if (fCheckValString(unit.s) && unit.s != '[#]'  && unit.s != '--') unit.s <-
        paste(' (', unit.s, ') ', sep = '') else unit.s <- ' (-) '
    # Set title depending on quality flag
    if (QFVar.s != 'none') {
      Title.s <- paste(Name.s, ' at ', sID, ':\n', Var.s, unit.s, ' with '
          , QFVar.s, ' = ', round(QFValue.n, digits = 3), sep = '')
    } else {
      Title.s <- paste(Name.s, ' at ', sID, ':\n', Var.s, unit.s, sep = '')
    }

    return(Title.s)
    ##value<<
    ## String with plot title
}
sEddyProc$methods(.sxSetTitle = .sEddyProc_sxSetTitle)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.sEddyProc_sxOpenPlot <- function(
    ##title<<
    ## sEddyProc - Internal function
    ##description<<
    ## Open graphics device.
    Var.s               ##<< Variable to plot
    , QFVar.s            ##<< Quality flag of variable to be filled
    , QFValue.n          ##<< Value of quality flag for data to plot
    , PlotType.s         ##<< Internal plot type
    , WInch.n            ##<< Width in inch
    , HInch.n            ##<< Height in inch
    , Format.s           ##<< Graphics format ('png' or 'pdf' or 'cairo-png')
    , Dir.s              ##<< Directory for plotting
    , CallFunction.s = ''  ##<< Name of function called from
	  , DotsPerInc.n = 72	  ##<< Number of dots per inch for converting width
        ## and height to pixels on png output
) {
  ##author<<
  ## AMM, KS, TW
    'Open graphics device.'
    # Check if variable names exist and numeric before opening plot
    SubCallFunc.s <- paste(CallFunction.s, '.self$.sxOpenPlot', sep = ':::')
    fCheckColNames(cbind(sDATA, sTEMP), c(Var.s, QFVar.s), SubCallFunc.s)
    fCheckColNum(cbind(sDATA, sTEMP), c(Var.s, QFVar.s), SubCallFunc.s)

    #Set file name
    FileName.s <-
      if (QFVar.s != 'none') {
        paste(sID, '_', sINFO$Y.NAME, '_', PlotType.s, '_', Var.s, '('
              , QFVar.s, ' = ', round(QFValue.n, digits = 3), ')', sep = '')
      } else {
        paste(sID, '_', sINFO$Y.NAME, '_', PlotType.s, '_', Var.s, sep = '')
      }
    FileExtension.s <- if (Format.s == 'cairo-png' ||  Format.s == 'cairo')
      'png' else Format.s
    PlotFile.s <- fSetFile(paste(FileName.s, '.', FileExtension.s, sep = '')
      , Dir.s, F, SubCallFunc.s)

    ##details<<
    ## Not all formats are supported on all platforms. The \code{png} will
    ## not work on unix without X-system. However
    ## there might be cairo support be built into R, allowing to use
    ## the 'cairo-png' format.

    # Prepare the name and open the plot output file
    if (Format.s == 'png') {
      png(filename = PlotFile.s, width = round(WInch.n * DotsPerInc.n)
          , height = round(HInch.n * DotsPerInc.n) )
    } else if (Format.s == 'pdf') {
      pdf(file = PlotFile.s, width = WInch.n, height = HInch.n)
    } else if (Format.s == 'cairo') {
      #Should work on Mac but needs bug fix by developer,
      # see http: // tolstoy.newcastle.edu.au / R / e17 / devel / 12 / 01 / 0128.html
      png(filename = PlotFile.s, width = round(WInch.n * DotsPerInc.n)
          , height = round(HInch.n * DotsPerInc.n), type = 'cairo')
    } else if (Format.s == 'cairo-png') {
      png(filename = PlotFile.s, width = round(WInch.n * DotsPerInc.n)
          , height = round(HInch.n * DotsPerInc.n), type = 'cairo-png')
    } else {
      stop(SubCallFunc.s, '::: Format.s not valid: ', Format.s, '!')
    }
    devAskNewPage(ask = FALSE)  # to prevent asking Return
    PlotFile.s
    ##value<<
    ## Name of opened graphics device
}
sEddyProc$methods(.sxOpenPlot = .sEddyProc_sxOpenPlot)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.sEddyProc_sxClosePlot <- function(
    ##title<<
    ## sEddyProc - Internal function
    ##description<<
    ## Close screens and save graphics device to file.
    PlotFile.s          ##<< Name of opened graphics device
) {
##author<<
    ## KS, AMM
    'Close screens and save graphics device to file'
    # Close screen
    close.screen(all.screens = TRUE)
    # Save graphics file
    dev.off()
    message(paste('Saved plot to:', PlotFile.s))
}
sEddyProc$methods(.sxClosePlot = .sEddyProc_sxClosePlot)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Fingerprint
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#.plotFingerprintY = function(
#		#twutz: TODO to use outside R5 class, makes reference to self
#		### Plot fingerprint of specified year
#		Var.s               ##<< Variable to plot
#		, QFVar.s = 'none'     ##<< Quality flag of variable to be filled
#		, QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
#		, Year.i             ##<< Year to plot
#		, Legend.b = F         ##<< Plot only legend
#		, col = colorRampPalette(c('#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F', 'yellow', '#FF7F00', 'red', '#7F0000'))(50)
#) {
#	##description<<
#	## The fingerprint for a single year is plotted to the current device, scaled to all data.
#	##author<<
#	## AMM, KS, TW
#	# TEST: sPlotFingerprintY('NEE', 'none', NA, 1998); sPlotFingerprintY('NEE_f', 'NEE_fqc', 1, 1998)
#	'Plot fingerprint of specified year'
#	# Set plot contents
#	Data.V.n <- fSetQF(cbind(sDATA, sTEMP), Var.s, QFVar.s, QFValue.n, 'sPlotFingerprintY')
#	# Scale to all data
#	YMin.n <- min(Data.V.n, na.rm = T)
#	YMax.n <- max(Data.V.n, na.rm = T)
#	#Set yearly data
#	FullYearData.F <- fExpandToFullYear(sDATA$sDateTime, Data.V.n, Year.i, sINFO$DTS, 'sPlotFingerprintY')
#	Time.V.n <- FullYearData.F$DateTime
#	Plot.V.n <- FullYearData.F$Data
#
#	# Calculate plot parameters
#	XAxis.V.n <- seq(0, 24, by = 2)
#	YAxis.V.n <- seq(15, 345, by = 30)
#	#fJetColors <- colorRampPalette(c('#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F', 'yellow', '#FF7F00', 'red', '#7F0000'))
#	#Jet.n <- 50
#
#	# Daily sequence of DoY
#	DoY.V.d  <- c(0:max(as.numeric(format(Time.V.n, '%j')), na.rm = T))
#
#	# Plot
#	if (!sum(!is.na(Plot.V.n)) == 0 && Legend.b == F) {
#		# Plot fingerprint
#		par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
#		image(seq(0, 24, by = (24 / sINFO$DTS)), DoY.V.d, matrix(Plot.V.n, nrow = sINFO$DTS), zlim = c(YMin.n, YMax.n), col = col,
#				axes = F, xlab = '', ylab = '', main = Year.i)
#		axis(1, at = XAxis.V.n, cex.axis = 1.0, tck = 0.03, col.axis = 'blue')
#		axis(2, at = YAxis.V.n, cex.axis = 1.0, tck = 0.03, labels = month.abb, padj = 1, col.axis = 'dark violet')
#		box()
#	} else if (Legend.b == F) {
#		#Plot empy box
#		par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
#		image(seq(0, 24, by = (24 / sINFO$DTS)), DoY.V.d, matrix(Plot.V.n, nrow = sINFO$DTS), zlim = c(0, 1), col = col,
#				axes = F, xlab = '', ylab = '', main = Year.i)
#		axis(1, at = XAxis.V.n, cex.axis = 1.0, tck = 0.03, col.axis = 'blue')
#		axis(2, at = YAxis.V.n, cex.axis = 1.0, tck = 0.03, labels = month.abb, padj = 1, col.axis = 'dark violet')
#		box()
#		warning('sPlotFingerprintY::: No data available for year: ', Year.i, '!')
#	} else { #Plot legend and title
#		Title.s <- .self$.sxSetTitle(Var.s, QFVar.s, QFValue.n, 'Fingerprint')
#		Seq.V.n <- seq(YMin.n, YMax.n, by = (YMax.n-YMin.n) / (length(col)-1))
#		par(mai = c(3, 1, 3, 1))
#		image(Seq.V.n, c(0, 1), matrix(Seq.V.n, ncol = 1), col = col, zlim = c(YMin.n, YMax.n),
#				xlab = Var.s, yaxt = 'n', ylab = '', main = Title.s)
#		box()
#	}
#}
#attr(.plotFingerprintY, "ex") <- function() {
#	print("TODO")
#}


#' @export
sEddyProc_sPlotFingerprintY <- function(
    ##title<<
    ## sEddyProc$sPlotFingerprintY - Plot fingerprint of specified year
    ##description<<
    ## The fingerprint for a single year is plotted to the current device,
    ## scaled to all data.
    Var.s               ##<< Variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Year.i             ##<< Year to plot
    , Legend.b = F         ##<< Plot only legend
	, Col.V =				##<< Color palette for fingerprint plot
	  ## (can be also defined by user), i.e. color scale argument to \code{\link{image}}
		colorRampPalette(c('#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F', 'yellow', '#FF7F00', 'red', '#7F0000'))(50)
	, valueLimits = range(Plot.V.n, na.rm = TRUE)	##<< values outside this range
	  ## will be set to the range borders to avoid distorting colour scale
		## e.g. valueLimits = quantile(EddyProc.C$sDATA$NEE, prob = c(0.05, 0.95), na.rm = TRUE)
) {
    ##author<<
    ## AMM, KS, TW
    # TEST: sPlotFingerprintY('NEE', 'none', NA, 1998); sPlotFingerprintY('NEE_f', 'NEE_fqc', 1, 1998)
	#
    'Plot fingerprint of specified year'
    # Set plot contents
    Data.V.n <- fSetQF(cbind(sDATA, sTEMP), Var.s, QFVar.s, QFValue.n, 'sPlotFingerprintY')
    # Scale to all data
    YMin.n <- min(Data.V.n, na.rm = T)
    YMax.n <- max(Data.V.n, na.rm = T)
    #Set yearly data
    FullYearData.F <- fExpandToFullYear(sDATA$sDateTime, Data.V.n, Year.i
          , sINFO$DTS, 'sPlotFingerprintY')
    Time.V.n <- FullYearData.F$DateTime
    Plot.V.n <- FullYearData.F$Data
	  # set outliers to range limits in order to not distort colour scale
	  Plot.V.n <- pmax(pmin(Plot.V.n, valueLimits[2]), valueLimits[1])

    # Calculate plot parameters
    XAxis.V.n <- seq(0, 24, by = 2)
    YAxis.V.n <- seq(15, 345, by = 30)
    #fJetColors <- colorRampPalette(c('#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F', 'yellow', '#FF7F00', 'red', '#7F0000'))
    #Jet.n <- 50

    # Daily sequence of DoY
    DoY.V.d  <- c(0:max(as.numeric(format(Time.V.n, '%j')), na.rm = T))

    # Plot
    if (!sum(!is.na(Plot.V.n)) == 0 && Legend.b == F) {
      # Plot fingerprint
      par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
      image(seq(0, 24, by = (24 / sINFO$DTS)), DoY.V.d, matrix(Plot.V.n, nrow = sINFO$DTS), zlim = c(YMin.n, YMax.n), col = Col.V,
            axes = F, xlab = '', ylab = '', main = Year.i)
      axis(1, at = XAxis.V.n, cex.axis = 1.0, tck = 0.03, col.axis = 'blue')
      axis(2, at = YAxis.V.n, cex.axis = 1.0, tck = 0.03, labels = month.abb, padj = 1, col.axis = 'dark violet')
      box()
    } else if (Legend.b == F) {
      #Plot empy box
      par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
      image(seq(0, 24, by = (24 / sINFO$DTS)), DoY.V.d, matrix(Plot.V.n, nrow = sINFO$DTS), zlim = c(0, 1), col = Col.V,
           axes = F, xlab = '', ylab = '', main = Year.i)
      axis(1, at = XAxis.V.n, cex.axis = 1.0, tck = 0.03, col.axis = 'blue')
      axis(2, at = YAxis.V.n, cex.axis = 1.0, tck = 0.03, labels = month.abb, padj = 1, col.axis = 'dark violet')
      box()
      warning('sPlotFingerprintY::: No data available for year: ', Year.i, '!')
    } else { #Plot legend and title
      Title.s <- .self$.sxSetTitle(Var.s, QFVar.s, QFValue.n, 'Fingerprint')
      Seq.V.n <- seq(YMin.n, YMax.n, by = (YMax.n-YMin.n) / (length(Col.V)-1))
      par(mai = c(3, 1, 3, 1))
      image(Seq.V.n, c(0, 1), matrix(Seq.V.n, ncol = 1), col = Col.V, zlim = c(YMin.n, YMax.n),
            xlab = Var.s, yaxt = 'n', ylab = '', main = Title.s)
      box()
    }
}
sEddyProc$methods(sPlotFingerprintY = sEddyProc_sPlotFingerprintY)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotFingerprint <- function(
    ##title<<
    ## sEddyProc$sPlotFingerprint - Image with fingerprints of each year
    ##description<<
    ## Generates image in specified format \code{Format.s} (e.g. 'pdf' or 'png')
	  ## with fingerprint, see also \code{\link{sEddyProc_sPlotFingerprintY}}.
    Var.s               ##<< Variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Format.s = 'pdf'     ##<< Graphics file format (e.g. 'pdf', 'png')
    , Dir.s = 'plots'      ##<< Directory for plotting
	, ...				##<< further arguments to \code{\link{sEddyProc_sPlotFingerprintY}}
) {
    ##author<<
    ## KS, AMM
    ##alias<< sPlotFingerprint
    # TEST: sPlotFingerprint('NEE'); sPlotFingerprint('NEE_f', 'NEE_fqc', 1)
    'Image with fingerprints of each year'
    # Calculate number of screens and width and heigth
    Screens.n <- (sINFO$Y.NUMS + 3) %/% 3
    WInch.n <- 15 #Needs to be this big to have enough space for margins
    HInch.n <- WInch.n / 2 * Screens.n

    # Open plot
    PlotType.s <- 'FP'
    PlotFile.s <- .self$.sxOpenPlot(Var.s, QFVar.s, QFValue.n, PlotType.s
                      , WInch.n, HInch.n, Format.s, Dir.s, 'sPlotFingerprint')

    tryCatch({
      #Split into Screens.n screens with 3 columns
      split.screen(c(Screens.n, 3))

      for (Year.i in sINFO$Y.START:sINFO$Y.END) {
        screen(Year.i-sINFO$Y.START + 1)
        sPlotFingerprintY(Var.s, QFVar.s, QFValue.n, Year.i, ...)
      }
      screen(sINFO$Y.END-sINFO$Y.START + 2)
      sPlotFingerprintY(Var.s, QFVar.s, QFValue.n, sINFO$Y.END, Legend = T, ...)

      # Close plot
    }, finally = .self$.sxClosePlot(PlotFile.s))
}
sEddyProc$methods(sPlotFingerprint = sEddyProc_sPlotFingerprint)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Diurnal cycles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
.sEddyProc_sPlotDiurnalCycleM <- function(
    ##title<<
    ## sEddyProc$.sPlotDiurnalCycleM - Plot diurnal cycles of specified month
    ##description<<
    ## The diurnal cycles of a single month are potted to the current device, scaled to all data. Each year is plotted as a different (coloured) line.
    Var.s               ##<< Variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Month.i            ##<< Month to plot
    , Legend.b = T         ##<< Plot with legend
) {
    ##author<<
    ## AMM, KS
    # TEST: .sPlotDiurnalCycleM('NEE', 'none', NA, 10)
    'Plot diurnal cycles of specified month'
    # Set plot contents
    # Diurnal cycles
    Plot.V.n <- fSetQF(cbind(sDATA, sTEMP), Var.s, QFVar.s, QFValue.n, '.sPlotDiurnalCycleM')
    Month.V.d <- matrix(as.numeric(format(sDATA$sDateTime, '%m')), nrow = sINFO$DTS)[1, ]
    DYear.V.d <- matrix(as.numeric(format(sDATA$sDateTime, '%Y')), nrow = sINFO$DTS)[1, ]
    Plot.M.n <- matrix(Plot.V.n, ncol = sINFO$DTS, byrow = T)
    # Average all years: Mean.M.m <- as.matrix(aggregate(Plot.M.n, by = list(Month.V.d), FUN = mean, simplify = T, na.rm = T)[, -1])
    Mean.F.m <- aggregate(Plot.M.n, by = list(Year = DYear.V.d, Month = Month.V.d), FUN = mean, simplify = T, na.rm = T)

    # Scale to all data
    YMin.n <- min(Mean.F.m[, c(-1, -2)], na.rm = T)
    YMax.n <- max(Mean.F.m[, c(-1, -2)], na.rm = T)
    # Axis settings
    XAxis.V.n <- seq(0, 24, by = 2)

    # Plot diurnal cycles
    par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
    if (sum(!is.na(Mean.F.m[Mean.F.m$Month == Month.i, c(-1, -2)])) > 0) {
      # Plot
      plot(rep(0, sINFO$DTS)~seq(0.0, 23.5, by = (24 / sINFO$DTS)), type = 'n',
            axes = F, xlab = '', ylab = '', ylim = c(YMin.n, YMax.n), main = month.name[Month.i])
      for (Year.i in sINFO$Y.START:sINFO$Y.END) {
        MeanY.V.m <- as.numeric(Mean.F.m[Mean.F.m$Year == Year.i & Mean.F.m$Month == Month.i, c(-1, -2)])
        points(MeanY.V.m ~ seq(0.0, 23.5, by = (24 / sINFO$DTS)), type = 'o', lty = 'solid', lwd = 1, col = (Year.i-sINFO$Y.START) + 2, pch = 20, cex = 1)
      }
      abline(h = 0, col = 'grey')
      axis(1, at = XAxis.V.n, cex.axis = 0.9, col.axis = 'blue')
      axis(2, cex.axis = 1.0)
      if (Legend.b) legend('bottomright', legend = c(sINFO$Y.START:sINFO$Y.END), lty = 'solid', col = ((sINFO$Y.START:sINFO$Y.END)-sINFO$Y.START) + 2)
      box()
    } else {
      # Plot empty box
      plot(rep(0, sINFO$DTS)~seq(0.0, 23.5, by = (24 / sINFO$DTS)), type = 'n', axes = F, xlab = '', ylab = '', main = month.name[Month.i])
      axis(1, at = XAxis.V.n, cex.axis = 1.0, col.axis = 'blue')
      box()
      warning('.sPlotDiurnalCycleM::: No data available for month: ', month.name[Month.i], '!')
    }
}
sEddyProc$methods(.sPlotDiurnalCycleM = .sEddyProc_sPlotDiurnalCycleM)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotDiurnalCycle <- function(
    ##title<<
    ## sEddyProc$sPlotDiurnalCycle - Image with diurnal cycles of each month
    ##description<<
    ## Generates image in specified format ('pdf' or 'png') with diurnal cycles.
    Var.s               ##<< Variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Format.s = 'pdf'     ##<< Graphics file format ('pdf' or 'png')
    , Dir.s = 'plots'      ##<< Directory for plotting
) {
    ##author<<
    ## KS, AMM
    # TEST: sPlotDiurnalCycle('NEE')
    'Image with diurnal cycles of each month'
    # Open plot
    PlotType.s <- 'DC'
    WInch.n <- 15
    HInch.n <- WInch.n / 3 * 5
    PlotFile.s <- .self$.sxOpenPlot(Var.s, QFVar.s, QFValue.n, PlotType.s
                      , WInch.n, HInch.n, Format.s, Dir.s, 'sPlotDiurnalCycle')

    tryCatch({
      # Slpit the printing area in 5 lines and 3 rows
      split.screen(c(5, 1))
      split.screen(c(3, 1), screen = 1)
      split.screen(c(1, 3), screen = 2)
      split.screen(c(1, 3), screen = 3)
      split.screen(c(1, 3), screen = 4)
      split.screen(c(1, 3), screen = 5)

      # Set title of plot
      screen(6)
      mtext(.self$.sxSetTitle(Var.s, QFVar.s, QFValue.n, 'Diurnal cycles')
            , line = -3, side = 3, cex = 2.0)
      screen(8)
      mtext(sINFO$Y.NAME, line = 1, side = 3, cex = 2.0)

      # Loop over all months
      for (Month.i in 1:12) {
        screen(Month.i + 8)
        if (Month.i == 12) {
			.self$.sPlotDiurnalCycleM(Var.s, QFVar.s, QFValue.n, Month.i) #with legend
        } else {
			.self$.sPlotDiurnalCycleM(Var.s, QFVar.s, QFValue.n, Month.i, Legend.b = F)
        }
      }

      # Close plot
    }, finally = .self$.sxClosePlot(PlotFile.s))
}
sEddyProc$methods(sPlotDiurnalCycle = sEddyProc_sPlotDiurnalCycle)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Yearly half-hourly fluxes with daily means
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotHHFluxesY <- function(
    ##title<<
    ## sEddyProc$sPlotHHFluxesY -  Plot half-hourly fluxes of specified year
    ##description<<
    ## The half-hourly fluxes for a single year are plotted to the current device,
    ## scaled to all data.
    Var.s               ##<< Variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Year.i             ##<< Year to plot
) {
##author<<
    ## AMM, KS
    # TEST: sPlotHHFluxesY('NEE', 'none', NA, 1998)
    'Plot half-hourly fluxes of specified year'
    # Set plot contents
    Data.V.n <- fSetQF(cbind(sDATA, sTEMP), Var.s, QFVar.s, QFValue.n, 'sPlotHHFluxesY')
    FullYearData.F <- fExpandToFullYear(sDATA$sDateTime, Data.V.n, Year.i, sINFO$DTS, 'sPlotHHFluxesY')
    Time.V.n <- FullYearData.F$DateTime
    Plot.V.n <- FullYearData.F$Data
    # Additional line with daily mean
    DMean.V.n <- rep(apply(matrix(Plot.V.n, nrow = sINFO$DTS), 2, mean, na.rm = T), each = sINFO$DTS)

    #Scale to all data
    YMin.n <- min(Plot.V.n, na.rm = T)
    YMax.n <- max(Plot.V.n, na.rm = T)
    # Axis settings
    Julian.i <- julian(Time.V.n, origin = as.POSIXct(paste(Year.i, '01-01', sep = '-'), tz = getTZone(Time.V.n)))
    XAxis.V.n <- seq(15, 345, by = 30)

    # Plot half-hourly fluxes
    par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
    if (!sum(!is.na(Plot.V.n)) == 0) {
      # Plot
      plot(Plot.V.n ~ Julian.i, ylim = c(YMin.n, YMax.n), col = rgb(0.4, 0.4, 0.4, alpha = 0.2), pch = 20, cex = 0.3,
           axes = F, xlab = '', ylab = '', main = Year.i)
      abline(h = 0, col = 'grey')
      lines(DMean.V.n ~ Julian.i, lty = 'solid', lwd = 1, col = 'red', pch = 0, cex = 1)
      axis(1, at = XAxis.V.n, cex.axis = 1.0, col.axis = 'dark violet', labels = month.abb)
      axis(2, cex.axis = 1.0)
      box()
    } else {
      # Plot empty box
      plot(rep(0, length(Time.V.n)) ~ Julian.i, type = 'n', axes = F, xlab = '', ylab = '', main = Year.i)
      axis(1, at = XAxis.V.n, cex.axis = 1.0, col.axis = 'dark violet', labels = month.abb)
      box()
      warning('sPlotHHFluxesY::: No data available in year: ', Year.i, '!')
    }
}
sEddyProc$methods(sPlotHHFluxesY = sEddyProc_sPlotHHFluxesY)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotHHFluxes <- function(
    ### Produce image-plot with half-hourly fluxes for each year
    Var.s                  ##<< (Filled) variable to plot
    , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
    , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
    , Format.s = 'pdf'     ##<< Graphics file format ('pdf' or 'png')
    , Dir.s = 'plots'      ##<< Directory for plotting
) {
  ##details<<
  ## Generates image in specified format ('pdf' or 'png') with half-hourly fluxes
  ## and their daily means,
  ## see also \code{\link{sEddyProc_sPlotHHFluxesY}}.
  ##author<<
    ## KS, AMM
    # TEST: sPlotHHFluxes('NEE')
    'Image with half-hourly fluxes for each year'
    # Open plot
    PlotType.s <- 'Flux'
    WInch.n <- 15
    HInch.n <- WInch.n / 3 * (sINFO$Y.NUMS + 1)
    PlotFile.s <- .self$.sxOpenPlot(Var.s, QFVar.s, QFValue.n, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, 'sPlotHHFluxes')

    tryCatch({
      # Split the screen
      split.screen(c(sINFO$Y.NUMS + 1, 1))
      split.screen(c(3, 1), screen = 1)

      # Set title of plot
      screen(sINFO$Y.NUMS + 3)
      mtext(.self$.sxSetTitle(Var.s, QFVar.s, QFValue.n
        , 'Half-hourly fluxes and daily means')
        , line = -3, side = 3, cex = 2.0)

      # Loop over all years
      for (Year.i in sINFO$Y.START:sINFO$Y.END) {
        screen(Year.i - sINFO$Y.START + 1 + 1)
        sPlotHHFluxesY(Var.s, QFVar.s, QFValue.n, Year.i)
      }

      # Close plot
    }, finally = .self$.sxClosePlot(PlotFile.s))
}
sEddyProc$methods(sPlotHHFluxes = sEddyProc_sPlotHHFluxes)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Daily sums with and without uncertainties
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotDailySumsY <- function(
    ##title<<
    ## sEddyProc$sPlotDailySumsY - Plot daily sum of specified year
    ##description<<
    ## The daily sums for a single year are plotted to the current device, scaled to all data.
    ## The daily sums are only calculated for days with complete data.
    Var.s               ##<< (Filled) variable to plot
    , VarUnc.s = 'none'    ##<< Uncertainty estimates for variable
    , Year.i             ##<< Year to plot
	, timeFactor.n = 3600 * 24	##<< time conversion factor with default per second to per day
	, massFactor.n = (44.0096 / 1000000) * (12.011 / 44.0096) ##<< mass conversion factor with default from mumol CO2 to g C
	, unit.s = "gC/m2/day"	##<< resulting unit
) {
    ##author<<
    ## AMM, KS
    # TEST: sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
    'Plot daily sum of specified year'
	##description<<
	## This function first computes the everage flux for each day.
	## If the original unit is not "per day", then it need to be converted to "per day" by argument \code{timeFactor.n}.
	## Furthermore, a change of the mass unit is provided by argument \code{massFactor.n}.
	## The default parameters assume original units of mumol CO2 / m2 / second and convert to gC / m2 / day.
	## The conversion factors allow plotting variables with different units
	# Set plot contents
    Data.V.n <- fSetQF(cbind(sDATA, sTEMP), Var.s, 'none', NA, 'sPlotDailySumsY')
    FullYearData.F <- fExpandToFullYear(sDATA$sDateTime, Data.V.n, Year.i, sINFO$DTS, 'sPlotDailySumsY')
    Time.V.n <- FullYearData.F$DateTime
    Plot.V.n <- FullYearData.F$Data

    if (VarUnc.s != 'none') {
      DataSD.V.n <- fSetQF(cbind(sDATA, sTEMP), VarUnc.s, 'none', NA, 'sPlotDailySumsY')
      PlotSD.V.n <- fExpandToFullYear(sDATA$sDateTime, DataSD.V.n, Year.i, sINFO$DTS, 'sPlotDailySumsY')$Data
    } else {
      # Set uncertainties to Zero
      PlotSD.V.n <- (rep(0, length(Plot.V.n)))
    }
    # Uncertainty only used where data
    PlotSD.V.n <- ifelse(is.na(Plot.V.n), NA, PlotSD.V.n)
    # If there is data but no uncertainty estimates, an empty box will be plotted
    CountMissingUnc.n <- sum(!is.na(Plot.V.n) & is.na(PlotSD.V.n))

    # Compute daily sums
	nRecInDay <- sINFO$DTS
    DYear.V.d <- matrix(as.numeric(format(Time.V.n, '%Y')), nrow = sINFO$DTS)[1, ]
    DoY.V.d  <- matrix(as.numeric(format(Time.V.n, '%j')) , nrow = sINFO$DTS)[1, ]
	DAvg.V.d <- (1 / sINFO$DTS) * apply(matrix(Plot.V.n, nrow = nRecInDay), 2, mean)
    DSum.V.d <- DAvg.V.d * timeFactor.n * massFactor.n
    fSumOfSquares <- function(x, ...) {sum(x^2, ...)}
    #DUnc.V.d <- (1 / sINFO$DTS) * sqrt(apply(matrix(PlotSD.V.n, nrow = sINFO$DTS), 2, fSumOfSquares))
	# twutz: 160729:  * timeFactor.n * massFactor.n
	DUnc.V.d <- (1 / nRecInDay) * sqrt(apply(matrix(PlotSD.V.n, nrow = nRecInDay), 2, fSumOfSquares)) * timeFactor.n * massFactor.n

    # Scale to all data
    YMin.n <- min(DSum.V.d - DUnc.V.d, na.rm = T)
    YMax.n <- max(DSum.V.d + DUnc.V.d, na.rm = T)
    # Axis settings
    XAxis.V.n <- seq(15, 345, by = 30)

    # Plot daily sums
    par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
    if (!sum(!is.na(DSum.V.d)) == 0 && CountMissingUnc.n == 0) {
      # Plot
      plot(DSum.V.d ~ DoY.V.d, type = 'n', ylim = c(YMin.n, YMax.n),
           axes = F, xlab = '', ylab = '', main = Year.i)
   	  mtext(unit.s, 2, 2.2)

      if (VarUnc.s != 'none') {
        t.b <- !is.na(DUnc.V.d) #Polygons sensitive to NAs
        polygon(c(DoY.V.d[t.b], rev(DoY.V.d[t.b])), c(DSum.V.d[t.b] + DUnc.V.d[t.b], rev(DSum.V.d[t.b] - DUnc.V.d[t.b])),
                col = 'dark grey', border = NA)
      }
      abline(h = 0, col = 'grey')
      lines(DSum.V.d, lty = 'solid', lwd = 1, col = 'dark green')
      points(DSum.V.d, pch = 20, cex = 0.7, col = 'dark green')
      axis(1, at = XAxis.V.n, cex.axis = 1.0, labels = month.abb, col.axis = 'dark violet')
      axis(2, cex.axis = 1.0)
      box()
    } else {
      # Plot empty box
      plot(rep(0, length(DSum.V.d)) ~ DoY.V.d, type = 'n', axes = F, xlab = '', ylab = '', main = Year.i)
      axis(1, at = XAxis.V.n, cex.axis = 1.0, labels = month.abb, col.axis = 'dark violet')
      box()
      if (CountMissingUnc.n != 0) {
        warning('sPlotDailySumsY::: Uncertainty estimates missing for ', CountMissingUnc.n, ' data points of ', Var.s,
                ' in year: ', Year.i, 'This will cause an empty plot!')
      } else {
        warning('sPlotDailySumsY::: Missing data in year: ', Year.i, '!')
      }
    }
}
sEddyProc$methods(sPlotDailySumsY = sEddyProc_sPlotDailySumsY)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotDailySums <- function(
    ##title<<
    ## sEddyProc$sPlotDailySums - Image with daily sums of each year
    ##description<<
    ## Generates image in specified format ('pdf' or 'png') with daily sums, see also \code{\link{sEddyProc_sPlotDailySumsY}}.
    Var.s               ##<< (Filled) variable to plot
    , VarUnc.s = 'none'    ##<< Uncertainty estimates for variable
    , Format.s = 'pdf'     ##<< Graphics file format ('pdf' or 'png')
    , Dir.s = 'plots'      ##<< Directory for plotting
	, unit.s = 'gC/m2/day' ##<< unit of the daily sums
	, ...				##<< further arguments to \code{\link{sEddyProc_sPlotDailySumsY}}, such as \code{timeFactor.n} and \code{massFactor.n}.
) {
    ##author<<
    ## KS, AMM
    # TEST: sPlotDailySums('NEE'); sPlotDailySums('NEE_f', 'NEE_fsd')
    'Image with daily sums of each year'
    # Open plot
    PlotType.s <- if (VarUnc.s == 'none') 'DSum' else 'DSumU'
    WInch.n <- 15
    HInch.n <- WInch.n / 3 * (sINFO$Y.NUMS + 1)
    PlotFile.s <- .self$.sxOpenPlot(Var.s, 'none', NA, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, 'sPlotDailySums')

    tryCatch({
      # Split screen
      split.screen(c(sINFO$Y.NUMS + 1, 1))
      split.screen(c(3, 1), screen = 1)

      # Set title of plot
      screen(sINFO$Y.NUMS + 3)
      if (VarUnc.s == 'none') {
        mtext(.self$.sxSetTitle(Var.s, 'none', NA, 'Daily sums', unit.s = unit.s), line = -3, side = 3, cex = 2.0)
      } else {
        mtext(.self$.sxSetTitle(Var.s, 'none', NA, 'Daily sums with uncertainties', unit.s = unit.s), line = 1, side = 3, cex = 2.0)
	}

      # Loop over all years
      for (Year.i in sINFO$Y.START:sINFO$Y.END) {
        screen(Year.i - sINFO$Y.START + 1 + 1)
        sPlotDailySumsY(Var.s, VarUnc.s, Year.i, unit.s = unit.s, ...)
      }

      # Close plot
    }, finally = .self$.sxClosePlot(PlotFile.s))
}
sEddyProc$methods(sPlotDailySums = sEddyProc_sPlotDailySums)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ NEE vs UStar for diagnosing uStar Threshold estimation
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotNEEVersusUStarForSeason <- function(
	##title<<
  	## sEddyProc$sPlotNEEVersusUStarForSeason - Image with NEE versus UStar for each Temperature class of given season
  	##description<<
  	## Generates image in specified format ('pdf' or 'png')
  season = levels(sDATA$season)[1]	 ##<< string of season, i.e. time period, to plot
  , format = 'pdf'     ##<< string of Graphics file format ('pdf' or 'png')
  , dir = 'plots'      ##<< string of Directory for plotting
  , UstarColName = "Ustar"		##<< column name for UStar
  , NEEColName = "NEE"			##<< column name for NEE
  , TempColName = "Tair"		##<< column name for air temperature
  , WInch = 16 * 0.394		##<< width of the plot in inches, defaults to 16cm
  , HInchSingle = 6 * 0.394	##<< height of a subplot in inches, defaults to 6cm
  , ...						##<< other arguments to \code{.plotNEEVersusUStarTempClass}, such as xlab and ylab axis label strings
) {
  ##author<< TW
  'Image with daily sums of each year'
  # generate subset of data
  dsSeason <- subset(.self$sDATA, season == season)
  tempBinLevels <- sort(unique(dsSeason$tempBin))
  # Open plot
  PlotType.s <- paste('NEEvsUStar', season, sep = "_")
  HInch <- HInchSingle * (length(tempBinLevels) + 1)
  PlotFile.s <- .self$.sxOpenPlot('none', 'none', NA, PlotType.s, WInch, HInch, format, dir, 'sPlotNEEVersusUStarForSeason')

  tryCatch({
			  # Split screen
  	  split.screen(c(length(tempBinLevels) + 1, 1))
		  split.screen(c(3, 1), screen = 1)
		  # Set title of plot
		  screen(length(tempBinLevels) + 3)
		  mtext(.self$.sxSetTitle('NEE', 'none', NA, paste('NEE versus uStar for season', season)), line = -3, side = 3, cex = 1.1)

		  # Loop over all temperature classes
		  # tempBinI <- 1L
		  for (tempBinI in seq_along(tempBinLevels) ) {
			  screen(1L + tempBinI)
			  tempBinLevel <- tempBinLevels[tempBinI]
			  uStarTh <- sUSTAR$tempInSeason[tempBinLevel, season]
			  dss <- filter_(dsSeason,  ~tempBin == tempBinLevel)
			  par(las = 1)                   #also y axis labels horizontal
			  par(mar = c(2.0, 3.3, 0, 0) + 0.3)  #margins
			  par(tck = 0.02)                          #axe-tick length inside plots
			  par(mgp = c(1.1, 0.2, 0) )  #positioning of axis title, axis labels, axis
			  par(cex = 10 / 12)			# default font size 10pt
			  .plotNEEVersusUStarTempClass(dss, uStarTh, UstarColName = UstarColName, NEEColName = NEEColName, TempColName = TempColName, ...)
		  }

		  # Close plot
			  }, finally = .self$.sxClosePlot(PlotFile.s))
}
sEddyProc$methods(sPlotNEEVersusUStarForSeason = sEddyProc_sPlotNEEVersusUStarForSeason)


.tmp.f <- function() {
  if (!exists("Example_DETha98")) load("data / Example_DETha98.RData")
  EddyData.F <- Example_DETha98
  EddyData.F <- cbind(EddyData.F, VPD =
                        fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year'
                                             , Day.s = 'DoY', Hour.s = 'Hour')
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with the variables needed for processing later
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F,
                              c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
  uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh
  # plot saturation of NEE with UStar for one season -> in directory plots
  EddyProc.C$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[3] )
}

