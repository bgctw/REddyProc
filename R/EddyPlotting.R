#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for plotting +++
#+++ fingerprint, diurnal cycle, half-hourly fluxes, and daily sums +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Internal helper functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
    attr(data[, Var.s], 'units')
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
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
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
) {
  ##author<<
  ## AMM, KS, TW
  'Open graphics device.'
  # Check if variable names exist and numeric before opening plot
  SubCallFunc.s <- paste(CallFunction.s, '.self$.sxOpenPlot', sep = ':::')
  fCheckColNames(data, c(Var.s, QFVar.s), SubCallFunc.s)
  fCheckColNum(data, c(Var.s, QFVar.s), SubCallFunc.s)

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
    # see http://tolstoy.newcastle.edu.au/R/e17/devel/12/01/0128.html
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

#' @export
sEddyProc_sPlotFingerprintY <- function(
  ### Plot fingerprint for a single year scaled to all data.
  Var = Var.s               ##<< Variable to plot
  , QFVar = 'none'     ##<<
  ## Quality flag of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_ ##<<
  ## Value of quality flag for data to plot
  , Year = Year.i             ##<< Year to plot
  , onlyLegend = if (!missing(Legend.b)) Legend.b else  F         ##<< Plot
  ## only legend
  , colors =	if (!missing(Col.V)) Col.V else 	##<< Color palette
    ## for fingerprint plot
    ## (can be also defined by user), i.e. color scale argument to
    ## \code{\link{image}}
    colorRampPalette(c(
      '#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F'
      , 'yellow', '#FF7F00', 'red', '#7F0000'))(50)
  , valueLimits = range(Plot.V.n, na.rm = TRUE)	##<< values outside this range
  ## will be set to the range borders to avoid distorting colour scale
  ## e.g. \code{valueLimits = quantile(EddyProc.C$sDATA$NEE, prob = c(
  ## 0.05, 0.95), na.rm = TRUE)}
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
  , dts = sINFO$DTS              ##<< numeric integer of hours in day
  , Var.s   ##<< deprecated
  , QFVar.s ##<< deprecated
  , QFValue.n ##<< deprecated
  , Year.i      ##<< deprecated
  , Legend.b ##<< deprecated
  , Col.V ##<< deprecated
) {
  if (!missing(QFVar.s)) QFVar <- QFVar.s
  varNamesDepr <- c("Var.s","QFVar.s","QFValue.n","Year.i","Legend.b","Col.V")
  varNamesNew <- c("Var","QFVar","QFValue","Year","onlyLegend","colors")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n),missing(Year.i)
    ,missing(Legend.b),missing(Col.V)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<<
  ## AMM, KS, TW
  # Set plot contents
  Data.V.n <- fSetQF(data, Var, QFVar, QFValue, 'sPlotFingerprintY')
  # Scale to all data
  YMin.n <- min(Data.V.n, na.rm = T)
  YMax.n <- max(Data.V.n, na.rm = T)
  #Set yearly data
  FullYearData.F <- fExpandToFullYear(
    data$sDateTime, Data.V.n, Year, dts, 'sPlotFingerprintY')
  Time.V.n <- FullYearData.F$DateTime
  Plot.V.n <- FullYearData.F$Data
  Plot.V.n[!is.finite(Plot.V.n)] <- NA
  # set outliers to range limits in order to not distort colour scale
  Plot.V.n <- pmax(pmin(Plot.V.n, valueLimits[2]), valueLimits[1])
  YMin.n <- max(YMin.n, min(Plot.V.n, na.rm = T))
  YMax.n <- min(YMax.n, max(Plot.V.n, na.rm = T))
  # Calculate plot parameters
  XAxis.V.n <- seq(0, 24, by = 2)
  YAxis.V.n <- seq(15, 345, by = 30)
  # Daily sequence of DoY
  DoY.V.d  <- c(0:max(as.numeric(format(Time.V.n, '%j')), na.rm = T))
  # Plot
  Month1st.P <- as.POSIXct(paste(format(
    min(FullYearData.F$DateTime), '%Y-%m-'),'01',sep = '')
    , tz = attr(FullYearData.F$DateTime,"tzone"))
  if (!sum(!is.na(Plot.V.n)) == 0 && onlyLegend == F) {
    # Plot fingerprint
    par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
    image(
      seq(0, 24, by = (24 / dts)), DoY.V.d
      , matrix(Plot.V.n, nrow = dts)
      , zlim = c(YMin.n, YMax.n)
      , col = colors
      , axes = F, xlab = '', ylab = '', main = Year)
    axis(1, at = XAxis.V.n, cex.axis = 1.0, tck = 0.03, col.axis = 'blue')
    axis(2, at = as.POSIXlt(
      seq(Month1st.P, max(FullYearData.F$DateTime), by = "month"))$yday
      , cex.axis = 1.0, tck = 0.03
      , labels = month.abb, padj = 1, col.axis = 'dark violet')
    # axis(2, at = YAxis.V.n, cex.axis = 1.0, tck = 0.03
    #     , labels = month.abb, padj = 1, col.axis = 'dark violet')
    box()
  } else if (onlyLegend == F) {
    #Plot empy box
    par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
    image(
      seq(0, 24, by = (24 / dts)), DoY.V.d
      , matrix(Plot.V.n, nrow = dts)
      , zlim = c(0, 1)
      , col = colors
      , axes = F, xlab = '', ylab = '', main = Year)
    axis(1, at = XAxis.V.n, cex.axis = 1.0, tck = 0.03, col.axis = 'blue')
    axis(2, at = as.POSIXlt(
      seq(Month1st.P, max(FullYearData.F$DateTime), by = "month"))$yday
      , cex.axis = 1.0, tck = 0.03
      , labels = month.abb, padj = 1, col.axis = 'dark violet')
    box()
    warning('sPlotFingerprintY::: No data available for year: ', Year, '!')
  } else {#Plot legend and title
    Title.s <- .self$.sxSetTitle(Var, QFVar, QFValue, 'Fingerprint')
    Seq.V.n <- seq(YMin.n, YMax.n, by = (YMax.n - YMin.n) / (length(colors) - 1))
    par(mai = c(3, 1, 3, 1))
    image(
      Seq.V.n, c(0, 1)
      , matrix(Seq.V.n, ncol = 1)
      , col = colors
      , zlim = c(YMin.n, YMax.n)
      , xlab = Var, yaxt = 'n', ylab = '', main = Title.s)
    box()
  }
}
sEddyProc$methods(sPlotFingerprintY = sEddyProc_sPlotFingerprintY)


#' @export
sEddyProc_sPlotFingerprint <- function(
  ### Generates fingerprint in file
  Var = Var.s               ##<< Variable to plot
  , QFVar = 'none'     ##<< Quality flag of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_ ##<< Value of
  ## quality flag for data to plot
  , Format = if (!missing(Format.s)) Format.s else 'pdf'     ##<<
  ## Graphics file format (e.g. 'pdf', 'png')
  , Dir = if (!missing(Dir.s)) Dir.s else 'plots'      ##<< Directory
  ## for plotting
  , ...				##<< further arguments to \code{\link{sEddyProc_sPlotFingerprintY}}
  , Var.s               ##<< Variable to plot
  , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
  , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
  , Format.s = 'pdf'     ##<< Graphics file format (e.g. 'pdf', 'png')
  , Dir.s = 'plots'      ##<< Directory for plotting
) {
  if (!missing(QFVar.s)) QFVar <- QFVar.s
  varNamesDepr <- c("Var.s","QFVar.s","QFValue.n","Format.s","Dir.s")
  varNamesNew <- c("Var","QFVar","QFValue","Format","Dir")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n)
    ,missing(Format.s),missing(Dir.s)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< KS, AMM
  ##alias<< sPlotFingerprint
  # TEST: sPlotFingerprint('NEE'); sPlotFingerprint('NEE_f', 'NEE_fqc', 1)
  'Image with fingerprints of each year'
  # Calculate number of screens and width and heigth
  Screens.n <- (sINFO$Y.NUMS + 3) %/% 3
  WInch.n <- 15 #Needs to be this big to have enough space for margins
  HInch.n <- WInch.n / 2 * Screens.n

  # Open plot
  PlotType.s <- 'FP'
  PlotFile.s <- .self$.sxOpenPlot(
    Var, QFVar, QFValue, PlotType.s, WInch.n, HInch.n, Format, Dir
    , 'sPlotFingerprint')
  on.exit(.self$.sxClosePlot(PlotFile.s))
  #Split into Screens.n screens with 3 columns
  split.screen(c(Screens.n, 3))
  for (Year in sINFO$Y.START:sINFO$Y.END) {
    screen(Year - sINFO$Y.START + 1)
    sPlotFingerprintY(Var, QFVar, QFValue, Year, ...)
  }
  screen(sINFO$Y.END - sINFO$Y.START + 2)
  sPlotFingerprintY(Var, QFVar, QFValue, sINFO$Y.END, onlyLegend = T, ...)
}
sEddyProc$methods(sPlotFingerprint = sEddyProc_sPlotFingerprint)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Diurnal cycles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
.sEddyProc_sPlotDiurnalCycleM <- function(
  ### The diurnal cycles of a single month are potted to the current device,
  ### scaled to all data. Each year is plotted as a different (coloured) line.
  Var = Var.s               ##<< Variable to plot
  , QFVar = if (!missing(QFVar.s)) QFVar.s else 'none'     ##<<
  ## Quality flag of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_ ##<<
  ## Value of quality flag for data to plot
  , Month = Month.i            ##<< Month to plot
  , Legend = if (!missing(Legend.b)) Legend.b else T         ##<<
  ## Plot with legend
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
  , dts = sINFO$DTS              ##<< numeric integer
  , Var.s               ##<< Variable to plot
  , QFVar.s = 'none'     ##<< Quality flag of variable to be filled
  , QFValue.n = NA_real_ ##<< Value of quality flag for data to plot
  , Month.i            ##<< Month to plot
  , Legend.b = T         ##<< Plot with legend
) {
  varNamesDepr <- c("Var.s","QFVar.s","QFValue.n","Month.i","Legend.b")
  varNamesNew <- c("Var","QFVar","QFValue","Month","Legend")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n),missing(Month.i)
    ,missing(Legend.b)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<<
  ## AMM, KS
  # TEST: .sPlotDiurnalCycleM('NEE', 'none', NA, 10)
  'Plot diurnal cycles of specified month'
  # Set plot contents
  # Diurnal cycles
  Plot.V.n <- fSetQF(data, Var, QFVar, QFValue, '.sPlotDiurnalCycleM')
  Month.V.d <- matrix(as.numeric(format(data$sDateTime, '%m')), nrow = dts)[1, ]
  DYear.V.d <- matrix(as.numeric(format(data$sDateTime, '%Y')), nrow = dts)[1, ]
  Plot.M.n <- matrix(Plot.V.n, ncol = dts, byrow = T)
  # Average all years: Mean.M.m <- as.matrix(aggregate(
  # Plot.M.n, by = list(Month.V.d), FUN = mean, simplify = T, na.rm = T)[, -1])
  Mean.F.m <- aggregate(
    Plot.M.n, by = list(Year = DYear.V.d, Month = Month.V.d), FUN = mean
    , simplify = T, na.rm = T)

  # Scale to all data
  YMin.n <- min(Mean.F.m[, c(-1, -2)], na.rm = T)
  YMax.n <- max(Mean.F.m[, c(-1, -2)], na.rm = T)
  # Axis settings
  XAxis.V.n <- seq(0, 24, by = 2)

  # Plot diurnal cycles
  par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
  if (sum(!is.na(Mean.F.m[Mean.F.m$Month == Month, c(-1, -2)])) > 0) {
    # Plot
    plot(
      rep(0, dts)~seq(0.0, 23.5, by = (24 / dts)), type = 'n'
      , axes = F, xlab = '', ylab = '', ylim = c(YMin.n, YMax.n)
      , main = month.name[Month])
    for (Year in sINFO$Y.START:sINFO$Y.END) {
      MeanY.V.m <- as.numeric(
        Mean.F.m[Mean.F.m$Year == Year & Mean.F.m$Month == Month, c(-1, -2)])
      points(
        MeanY.V.m ~ seq(0.0, 23.5, by = (24 / dts)), type = 'o', lty = 'solid'
        , lwd = 1, col = (Year - sINFO$Y.START) + 2, pch = 20, cex = 1)
    }
    abline(h = 0, col = 'grey')
    axis(1, at = XAxis.V.n, cex.axis = 0.9, col.axis = 'blue')
    axis(2, cex.axis = 1.0)
    if (Legend) legend(
      'bottomright', legend = c(sINFO$Y.START:sINFO$Y.END), lty = 'solid'
      , col = ((sINFO$Y.START:sINFO$Y.END) - sINFO$Y.START) + 2)
    box()
  } else {
    # Plot empty box
    plot(
      rep(0, dts)~seq(0.0, 23.5, by = (24 / dts)), type = 'n', axes = F
      , xlab = '', ylab = '', main = month.name[Month])
    axis(1, at = XAxis.V.n, cex.axis = 1.0, col.axis = 'blue')
    box()
    warning(
      '.sPlotDiurnalCycleM::: No data available for month: '
      , month.name[Month], '!')
  }
}
sEddyProc$methods(.sPlotDiurnalCycleM = .sEddyProc_sPlotDiurnalCycleM)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotDiurnalCycle <- function(
  ### Generates image in specified format ('pdf' or 'png') with diurnal cycles.
  Var = Var.s               ##<< Variable to plot
  , QFVar = if (!missing(QFVar.s)) QFVar.s else 'none'     ##<< Quality
  ##  flag of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_ ##<< Value
  ##  of quality flag for data to plot
  , Format = if (!missing(Format.s)) Format.s else 'pdf'     ##<< Graphics
  ## file format (e.g. 'pdf', 'png')
  , Dir = if (!missing(Dir.s)) Dir.s else 'plots'   ##<< Directory for plotting
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
  , dts = sINFO$DTS              ##<< numeric integer
  , Var.s   ##<< deprecated
  , QFVar.s ##<< deprecated
  , QFValue.n ##<< deprecated
  , Format.s ##<< deprecated
  , Dir.s ##<< deprecated
) {
  varNamesDepr <- c("Var.s","QFVar.s","QFValue.n","Format.s","Dir.s")
  varNamesNew <- c("Var","QFVar","QFValue","Format","Dir")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n)
    ,missing(Format.s),missing(Dir.s)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< KS, AMM
  # Open plot
  PlotType.s <- 'DC'
  WInch.n <- 15
  HInch.n <- WInch.n / 3 * 5
  PlotFile.s <- .self$.sxOpenPlot(
    Var, QFVar, QFValue, PlotType.s, WInch.n, HInch.n, Format, Dir
    , 'sPlotDiurnalCycle')

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
    mtext(.self$.sxSetTitle(Var, QFVar, QFValue, 'Diurnal cycles')
          , line = -3, side = 3, cex = 2.0)
    screen(8)
    mtext(sINFO$Y.NAME, line = 1, side = 3, cex = 2.0)

    # Loop over all months
    for (Month in 1:12) {
      screen(Month + 8)
      if (Month == 12) {
        .self$.sPlotDiurnalCycleM(Var, QFVar, QFValue, Month) #with legend
      } else {
        .self$.sPlotDiurnalCycleM(Var, QFVar, QFValue, Month, Legend = F)
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
  ### Plot half-hourly fluxes for a single year scaled to all data.
  Var = Var.s               ##<< Variable to plot
  , QFVar = if (!missing(QFVar.s)) QFVar.s else 'none'     ##<<
  ## Quality flag of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_ ##<<
  ## Value of quality flag for data to plot
  , Year = Year.i             ##<< Year to plot
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
  , dts = sINFO$DTS              ##<< numeric integer
  , Var.s   ##<< deprecated
  , QFVar.s ##<< deprecated
  , QFValue.n ##<< deprecated
  , Year.i      ##<< deprecated
) {
  if (!missing(QFVar.s)) QFVar <- QFVar.s
  varNamesDepr <- c("Var.s","QFVar.s","QFValue.n","Year.i")
  varNamesNew <- c("Var","QFVar","QFValue","Year")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n),missing(Year.i)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< AMM, KS
  # Set plot contents
  Data.V.n <- fSetQF(data, Var, QFVar, QFValue.n, 'sPlotHHFluxesY')
  FullYearData.F <- fExpandToFullYear(
    data$sDateTime, Data.V.n, Year, dts, 'sPlotHHFluxesY')
  Time.V.n <- FullYearData.F$DateTime
  Plot.V.n <- FullYearData.F$Data
  # Additional line with daily mean
  DMean.V.n <- rep(
    apply(matrix(Plot.V.n, nrow = dts), 2, mean, na.rm = T), each = dts)
  #Scale to all data
  YMin.n <- min(Plot.V.n, na.rm = T)
  YMax.n <- max(Plot.V.n, na.rm = T)
  # Axis settings
  Julian.i <- julian(Time.V.n, origin = as.POSIXct(
    paste(Year, '01-01', sep = '-'), tz = getTZone(Time.V.n)))
  XAxis.V.n <- seq(15, 345, by = 30)

  # Plot half-hourly fluxes
  par(mai = c(0.7, 0.7, 0.7, 0.4)) #Set margin
  if (!sum(!is.na(Plot.V.n)) == 0) {
    # Plot
    plot(
      Plot.V.n ~ Julian.i, ylim = c(YMin.n, YMax.n)
      , col = rgb(0.4, 0.4, 0.4, alpha = 0.2), pch = 20, cex = 0.3
      , axes = F, xlab = '', ylab = '', main = Year)
    abline(h = 0, col = 'grey')
    lines(
      DMean.V.n ~ Julian.i, lty = 'solid', lwd = 1, col = 'red', pch = 0, cex = 1)
    axis(
      1, at = XAxis.V.n, cex.axis = 1.0, col.axis = 'dark violet'
      , labels = month.abb)
    axis(2, cex.axis = 1.0)
    box()
  } else {
    # Plot empty box
    plot(
      rep(0, length(Time.V.n)) ~ Julian.i, type = 'n', axes = F
      , xlab = '', ylab = '', main = Year)
    axis(
      1, at = XAxis.V.n, cex.axis = 1.0, col.axis = 'dark violet'
      , labels = month.abb)
    box()
    warning('sPlotHHFluxesY::: No data available in year: ', Year, '!')
  }
}
sEddyProc$methods(sPlotHHFluxesY = sEddyProc_sPlotHHFluxesY)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotHHFluxes <- function(
  ### Produce image-plot with half-hourly fluxes for each year
  Var = Var.s               ##<< Variable to plot
  , QFVar = if (!missing(QFVar.s)) QFVar.s else 'none'     ##<< Quality flag
  ## of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_ ##<< Value
  ## of quality flag for data to plot
  , Format = if (!missing(Format.s)) Format.s else 'pdf'     ##<< Graphics
  ## file format (e.g. 'pdf', 'png')
  , Dir = if (!missing(Dir.s)) Dir.s else 'plots'   ##<< Directory for plotting
  , Var.s   ##<< deprecated
  , QFVar.s ##<< deprecated
  , QFValue.n ##<< deprecated
  , Format.s ##<< deprecated
  , Dir.s ##<< deprecated
) {
  varNamesDepr <- c("Var.s","QFVar.s","QFValue.n","Format.s","Dir.s")
  varNamesNew <- c("Var","QFVar","QFValue","Format","Dir")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n)
    ,missing(Format.s),missing(Dir.s)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##details<<
  ## Generates image in specified format ('pdf' or 'png') with
  ## half-hourly fluxes and their daily means,
  ## see also \code{\link{sEddyProc_sPlotHHFluxesY}}.
  ##author<<
  ## KS, AMM
  # TEST: sPlotHHFluxes('NEE')
  'Image with half-hourly fluxes for each year'
  # Open plot
  PlotType.s <- 'Flux'
  WInch.n <- 15
  HInch.n <- WInch.n / 3 * (sINFO$Y.NUMS + 1)
  PlotFile.s <- .self$.sxOpenPlot(
    Var, QFVar, QFValue.n, PlotType.s, WInch.n, HInch.n, Format, Dir
    , 'sPlotHHFluxes')
  on.exit(.self$.sxClosePlot(PlotFile.s))
  # Split the screen
  split.screen(c(sINFO$Y.NUMS + 1, 1))
  split.screen(c(3, 1), screen = 1)
  # Set title of plot
  screen(sINFO$Y.NUMS + 3)
  mtext(.self$.sxSetTitle(
    Var, QFVar, QFValue.n, 'Half-hourly fluxes and daily means')
    , line = -3, side = 3, cex = 2.0)
  # Loop over all years
  for (Year in sINFO$Y.START:sINFO$Y.END) {
    screen(Year - sINFO$Y.START + 1 + 1)
    sPlotHHFluxesY(Var, QFVar, QFValue.n, Year)
  }
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
  ## The daily sums for a single year are plotted to the current device,
  ## scaled to all data.
  ## The daily sums are only calculated for days with complete data.
  Var = Var.s               ##<< (Filled) variable to plot
  , VarUnc = 'none'    ##<< Uncertainty
  ## estimates for variable
  , Year = Year.i             ##<< Year to plot
  , timeFactor = if (!missing(timeFactor.n)) timeFactor.n else 3600 * 24	##<< time
  ## conversion factor with default per second to per day
  , massFactor = if (!missing(massFactor.n)) massFactor.n else ##<< mass
    ## conversion factor with default from mumol CO2 to g C
    (44.0096 / 1000000) * (12.011 / 44.0096)
  , unit = if (!missing(unit.s)) unit.s else 'gC/m2/day' ##<< unit
  ##  of the daily sums
  , data = cbind(sDATA, sTEMP)   ##<< data.frame with variables to plot
  , dts = sINFO$DTS              ##<< numeric integer
  , Var.s   ##deprecated
  , VarUnc.s   ##deprecated
  , Year.i   ##deprecated
  , timeFactor.n   ##deprecated
  , massFactor.n    ##deprecated
  , unit.s    ##deprecated
) {
  if (!missing(VarUnc.s)) VarUnc <- VarUnc.s
  varNamesDepr <- c("Var.s","VarUnc.s","Year.i","timeFactor.n","massFactor.n","unit.s")
  varNamesNew <- c("Var","VarUnc","Year","timeFactor","massFactor","unit")
  iDepr = which(!c(
    missing(Var.s),missing(VarUnc.s),missing(Year.i),missing(timeFactor.n)
    ,missing(massFactor.n),missing(unit.s)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< AMM, KS
  ##description<<
  ## This function first computes the average flux for each day.
  ## If the original unit is not "per day", then it need to be converted to
  ## "per day" by argument \code{timeFactor}.
  ## Furthermore, a change of the mass unit is provided by argument
  ## \code{massFactor}.
  ## The default parameters assume original units of mumol CO2 / m2 / second
  ## and convert to gC / m2 / day.
  ## The conversion factors allow plotting variables with different units
  # Set plot contents
  Data.V.n <- fSetQF(data, Var, 'none', NA, 'sPlotDailySumsY')
  FullYearData.F <- fExpandToFullYear(
    data$sDateTime, Data.V.n, Year, dts, 'sPlotDailySumsY')
  Time.V.n <- FullYearData.F$DateTime
  Plot.V.n <- FullYearData.F$Data

  if (VarUnc != 'none') {
    DataSD.V.n <- fSetQF(data, VarUnc, 'none', NA, 'sPlotDailySumsY')
    PlotSD.V.n <- fExpandToFullYear(
      data$sDateTime, DataSD.V.n, Year, dts, 'sPlotDailySumsY')$Data
  } else {
    # Set uncertainties to Zero
    PlotSD.V.n <- (rep(0, length(Plot.V.n)))
  }
  # Uncertainty only used where data
  PlotSD.V.n <- ifelse(is.na(Plot.V.n), NA, PlotSD.V.n)
  # If there is data but no uncertainty estimates, an empty box will be plotted
  CountMissingUnc.n <- sum(!is.na(Plot.V.n) & is.na(PlotSD.V.n))
  # Compute daily sums
  nRecInDay <- dts
  DYear.V.d <- matrix(as.numeric(format(Time.V.n, '%Y')), nrow = dts)[1, ]
  DoY.V.d  <- matrix(as.numeric(format(Time.V.n, '%j')) , nrow = dts)[1, ]
  DAvg.V.d <- (1 / dts) * apply(matrix(Plot.V.n, nrow = nRecInDay), 2, mean)
  DSum.V.d <- DAvg.V.d * timeFactor * massFactor
  fSumOfSquares <- function(x, ...) {sum(x^2, ...)}
  #DUnc.V.d <- (1 / dts) * sqrt(
  # apply(matrix(PlotSD.V.n, nrow = dts), 2, fSumOfSquares))
  # twutz: 160729:  * timeFactor * massFactor
  DUnc.V.d <- (1 / nRecInDay) * sqrt(
    apply(matrix(PlotSD.V.n, nrow = nRecInDay), 2, fSumOfSquares)) *
    timeFactor * massFactor

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
         axes = F, xlab = '', ylab = '', main = Year)
    mtext(unit, 2, 2.2)

    if (VarUnc != 'none') {
      t.b <- !is.na(DUnc.V.d) #Polygons sensitive to NAs
      polygon(
        c(DoY.V.d[t.b], rev(DoY.V.d[t.b]))
        , c(DSum.V.d[t.b] + DUnc.V.d[t.b], rev(DSum.V.d[t.b] - DUnc.V.d[t.b]))
        , col = 'dark grey', border = NA)
    }
    abline(h = 0, col = 'grey')
    lines(DSum.V.d, lty = 'solid', lwd = 1, col = 'dark green')
    points(DSum.V.d, pch = 20, cex = 0.7, col = 'dark green')
    axis(
      1, at = XAxis.V.n, cex.axis = 1.0, labels = month.abb
      , col.axis = 'dark violet')
    axis(2, cex.axis = 1.0)
    box()
  } else {
    # Plot empty box
    plot(
      rep(0, length(DSum.V.d)) ~ DoY.V.d, type = 'n', axes = F, xlab = ''
      , ylab = '', main = Year)
    axis(
      1, at = XAxis.V.n, cex.axis = 1.0, labels = month.abb
      , col.axis = 'dark violet')
    box()
    if (CountMissingUnc.n != 0) {
      warning(
        'sPlotDailySumsY::: Uncertainty estimates missing for '
        , CountMissingUnc.n, ' data points of ', Var
        , ' in year: ', Year, 'This will cause an empty plot!')
    } else {
      warning('sPlotDailySumsY::: Missing data in year: ', Year, '!')
    }
  }
}
sEddyProc$methods(sPlotDailySumsY = sEddyProc_sPlotDailySumsY)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotDailySums <- function(
  ##title<<
  ## sEddyProc$sPlotDailySums - Image with daily sums of each year
  ##description<<
  ## Generates image in specified format ('pdf' or 'png') with daily sums,
  ## see also \code{\link{sEddyProc_sPlotDailySumsY}}.
  Var = Var.s               ##<< (Filled) variable to plot
  , VarUnc = 'none'    ##<< Uncertainty
  ## estimates for variable
  , Format = if (!missing(Format.s)) Format.s else 'pdf'     ##<< Graphics
  ##  file format ('pdf' or 'png')
  , Dir = if (!missing(Dir.s)) Dir.s else 'plots'      ##<< Directory
  ##  for plotting
  , unit = if (!missing(unit.s)) unit.s else 'gC/m2/day' ##<< unit
  ##  of the daily sums
  , ...				##<< further arguments to \code{\link{sEddyProc_sPlotDailySumsY}},
  ## such as \code{timeFactor} and \code{massFactor}.
  , Var.s ##<< deprecated
  , VarUnc.s ##<< deprecated
  , Format.s ##<< deprecated
  , Dir.s ##<< deprecated
  , unit.s ##<< deprecated
) {
  if (!missing(VarUnc.s)) VarUnc <- VarUnc.s
  varNamesDepr <- c("Var.s","VarUnc.s","Format.s","Dir.s","unit.s")
  varNamesNew <- c("Var","VarUnc","Format","Dir","unit")
  iDepr = which(!c(
    missing(Var.s),missing(VarUnc.s),missing(Format.s),missing(Dir.s)
    ,missing(unit.s)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< KS, AMM
  # Open plot
  PlotType.s <- if (VarUnc == 'none') 'DSum' else 'DSumU'
  WInch.n <- 15
  HInch.n <- WInch.n / 3 * (sINFO$Y.NUMS + 1)
  PlotFile.s <- .self$.sxOpenPlot(
    Var, 'none', NA, PlotType.s, WInch.n, HInch.n, Format
    , Dir, 'sPlotDailySums')
  on.exit(.self$.sxClosePlot(PlotFile.s))
  # Split screen
  split.screen(c(sINFO$Y.NUMS + 1, 1))
  split.screen(c(3, 1), screen = 1)

  # Set title of plot
  screen(sINFO$Y.NUMS + 3)
  if (VarUnc == 'none') {
    mtext(
      .self$.sxSetTitle(Var, 'none', NA, 'Daily sums', unit.s = unit)
      , line = -3, side = 3, cex = 2.0)
  } else {
    mtext(
      .self$.sxSetTitle(
        Var, 'none', NA, 'Daily sums with uncertainties', unit.s = unit)
      , line = 1, side = 3, cex = 2.0)
  }
  # Loop over all years
  for (Year in sINFO$Y.START:sINFO$Y.END) {
    screen(Year - sINFO$Y.START + 1 + 1)
    sPlotDailySumsY(Var, VarUnc, Year, unit = unit, ...)
  }

  # Close plot
}
sEddyProc$methods(sPlotDailySums = sEddyProc_sPlotDailySums)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ NEE vs UStar for diagnosing uStar Threshold estimation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sPlotNEEVersusUStarForSeason <- function(
  ### Generates image in specified format ('pdf' or 'png')
  season = levels(data$season)[1]	 ##<< string of season, i.e. time period to plot
  , format = 'pdf'     ##<< string of Graphics file format ('pdf' or 'png')
  , dir = 'plots'      ##<< string of Directory for plotting
  , UstarColName = "Ustar"		##<< column name for UStar
  , NEEColName = "NEE"			##<< column name for NEE
  , TempColName = "Tair"		##<< column name for air temperature
  , WInch = 16 * 0.394		##<< width of the plot in inches, defaults to 16cm
  , HInchSingle = 6 * 0.394	##<< height of a subplot in inches, defaults to 6cm
  , ...						##<< other arguments to \code{.plotNEEVersusUStarTempClass},
  ## such as xlab and ylab axis label strings
  , data = cbind(sDATA, sTEMP, sUSTAR_DETAILS$bins[,c("uStarBin","tempBin")]) ##<<
  ## data.frame with variables to plot
) {
  ##author<< TW
  # generate subset of data
  dsSeason <- subset(data, season == season)
  tempBinLevels <- sort(unique(dsSeason$tempBin))
  # Open plot
  PlotType.s <- paste('NEEvsUStar', season, sep = "_")
  HInch <- HInchSingle * (length(tempBinLevels) + 1)
  PlotFile.s <- .self$.sxOpenPlot(
    'none', 'none', NA, PlotType.s, WInch, HInch, format, dir
    , 'sPlotNEEVersusUStarForSeason')
  on.exit(.self$.sxClosePlot(PlotFile.s))
  # Split screen
  split.screen(c(length(tempBinLevels) + 1, 1))
  split.screen(c(3, 1), screen = 1)
  # Set title of plot
  screen(length(tempBinLevels) + 3)
  mtext(.self$.sxSetTitle(
    'NEE', 'none', NA, paste('NEE versus uStar for season', season))
    , line = -3, side = 3, cex = 1.1)
  # Loop over all temperature classes
  # tempBinI <- 1L
  for (tempBinI in seq_along(tempBinLevels) ) {
    screen(1L + tempBinI)
    tempBinLevel <- tempBinLevels[tempBinI]
    uStarTh <- sUSTAR_DETAILS$tempInSeason[tempBinLevel, season]
    dss <- filter(dsSeason,  .data$tempBin == tempBinLevel)
    par(las = 1)                   #also y axis labels horizontal
    par(mar = c(2.0, 3.3, 0, 0) + 0.3)  #margins
    par(tck = 0.02)                          #axe-tick length inside plots
    par(mgp = c(1.1, 0.2, 0) )  #positioning of axis title, axis labels, axis
    par(cex = 10 / 12)			# default font size 10pt
    .plotNEEVersusUStarTempClass(
      dss, uStarTh, UstarColName = UstarColName, NEEColName = NEEColName
      , TempColName = TempColName, ...)
  }
}
sEddyProc$methods(
  sPlotNEEVersusUStarForSeason = sEddyProc_sPlotNEEVersusUStarForSeason)


.tmp.f <- function() {
  if (!exists("Example_DETha98")) load("data / Example_DETha98.RData")
  EddyData <- Example_DETha98
  EddyData <- cbind(
    EddyData, VPD = fCalcVPDfromRHandTair(EddyData$rH, EddyData$Tair))
  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix <- fConvertTimeToPosix(
    EddyData, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with the variables needed for processing later
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix, c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
  uStarTh <- EProc$sEstUstarThreshold()$uStarTh
  # plot saturation of NEE with UStar for one season -> in directory plots
  EProc$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[3] )
}

