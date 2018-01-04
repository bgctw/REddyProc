\name{sEddyProc_sPlotNEEVersusUStarForSeason}
\alias{sEddyProc_sPlotNEEVersusUStarForSeason}
\title{sEddyProc$sPlotNEEVersusUStarForSeason - Image with NEE versus UStar for each Temperature class of given season}
\description{Generates image in specified format ('pdf' or 'png')}
\usage{sEddyProc_sPlotNEEVersusUStarForSeason(season = levels(sDATA$season)[1], 
    format = "pdf", dir = "plots", UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair", 
    WInch = 16 * 0.394, HInchSingle = 6 * 
        0.394, ...)}
\arguments{
  \item{season}{string of season, i.e. time period, to plot}
  \item{format}{string of Graphics file format ('pdf' or 'png')}
  \item{dir}{string of Directory for plotting}
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{WInch}{width of the plot in inches, defaults to 16cm}
  \item{HInchSingle}{height of a subplot in inches, defaults to 6cm}
  \item{\dots}{other arguments to \code{.plotNEEVersusUStarTempClass}, such as xlab and ylab axis label strings}
}



\author{TW
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





