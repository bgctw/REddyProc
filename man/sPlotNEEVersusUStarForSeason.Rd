\name{sPlotNEEVersusUStarForSeason}
\alias{sPlotNEEVersusUStarForSeason}
\title{sEddyProc$sPlotNEEVersusUStarForSeason - Image with NEE versus UStar for each Temperature class of given season}
\description{Generates image in specified format ('pdf' or 'png')}
\usage{sPlotNEEVersusUStarForSeason(season.s = levels(sDATA$season)[1], 
    Format.s = "pdf", Dir.s = "plots", UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair", 
    WInch = 16 * 0.394, HInchSingle = 6 * 
        0.394, ...)}
\arguments{
  \item{season.s}{season, i.e. time period, to plot}
  \item{Format.s}{Graphics file format ('pdf' or 'png')}
  \item{Dir.s}{Directory for plotting}
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{WInch}{width of the plot in inches, defaults to 16cm}
  \item{HInchSingle}{height of a subplot in inches, defaults to 6cm}
  \item{\dots}{other arguments to \code{.plotNEEVersusUStarTempClass}, such as xlab and ylab axis label strings}
}



\author{TW  
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





