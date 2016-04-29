\name{sPlotNEEVersusUStarForSeason}
\alias{sPlotNEEVersusUStarForSeason}
\title{sEddyProc$sPlotNEEVersusUStarForSeason - Image with NEE versus UStar for each Temperature class of given season}
\description{Generates image in specified format ('pdf' or 'png')}
\usage{sPlotNEEVersusUStarForSeason(season.s = levels(sDATA$season)[1], 
    Format.s = "pdf", Dir.s = "plots", UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair")}
\arguments{
  \item{season.s}{season, i.e. time period, to plot}
  \item{Format.s}{Graphics file format ('pdf' or 'png')}
  \item{Dir.s}{Directory for plotting}
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
}



\author{TW  
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





