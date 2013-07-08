     \name{Example_DETha98}
     \docType{data}
     \alias{Example_DETha98}
     \title{Observations of SOM and respiration at Howland.}
     \description{
       This data set gives the observations of soil organic matter (SOM), litter inputs, and respiration including their
       14C signature at the Howland study site.
     }
     \usage{Example_DETha98}
     \format{For each site (entries \code{obsNutirentSite} and \code{obsTowerSite}) a list with observations \describe{
			\item{respCum}{yearly cumulated carbon in respiration from Reservoir-C (non-recent C) in gC/m2}
			\item{respFM}{fraction modern C14 of respiration}
			\item{somStock}{C-stock in SOM gC/m2}
			\item{somOStock}{C-stock in O-horizon gC/m2}
			\item{somOFM}{fraction modern C14 of SOM in O-Horizon}
		}
		An additional entry \code{litter} reports the quantified litterfall common to the sites \describe{
			\item{leaf}{carbon in leaf litter fall gC/m2/yr}
			\item{root}{carbon in root litter input gC/m2/yr}
		}
		Each of the components is itself a list with components \describe{
			\item{times}{numeric vector of time in years}
			\item{obs}{numeric vector of observed quantity (unit see above)	}
			\item{sdObs}{numeric vector of standard deviation of the observations }
		}
     }
     \source{Thomas Wutzler processed data given by Sue Trumbore in 2010}
     \references{
     }
     \keyword{datasets}
