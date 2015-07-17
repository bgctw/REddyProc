\name{fitSeg2}
\alias{fitSeg2}
\title{fitSeg2}
\description{fit a segmented relationship according to eq 1a of Barr13}
\usage{fitSeg2(x, y, n = length(x))}\arguments{
  \item{x}{
}
  \item{y}{
}
  \item{n}{
}
}

\value{numeric vector with entries
\item{a0}{intercept of second part}
\item{a1}{first slope}
\item{a2}{second slope}
\item{cp}{estimated breakpoint}
\item{sdCp}{estimated standard error of cp}
\item{p}{probability of F test that segmented model is better than linear model (with slope and intercept)
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
n <- 11L
x <- seq(0L,1L,length.out=n)
noise <- rnorm(n, sd=0.1)
y1 <- y2 <- y3 <- rep(1,n) + noise
iSlope <- 1:(n/2L) 
y2[iSlope] <- 0.5 + x[iSlope] + noise[iSlope]  
y3[iSlope] <- 0.2 + (0.8/0.5)*x[iSlope] + noise[iSlope]
y <- y2
plot( y ~ x)
cf2 <- fitSeg2(x,y)
cf1 <- fitSeg1(x,y)
}
