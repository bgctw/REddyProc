\name{fitSeg1}
\alias{fitSeg1}
\title{fitSeg1}
\description{fit a segmented relationship according to eq 1b of Barr13}
\usage{fitSeg1(x, y, n = length(x))}
\arguments{
  \item{x}{
}
  \item{y}{
}
  \item{n}{
}
}

\value{numeric vector with entries
\item{b0}{intercept of second part}
\item{b1}{first slope (second slope is fixed to zero)}
\item{cp}{estimated breakpoint}
\item{sdCp}{estimated standard error of cp}
\item{p}{probability of F test that segmented model is better than a linear model
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





