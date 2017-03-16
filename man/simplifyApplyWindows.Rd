\name{simplifyApplyWindows}
\alias{simplifyApplyWindows}
\title{simplifyApplyWindows}
\description{simplify the result returned by applyWindows where FUN returned a named vector, or a single-row data.frame}
\usage{simplifyApplyWindows(resApply)}
\arguments{
  \item{resApply}{result of \code{\link{applyWindows}}}
}
\details{If FUN returns a named vector or a single-row data.frame, the resFUN result component of applyWindows can be condensed to a data.frame.
This result is column-bound to the winInfo result component}
\value{A single data.frame with columns of winInfo and results of FUN}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





