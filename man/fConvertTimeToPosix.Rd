\name{fConvertTimeToPosix}
\alias{fConvertTimeToPosix}
\title{Convert different time formats to POSIX}
\description{The different time formats are converted to POSIX (GMT) and a 'TimeDate' column is prefixed to the data frame}
\usage{fConvertTimeToPosix(Data.F, TFormat.s, Year.s = "none", Month.s = "none", 
    Day.s = "none", Hour.s = "none", Min.s = "none", TName.s = "DateTime")}
\arguments{
  \item{Data.F}{Data frame with time columns to be converted}
  \item{TFormat.s}{Abbreviation for implemented time formats}
  \item{Year.s}{Column name of year}
  \item{Month.s}{Column name of month}
  \item{Day.s}{Column name of day}
  \item{Hour.s}{Column name of hour}
  \item{Min.s}{Column name of min}
  \item{TName.s}{Column name of new column}
}
\details{Implemented time formats:}
\value{Data frame with prefixed POSIX time column.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
# See unit test in test_fConvertTimeToPosix for example
}
