\name{fLoadEuroFlux16}
\alias{fLoadEuroFlux16}
\title{fLoadEuroFlux16}
\description{reads a sequence of annual files in the format of europe-fluxdata 2016}
\usage{fLoadEuroFlux16(siteName, dirName = "", additionalColumnNames = character(0))}
\arguments{
  \item{siteName}{scalar string: the name of the site, i.e. start of the filename before _<year>_}
  \item{dirName}{scalar string: the directory where the files reside}
  \item{additionalColumnNames}{character vector: column names to read in addition to c("Month","Day","Hour","NEE_st","qf_NEE_st","ustar","Ta",'Rg')}
}
\details{The filenames should corresppond to the pattern <sitename>_<YYYY>_.*.txt
And hold columns c("Month","Day","Hour","NEE_st","qf_NEE_st","ustar","Ta",'Rg').
By default only those colums are read and reported only c("DateTime","NEE","Ustar","Tair","Rg","qf_NEE_st" (Note the renaming).
NEE is set to NA for all values with "qf_NEE_st != 0.
Values  of -9999.0 are replaced by NA}


\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





