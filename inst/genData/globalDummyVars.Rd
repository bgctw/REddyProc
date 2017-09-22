\name{globalDummyVars}
\alias{sID}
\alias{sDATA}
\alias{sINFO}
\alias{sLOCATION}
\alias{sTEMP}
\alias{sUSTAR}
\title{globalDummyVars}

\description{
Dummy global variables with the same name as fields in R5 classes have been defined.

Reason: Class methods have been defined as plain functions, so that they can be better documented.
However, the assignment operator \code{<<-} has no meaning in it and therefore R CMD check complains.
As a workaround they have been defined as global variable. Do not use them.   
} 

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





