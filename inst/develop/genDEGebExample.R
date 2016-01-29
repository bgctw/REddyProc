dirName <- "m:/work_3/REddyProcRelease/Level3"
siteName <- "_DEGeb_"

dsAllYears <- fLoadEuroFlux16(siteName, dirName)
#ds$R_pot <- NULL
dsAllYears$qf_NEE_st <- NULL
DEGebExample <- dsAllYears[ (as.POSIXlt(dsAllYears$DateTime-15L*60L)$year+1900) %in% 2004:2006,]

plot( NEE ~ DateTime, DEGebExample )
save(DEGebExample,file="data/DEGebExample.RData")
