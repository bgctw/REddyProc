.loadInputFile <- function(){
  df <- fLoadTXTIntoDataframe("tmp/input.txt")
  str(df)
  REddyProc:::fCheckColNum(df, c("NEE","Tair","rH"))
}

.loadDump <- function(){
  #setwd("/User/homes/twutz/REddyProcWeb/work/001")
  setwd("/User/homes/MDIwork/REddyProcWeb/work/001")
  dumpFileBasename <- "RBatch_dump"
  load(paste(dumpFileBasename,".rda",sep = ""))
  debugger(get(dumpFileBasename))
}
