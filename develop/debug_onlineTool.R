.loadInputFile <- function(){
  df <- fLoadTXTIntoDataframe("tmp/input.txt")
  str(df)
  REddyProc:::fCheckColNum(df, c("NEE","Tair","rH"))
}
