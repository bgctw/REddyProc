.profileGapFill <- function(){
  require(profr)
  EddyDataWithPosix2.F <- cbind(
    EddyDataWithPosix.F
    , QF = c(1, 0, 1, 0, 1, 0, 0, 0, 0, 0))
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2.F[1:(48*3*30),]
    , c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  p1 <- profr({
    #for( i in 1:1 ){
    EddyProc.C$sMDSGapFill('NEE', Verbose.b = F, FillAll.b = TRUE)
    #}
  }, 0.01 )
  plot(p1)
  plot(subset(p1, start > 1 & start < 2))
  plot(subset(p1, start > 1.6 & start < 1.8))
}

.profilePartGL <- function(){
  require(profr)
  p1 <- profr({
    for (i in 1:1) {
      tmp <- partitionNEEGL( dsNEE1 )
    }
  }, 0.01 )
  plot(p1)
  range(dsNEE$sDateTime)
}

