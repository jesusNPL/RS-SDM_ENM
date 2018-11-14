
climTurnover <- function(rt0, rt1){
  if ( ! ("raster" %in% installed.packages())) {install.packages("raster", dependencies = T)}
  require(raster)
  tmp <- (rt1-rt0)
  rgain <- calc(tmp, fun = function(x){ x[x <= 0] <- NA; return(x)} )
  rlost <- calc(tmp, fun = function(x){ x[x >= 0] <- NA; return(x)} )
  rturnWD <- (tmp/(rt0 + rgain)) # Assuming dispersal
  rturnND <- (abs(rlost)/rt0) # Assuming no-dispersal
  
  results <- stack(tmp, rgain, rlost, rturnWD, rturnND)
  names(results) <- c("G&L", "Gain", "Lost", "TurnoverWD", "TurnoverND")
  return(results)
}
