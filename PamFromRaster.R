
pamFromRaster <- function(models, resol, xmn, xmx, ymn, ymx){
  require(raster)
  r <- raster(xmn, xmx, ymn, ymx)
  res(r) <- resol
  values(r) <- 0
  valores <- values(r)
  xy <- xyFromCell(r, 1:length(valores))
  tmp <- extract(models, xy)
  pam <- cbind(xy, tmp)
  return(pam)
}
