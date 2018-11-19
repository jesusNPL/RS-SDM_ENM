
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


pamFromRaster2 <- function(species, resol, xmn, xmx, ymn, ymx){
  require(raster)
  r <- raster(xmn, xmx, ymn, ymx)
  res(r) <- resol
  values(r) <- 0
  valores <- values(r)
  xy <- xyFromCell(r, 1:length(valores))
  tmp <- list()
  for(i in 1:lenght(species)){
    print(i)
  tmp[[i]] <- extract(species, xy)
    }
  tmp2 <- do.call(rbind, tmp)
  pam <- cbind(xy, sample2matrix(tmp2))
  return(pam)
}
