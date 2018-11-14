

##### Function to calculate metrics from LAI and NDVI #####
demonMETRICS <- function(rslist, ext, Nraster, saveResults){
  if ( ! ("rgdal" %in% installed.packages())) {install.packages("rgdal", dependencies = T)}
  if ( ! ("raster" %in% installed.packages())) {install.packages("raster", dependencies = T)}
  if ( ! ("rworldmap" %in% installed.packages())) {install.packages("rworldmap", dependencies = T)}
  
  require(raster)
  require(rworldmap)
  require(rgdal)
  
  tmpRasters <- stack(rslist)
  tmpRasters <- crop(tmpRasters, ext)
  
  Cummulative <- calc(tmpRasters, fun = sum)
  MeanRas <- calc(tmpRasters, fun = mean)
  MaxRas <- calc(tmpRasters, fun = max)
  MinRas <- calc(tmpRasters, fun = min)
  SDRas <- calc(tmpRasters, fun = sd)
  Seasonality <- (SDRas/MeanRas)
  
  if(saveResults == TRUE){
    writeRaster(Cummulative, "Cummulative_metric", format = "GTiff", overwrite = TRUE)
    writeRaster(MeanRas, "Mean_metric", format = "GTiff", overwrite = TRUE)
    writeRaster(MaxRas, "Maximum_metric", format = "GTiff", overwrite = TRUE)
    writeRaster(MinRas, "Minimum_metric", format = "GTiff", overwrite = TRUE)
    writeRaster(Seasonality, "Seasonality_metric", format = "GTiff", overwrite = TRUE)
  }
}


##### Function to extract information from LAI or other raster stack #####
demonExtract <- function(rasters, specie, Nraster, saveResults){
  tmp <- list()
  tmp2 <- list()
  for(i in 1:Nraster){
    svMisc::progress(i, max.value = Nraster)
    tmp[[i]] <- extract(rasters[[i]], specie, na.rm = TRUE)
    tmp2[[i]] <- na.omit(unlist(tmp[[i]]))
  }
  Dat <- do.call(cbind, tmp2)
  days <- c(1, 9, 17, 25, 33, 41, 49, 57, 65, 73, 81, 89, 97, 105, 113, 121, 129, 137, 145, 153, 161, 169, 
            177, 185, 193, 201, 209, 217, 225, 233, 241, 249, 257, 265, 273, 281, 289, 297, 305, 313, 321, 
            329, 337, 345, 353, 361)
  cums <- apply(Dat, 2, sum)
  means <- apply(Dat, 2, mean)
  mins <- apply(Dat, 2, min)
  maxs <- apply(Dat, 2, max)
  sds <- apply(Dat, 2, sd)
  seasonality <- sds/means
  var2 <- apply(Dat, 2, var)
  Tabla <- data.frame(days, cums, means, mins, maxs, sds, seasonality, var2)
  if(saveResults == TRUE){
    write.csv(Tabla, "Tabla_results.csv")
  }
  return(Tabla)
}

