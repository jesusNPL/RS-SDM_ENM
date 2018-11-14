
HDFtoTIFF <- function(dire, HDFs, TIFFs, lyr, fromSRS, toSRS){
  if ( ! ("gdalUtils" %in% installed.packages())) {install.packages("gdalUtils", dependencies = T)}
  require(gdalUtils)
  
  setwd(dire)
  suppressWarnings(dir.create(paste(dire, "Projected", sep = "/")))
  for(i in 1:length(HDFs)){
    gdal_translate(HDFs[i], TIFFs[i], sd_index = lyr)
    gdalwarp(TIFFs[i], paste(dire, "Projected", TIFFs[i], sep = "/"), overwrite = T, 
             s_srs = fromSRS, t_srs = toSRS, srcnodata = -3000, dstnodata = -3000)
    unlink(TIFFs[i])
  }
}

