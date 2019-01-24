#compares two vectors and gives you elements that are present only in one of the vectors 
#a, b: vectores to be compared

which.isNOTin <- function(a,b){
ab <- c(a, b)
ba <- c(b, a)
la <- length(a)
lb <- (length(b))
notINa <- b[!duplicated(ab)[(la + 1):(la + lb)]]
notINb <- a[!duplicated(ba)[(lb + 1):(la + lb)]]
return(list("not in first list" = notINa, "not in second list" = notINb))
}

#x <- read.csv("PresAbs_biomes_mammals.csv")
### xxxx
#which.isNOTin(a = x$mammals_all, b = x$mammals_biome1)
