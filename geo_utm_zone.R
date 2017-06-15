# determines an appropriate UTM zone from a set of coordinates.
# e.g. coordinates can correspond to the vertices of a shapefile or a set of point samples.
# the code estimates the UTM zone correspondent to each coordinate pair and then 

geo_utm_zone <- function(sc) {
  
  # find target UTM zone
  zc <- vector('character', length(sc))
  o <- vector('character', length(sc))
  for (c in 1:length(sc)) {
    if (sc[c,1] > 0) {o[c]<-'N'} else {o[c]<-'S'}
    if (sc[c,2] > 0) {cz[c]<-round(sc[c,2]/6.)+31} else {cz[c]<-round((180+sc[c,2])/6)+1}
  }
  zc <- sapply(paste0(cz, o), function(x) length(unique(x))) # zone code counts
  ind <- which(zc==max(zc))
  return(list(zone=cz[ind], orientation=o[ind])) # return dominant zone/orientation
}

