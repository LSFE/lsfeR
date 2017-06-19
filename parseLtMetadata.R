#---------------------------------------------------------------------------------------#
# description
#---------------------------------------------------------------------------------------#
# Parses Landsat collection 1 data and extracts relevant information.
# Output is used as reference to select relevant acquisition.
# It provides information on
# - directory path
# - cloud cover (%) based on the fmask or mask from quality assessment (see "ltBitGet.R")
# - solar zenith
# - solar azimuth
# - acquisition year
# - acquisition day
# - upper left coordinates (UTM)
# - lower right coordinates (UTM)
# - sensor

#---------------------------------------------------------------------------------------#

bpath <- 'D:/01_DATA/LANDSAT/SR/' # base path
mpath <- 'D:/01_DATA/LANDSAT/infos/metadata/'# metadata path

#---------------------------------------------------------------------------------------#

# check/construct file path
bpath <- file.path(bpath)
mpath <- file.path(mpath)

#check if package is isntalled
pkgTest <- function(x){
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("raster")
pkgTest("XML")

# load packages
library(raster)
library(XML)

# list tile paths
tpath <- list.dirs(bpath, full.names=T, recursive=F)

# loop through each tile
for (t in 1:length(tpath)) {

#---------------------------------------------------------------------------------------#
  # initiate variables
#---------------------------------------------------------------------------------------#
  
  dirs <- list.dirs(tpath, full.names=T, recursive=F) # list acquisitions
  nr <- length(dirs) # number of directories
  cc <- vector('numeric', nr) # cloud cover
  sa <- vector('numeric', nr) # sun azimuth
  sz <- vector('numeric', nr) # sun zenith
  ad <- vector('numeric', nr) # acquisition day
  ay <- vector('numeric', nr) # aquisition year
  ul <- matrix(0, 2, nr) # upper left coordinates
  lr <- matrix(0, 2, nr) # lower right coordinates
  ss <- vector('character', nr) # sensor
  
#---------------------------------------------------------------------------------------#
  # loop through each directory
#---------------------------------------------------------------------------------------#
  
  for (d in 1:length(dirs)) {
    
    # open/parse metadata
    xmlFile <- xmlToList(xmlParse(file))
    sz[d] <- as.numeric(xmlFile[[1]]$solar_angles[[1]])
    sa[d] <- as.numeric(xmlFile[[1]]$solar_angles[[2]])
    tmp <- as.Date(xmlFile[[1]]$acquisition_date)
    ay[d] <- as.character(format(tmp,'%Y'))
    ad[d] <- as.numeric(tmp-as.Date(paste0(y, '-01-01'))+1)
    ul[d,] <- as.numeric(xmlFile[[1]]$projection_information[[1]][2:3])
    lr[d,] <- as.numeric(xmlFile[[1]]$projection_information[[2]][2:3])
    ss[d] <- xmlFile[[1]]$satellite
    
  }
  
#---------------------------------------------------------------------------------------#  
#   save metadata
#---------------------------------------------------------------------------------------#  
  
  df <- data.frame(directory=dirs, zenith=sz, azimuth=az, year=ay, 
             doy=ad, ulx=ul[,1], uly=ul[,2], lrx=lr[,1], lry=lr[,2])
  write.csv(df, paste0(mpath, '/', basename(tpath[t]), '.csv'))
  
}

#---------------------------------------------------------------------------------------#

# write metadata