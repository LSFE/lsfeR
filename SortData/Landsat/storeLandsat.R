#---------------------------------------------------------------------------------------------------------------------#
# description
#---------------------------------------------------------------------------------------------------------------------#
# Unzips and stores landsat data by tile and by acquisition. QA bands are translated into a file names "*mask"
# Additionally, it parses and stores relevant metadata from each acquisition on a tile-by-tile basis.
# One should provide:
# zpPath: Path where the zip files are stored
# ltPath: Path where the acquisitions and metadata will be stored (subfolders are automatically created)
# c1: Logical (Default is TRUE). If TRUE it translated the QA band from collection 1 into a fmask-like file coded as:
#   0: Usable data
#   1: Water
#   2: Cloud shadow
#   3: Snow
#   4: Cloud
#   255: No Data

# the function is called as:
# storeLandsat(zpPath, ltPath) # no need to define c1 if c1=TRUE



# author: Ruben Remelgado (ruben.remelgado@uni-wuerzburg.de)
#---------------------------------------------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------------------------------------------#

storeLandsat <- function(zpPath, ltPath, c1=T) {
  
  # check directories
  if(!dir.exists(zpPath)) {stop('error: path containing zip files not found')} else {file.path(zpPath)}
  if(!dir.exists(ltPath)) {stop('error: path where acquisition will be stored not found')} else {file.path(ltPath)}
  
  # make metadata and sr directories
  mPath <- paste0(ltPath, '/infos/metadata/')
  dir.create(mPath)
  ltPath <- paste0(ltPath, '/sr/')
  dir.create(ltPath)
  
  # control variables for bit conversion
  a<-2^(0:15)
  b<-2*a
  
#---------------------------------------------------------------------------------------------------------------------#
  # 1. unzip and store files
#---------------------------------------------------------------------------------------------------------------------#
  
  # list zip files
  files <- list.files(zpPath, 'tar.gz')
  
  for (f in 1:length(files)) {
    
    # make/check target directory
    tile <- substr(files[f], 5, 10)
    tPath <- paste0(ltPath, '/', tile, '/')
    if(!dir.exists(tPath)) {dir.create(tPath)}
    
    # unzip file
    aPath <- paste0(tPath, strsplit(files[f], '[.]')[[1]][1])
    unzip(file, exdir=aPath)
    
    # if dealing with collection 1 translate quality layer
    if (c1==T) {
      img <- list.files(aPath, 'pixel_qa.', full.names=T)
      rr0 <- raster(img)
      rr1 = getValues(rr0)
      rb = as.integer((rr1 %% b[3])>=a[3])
      rb = rb + (as.integer((rr1 %% b[4])>=a[4])*2)
      rb = rb + (as.integer((rr1 %% b[6])>=a[6])*4)
      rb = rb + ((as.integer((rr1 %% b[5])>=a[5])*3)*(rb==0))
      rb[is.na(rb)] = 255
      rr0 = raster::setValues(rr0, rb)
      fe = raster::extension(img.ls[i])
      oname = paste0(strsplit(img, fe, '_mask', fe))
      writeRaster(rr0, filename=oname, datatype='INT1U', overwrite=TRUE)
      rm(img, rr0, rr1, rb, oname)
    }
    
  }

#---------------------------------------------------------------------------------------------------------------------#
  # 3. parse metadata
#---------------------------------------------------------------------------------------------------------------------#

  # list tile paths
  tpath <- list.dirs(ltPath, full.names=T, recursive=F)
  
  # loop through each tile
  for (t in 1:length(tpath)) {
    
    # initiate variables
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
    
    # loop through each directory
    for (d in 1:length(dirs)) {
      
      # read needed data
      xmlFile <- xmlToList(xmlParse(list.files(dirs[d], '.xml', full.names=T))) # metadata
      r <- getValues(raster(list.files(dirs[d], 'mask.tif', full.names=T))) # mask
      
      # retrieve 
      cc[d] <- sum(r!=0, na.rm=T) / sum(is.finite(r))
      sz[d] <- as.numeric(xmlFile[[1]]$solar_angles[[1]])
      sa[d] <- as.numeric(xmlFile[[1]]$solar_angles[[2]])
      tmp <- as.Date(xmlFile[[1]]$acquisition_date)
      ay[d] <- as.character(format(tmp,'%Y'))
      ad[d] <- as.numeric(tmp-as.Date(paste0(y, '-01-01'))+1)
      ul[d,] <- as.numeric(xmlFile[[1]]$projection_information[[1]][2:3])
      lr[d,] <- as.numeric(xmlFile[[1]]$projection_information[[2]][2:3])
      ss[d] <- xmlFile[[1]]$satellite
      
      # remote temporary data
      rm(xmlFile, r, tmp)
      
    }
    
    # save metadata
    df <- data.frame(directory=dirs, zenith=sz, azimuth=az, year=ay, 
                     doy=ad, ulx=ul[,1], uly=ul[,2], lrx=lr[,1], lry=lr[,2])
    write.csv(df, paste0(mPath, '/', basename(tpath[t]), '.csv'))
    
  }
  
}