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

storeLandsat <- function(zpPath, ltPath, c1=T) {
  
  #---------------------------------------------------------------------------------------------------------------------#
  # 0. install/load required packages
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
  library(R.utils)
  library(raster)
  library(gdalUtils)
  
  #---------------------------------------------------------------------------------------------------------------------#
  
  # make metadata and sr directories
  if (!exists('zpPath')) {stop('error: "input path files missing')} else {zpPath <- file.path(zpPath)}
  if (!exists('ltPath')) {stop('error: "output path missing')} else {ltPath <- file.path(ltPath)}
  mPath <- paste0(ltPath, '/infos/metadata/stored/')
  if (!dir.exists(mPath)) {dir.create(mPath)}
  ltPath <- paste0(ltPath, '/sr/')
  if(!dir.exists(ltPath)) {dir.create(ltPath)}
  
  # control variables for bit conversion
  a<-2^(0:15)
  b<-2*a
  
#---------------------------------------------------------------------------------------------------------------------#
# 1. extract file info
#---------------------------------------------------------------------------------------------------------------------#
  
  # list zip files
  files <- list.files(zpPath, 'tar.gz', full.names=T)
  cc <- vector('numeric', length(files)) # clear pixels %
  
  # extract date information
  adate <- as.Date(paste0(substr(basename(files), 11, 14), '-', 
                          substr(basename(files), 15, 16), '-', 
                          substr(basename(files), 17, 18)))
  pdate <- sapply(files, function(x){strsplit(basename(x), '-')[[1]][2]})
  pdate <- as.Date(paste0(substr(basename(pdate), 3, 6), '-', 
                          substr(basename(pdate), 7, 8), '-', 
                          substr(basename(pdate), 9, 10)))
  
  # determine tiles
  tiles <- substr(strsplit(basename(files), '-')[[1]][1], 5, 10)
  ut <- unique(tiles)
  
#---------------------------------------------------------------------------------------------------------------------#
# 2. unzip and store files
#---------------------------------------------------------------------------------------------------------------------#
  
  for (t in 1:length(ut)) {
    
    # make/check target directory
    tPath <- paste0(ltPath, ut[t], '/')
    if(!dir.exists(tPath)) {dir.create(tPath)}
    ind <- which(tiles==ut[t])
    odr <- as.character(sapply(files[ind], function(x){paste0(tPath, strsplit(basename(x), '-')[[1]][1])}))
    
    for (f in 1:length(ind))
    
    # unzip file
    untar(files[f], exdir=odr[f], tar = "internal")
    
    # if dealing with collection 1 translate quality layer
    if (c1==T) {
      img <- list.files(aPath, 'pixel_qa.tif', full.names=T)
      rr0 <- raster(img)
      rr1 = getValues(rr0)
      rb = as.integer((rr1 %% b[3])>=a[3])
      rb = rb + (as.integer((rr1 %% b[4])>=a[4])*2)
      rb = rb + (as.integer((rr1 %% b[6])>=a[6])*4)
      rb = rb + ((as.integer((rr1 %% b[5])>=a[5])*3)*(rb==0))
      rb[is.na(rb)] = 255
      cc[f] <- sum(rb==0) / sum(rb!=255) * 100
      rr0 = raster::setValues(rr0, rb)
      fe = raster::extension(img)
      oname = paste0(strsplit(img, fe), '_mask', fe)
      writeRaster(rr0, filename=oname, datatype='INT1U', overwrite=TRUE)
      rm(img, rr0, rr1, rb, oname)
    } else {
      r <- raster(list.files(aPath, 'fmask.tif'))
      cc[f] <- cellStats(r==0) / cellStats(r!=255, sum) * 100
      rm(r)
    }
    
    # save metadata
    df <- data.frame(date=adate, path=odr, processed=pdate, clear=cc, stringsAsFactors=F)
    write.csv(df, paste0(mPath, '/', basename(tpath[t]), '.csv'))
    
  }

}