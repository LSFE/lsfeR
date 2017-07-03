#-------------------------------------------------------------------------------------------------------------------------#
# Description
#-------------------------------------------------------------------------------------------------------------------------#
# Stores MODIS 13Q1 data into a standardized data structure.
# Retrieves bands for:
# - NDVI (ndvi)
# - EVI (evi)
# - Day of Acquisition (doa)
# - Pixel Reliability (prel)

# Variables
# ipath: Path to Hdf files
# opath: parent path where files will be stored
# vi: Which vegetation indices should be exported? Set "ndvi", "evi" a vector of both.
# delete: Should the hdf file be deleted after usage? Default is FALSE

# NOTE: The images are exported as "tiff".

#-------------------------------------------------------------------------------------------------------------------------#

storeS2 <- function(ipath, opath, delete=F) {
  
#-------------------------------------------------------------------------------------------------------------------------#
  
  # check variables
  if (!exists('ipath')) {return('error: input path missing')} else {ipath <- file.path(ipath)}
  if (!exists('opath')) {return('error: output path missing')} else {opath <- file.path(opath)}
  if (dir.exists(ipath)) {ipath <- file.path(ipath)} else {return('error: input data path not found')}
  if (dir.exists(opath)) {opath <- file.path(opath)} else {return('error: output data path not found')}
  if (!is.logical(delete)) {return('error: "delete" is not a valid logical argument')}
  
#-------------------------------------------------------------------------------------------------------------------------#
  
  # check for and load packages
  pkgTest <- function(x){
    if (!require(x,character.only = TRUE)){
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  pkgTest("gdalUtils")
  pkgTest("rgdal")
  
#-------------------------------------------------------------------------------------------------------------------------#
  
  # list hdf files
  zip.ls <- list.files(ipath, '.zip', full.names=T)
  
  # extract file info
  adate <- (sapply(zip.ls, function(x) {substr(strsplit(basename(x), '_')[[1]][3], 1, 8)})) # aq. date (1)
  adate <- as.Date(paste0(substr(adate, 1, 4), '-', substr(adate, 5, 6), '-', substr(adate, 7, 8))) # aq. date (2)
  pdate <- (sapply(zip.ls, function(x) {substr(strsplit(basename(x), '_')[[1]][7], 1, 8)})) # processing date (1)
  pdate <- as.Date(paste0(substr(pdate, 1, 4), '-', substr(pdate, 5, 6), '-', substr(pdate, 7, 8))) # processing date (2)
  
  # check for existing collections/tiles
  col <- as.character(sapply(zip.ls, function(x) {strsplit(basename(x), '_')[[1]][2]}))
  tiles <- sapply(zip.ls, function(x) {strsplit(basename(x), '_')[[1]][6]})
  tiles <- as.character(sapply(tiles, function(x) {substr(x, 2, nchar(x))}))
  
#-------------------------------------------------------------------------------------------------------------------------#
  
  # create folders for infos/metadata
  mpath <- paste0(opath, '/infos/')
  if(!dir.exists(mpath)) {dir.create(mpath)}
  mpath <- paste0(mpath, 'metadata/')
  if(!dir.exists(mpath)) {dir.create(mpath)}
  
#-------------------------------------------------------------------------------------------------------------------------#
  
  # write files and metadata
  uc <- unique(col)
  for (c in 1:length(uc)) {
    
    # create collection path
    path <- paste0(opath, '/', uc[c], '/') # collection directory
    if (!dir.exists(path)) {dir.create(path)}
    ut <- unique(tiles[which(col==uc[c])]) # unique tiles
    
    # loop through each tile
    for (t in 1:length(ut)) {
      
      # create tile path
      path <- paste0(opath, '/', uc[c], '/', ut[t])
      if (!dir.exists(path)) {dir.create(path)} # create tile path
      ind <- which(col==uc[c] & tiles==ut[t]) # find target tiles
      idr <- sapply(zip.ls[ind], function(x) {paste0(path, '/', strsplit(basename(x), '.zip')[[1]][1], '.SAFE')}) # file path
      
      # initiate metadata file
      df <- data.frame(date=adate[ind], path=idr, processed=pdate, clear=vector('numeric', length(idr)), stringsAsFactors=F)
      
      # deal with each zip
      for (z in 1:length(ind)) {

        # unzip data
        unzip(zip.ls, exdir=path)
        
        # read cloud cover
        file0 <- list.files(idr[z], 'CLD_60m.jp2', recursive=T, full.names=T)
        file1 <- paste0(strsplit(file, '.jp2')][[1]][1], '.tif')
        gdal_translate(file0, file1) # convert from jpeg2000 to geotiff
        r1 <- raster(file1)
        file.remove(file1)
        file2 <- list.files(idr[z], 'AOT_60m.jp2', recursive=T, full.names=T)
        
        # read aot (defines background)
        file0 <- list.files(idr[z], 'AOT_60m.jp2', recursive=T, full.names=T)
        file1 <- paste0(strsplit(file, '.jp2')][[1]][1], '.tif')
        gdal_translate(file0, file1) # convert from jpeg2000 to geotiff
        r2 <- raster(file1)
        file.remove(file1)
        
        # estimate percent of clear pixels
        df$clear[h] <- cellStats((r1==0 & r2 > 0), sum) / cellStats(r2!=255, sum) * 100
        rm(file0, file1, r1, file2, r2)
        
        if(delete) {file.remove(zip.ls[ind[h]])} # delete file if requested
        
      }
      
      # write metadata
      ofile <- paste0(mpath, ut[t], '_metadata.csv')
      if (file.exists(ofile)) {
        df0 <- read.csv(ofile) # see existing metadata
        dup <- df$date %in% as.Date(df0$date) # check for duplicates.
        if (max(dup)>0) {df <- rbind(df0[which(dup!=TRUE),], df)} # replace duplicates.
        rm(df0)}
      write.csv(df, ofile, row.names=F)
      rm(df)
      
    }
  }
  
  #---------------------------------------------------------------------------------------------------------#  
  
}