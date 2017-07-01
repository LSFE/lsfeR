#---------------------------------------------------------------------------------------------------------#
# Description
#---------------------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------------------#

storeModis13Q1 <- function(ipath, opath, vi=c('ndvi', 'evi'), delete=F) {
#---------------------------------------------------------------------------------------------------------#

  # check variables
  if (!exists('ipath')) {stop('error: input path missing')} else {ipath <- file.path(ipath)}
  if (!exists('opath')) {stop('error: output path missing')} else {opath <- file.path(opath)}
  if (dir.exists(ipath)) {ipath <- file.path(ipath)} else {stop('error: input data path not found')}
  if (dir.exists(opath)) {opath <- file.path(opath)} else {stop('error: output data path not found')}
  if (min(vi%in%c('ndvi', 'evi'))==0) {stop('error: "vi" argument not valid (use "ndvi", "evi" or both)')}
  if (!is.logical(delete)) {stop('error: "delete" is not a valid logical argument')}
  
#---------------------------------------------------------------------------------------------------------#

  # check for and load packages
  pkgTest <- function(x){
    if (!require(x,character.only = TRUE)){
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  pkgTest("gdalUtils")
  
#---------------------------------------------------------------------------------------------------------#
  
  # list hdf files
  hdf.ls <- list.files(ipath, '.hdf', full.names=T)
  
  # extract file info
  adate <- (sapply(hdf.ls, function(x) {substr(strsplit(basename(x), '[.]')[[1]][2], 2, 9)})) # aq. date (1)
  adate <- as.Date(paste0(substr(adate, 1, 4), '-01-01')) + (as.numeric(substr(adate, 5, 8))-1) # aq. date (2)
  pdate <- (sapply(hdf.ls, function(x) {substr(strsplit(basename(x), '[.]')[[1]][5], 1, 7)})) # processing date (1)
  pdate <- as.Date(paste0(substr(pdate, 1, 4), '-01-01')) + (as.numeric(substr(pdate, 5, 8))-1) # processing date (2)
  
  # check for existing collections/tiles
  col <- as.character(sapply(hdf.ls, function(x) {strsplit(basename(x), '[.]')[[1]][4]}))
  tiles <- as.character(sapply(hdf.ls, function(x) {strsplit(basename(x), '[.]')[[1]][3]}))

#---------------------------------------------------------------------------------------------------------#

  # create folders for infos/metadata
  mpath <- paste0(opath, '/infos/')
  if(!dir.exists(mpath)) {dir.create(mpath)}
  mpath <- paste0(mpath, 'metadata/')
  if(!dir.exists(mpath)) {dir.create(mpath)}
  
#---------------------------------------------------------------------------------------------------------#
  
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
      path <- paste0(opath, '/', uc[c], '/', ut[t], '/')
      if (!dir.exists(path)) {dir.create(path)} # create tile path
      ind <- which(col==uc[c] & tiles==ut[t]) # find target tiles
      idr <- sapply(hdf.ls[ind], function(x) {paste0(strsplit(basename(x), '[.]')[[1]][1:4], collapse='.')}) # file path (1)
      idr <- paste0(opath, '/', uc[c], '/', ut[t], '/', idr, '/') # file path (2) 
      
      # write metadata
      df <- data.frame(date=adate[ind], path=idr, processed=pdate, stringsAsFactors=F)
      ofile <- paste0(mpath, ut[t], '_metadata.csv')
      if (file.exists(ofile)) {
        df0 <- read.csv(ofile) # see existing metadata
        dup <- df$date %in% as.Date(df0$date) # check for duplicates.
        if (max(dup)>0) {df <- rbind(df0[which(dup!=TRUE),], df)} # replace duplicates.
        rm(df0)}
      write.csv(df, ofile, row.names=F)
      rm(df)
      
      # write files
      for (h in 1:length(ind)) {
        bname <- paste0(idr[h], basename(idr[h])) # base file name
        if (!dir.exists(bname)) {dir.create(idr[h])}
        if ('ndvi'%in%vi) {gdalUtils::gdal_translate(hdf.ls[ind[h]], paste0(bname, '.ndvi.tif'), sd_index=1)}
        if ('evi'%in%vi) {gdalUtils::gdal_translate(hdf.ls[ind[h]], paste0(bname, '.evi.tif'), sd_index=1)}
        gdalUtils::gdal_translate(hdf.ls[ind[h]], paste0(bname, '.doa.tif'), sd_index=11)
        gdalUtils::gdal_translate(hdf.ls[h], paste0(bname, '.prel.tif'), sd_index=12)
        if(delete) {file.remove(hdf.ls[ind[h]])} # delete file if requested
      }
      
    }
  }
    
#---------------------------------------------------------------------------------------------------------#  

}