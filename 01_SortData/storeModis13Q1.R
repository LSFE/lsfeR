#---------------------------------------------------------------------------------------------------------#
# Description
#---------------------------------------------------------------------------------------------------------#
# Stores MODIS 13Q1 data into a standardized data structure.
# Retrieves bands for:
# - NDVI
# - EVI
# - Day of Acquisition
# - Pixel Reliability

# Variables
# iPath: Path to Hdf files
# oPath: parent path where files will be stored
# delete: Should the hdf file be deleted after usage? Default is FALSE

#---------------------------------------------------------------------------------------------------------#

storeModis13Q1 <- function(iPath, oPath, vi=c('ndvi', 'evi'), delete=F) {
#---------------------------------------------------------------------------------------------------------#

  # check variables
  if (!exists('iPath')) {stop('error: input path missing')} else {iPath <- file.path(iPath)}
  if (!exists('oPath')) {stop('error: output path missing')} else {oPath <- file.path(oPath)}
  if (dir.exists(iPath)) {file.path(hdfPath)} else {stop('error: input data path not found')}
  if (dir.exists(oPath)) {file.path(hdfPath)} else {stop('error: output data path not found')}
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
  hdf.ls <- list.files(iPath, '.hdf', full.names=T)
  
  # check for existing collections and build file structure
  col <- as.character(sapply(hdf.ls, function(x) {strsplit(basename(x), '[.]')[[1]][4]}))
  tiles <- as.character(sapply(hdf.ls, function(x) {strsplit(basename(x), '[.]')[[1]][3]}))
  prod <- as.character(sapply(hdf.ls, function(x) {strsplit(basename(x), '[.]')[[1]][1]}))
  uc <- unique(col) # unique collections
  for (c in 1:length(uc)) {
    path <- paste0(oPath, '/', uc[c], '/') # collection directory
    if (!dir.exists(path)) {dir.create(path)}
    up <- unique(prod[which(col==uc[c])]) # unique products
    for (p in 1:length(up)) {
      ut <- unique(tiles[which(col==uc[c] & prod==up[p])]) # unique tiles
      path <- paste0(oPath, '/', uc[c], '/', up[p], '/')
      if (!dir.exists(path)) {dir.create(path)} # product path
      for (t in 1:length(ut)) {
        tpath <- paste0(oPath, '/', uc[c], '/', up[p], '/', ut[t], '/')
        if (!dir.exists(tpath)) {dir.create(tpath)} # create tile path
        path <- paste0(tpath, '/QA/') # quality data
        if (!dir.exists(path)) {dir.create(path)}
        path <- paste0(tpath, '/VI/') # vegetation indices
        if (!dir.exists(path)) {dir.create(path)}
      }
    }
  }
    
#---------------------------------------------------------------------------------------------------------#  
  
  # extract files
  for (h in 1:length(hdf.ls)) {
    bname <- strsplit(basename(hdf.ls[h]), '.hdf')[[1]][1] # base file name
    if ('ndvi'%in%vi) {
      oname <- paste0(oPath, '/', col[h], '/', prod[h], '/', tiles[h], '/VI/', bname, '.ndvi.tif')  
      gdalUtils::gdal_translate(hdf.ls[h], oname, sd_index=1, verbose=F) # ndvi
    }
    if ('evi'%in%vi) {
      oname <- paste0(oPath, '/', col[h], '/', prod[h], '/', tiles[h], '/VI/', bname, '.evi.tif')  
      gdalUtils::gdal_translate(hdf.ls[h], oname, sd_index=2, verbose=F) # evi
    }
    oname <- paste0(oPath, '/', col[h], '/', prod[h], '/', tiles[h], '/QA/', bname, '.doa.tif')  
    gdalUtils::gdal_translate(hdf.ls[h], oname, sd_index=11, verbose=F) # day of acquisition
    oname <- paste0(oPath, '/', col[h], '/', prod[h], '/', tiles[h], '/QA/', bname, '.prel.tif')  
    gdalUtils::gdal_translate(hdf.ls[h], oname, sd_index=12, verbose=F) # pixel reliability
    if(delete) {file.remove(hdf.ls[h])}
  }
  
#---------------------------------------------------------------------------------------------------------#  
  
}