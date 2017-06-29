#---------------------------------------------------------------------------------------------#
# Description
#---------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------#

storeModis13Q1 <- function(iPath, oPath, delete=F) {
#---------------------------------------------------------------------------------------------#

  # check variables
  if (!exists('iPath')) {stop('error: input path missing')}
  if (!exists('oPath')) {stop('error: output path missing')}
  if (dir.exists(iPath)) {file.path(hdfPath)} else {stop('error: input data path not found')}
  if (dir.exists(oPath)) {file.path(hdfPath)} else {stop('error: output data path not found')}
  if (!delete%in%c(TRUE, FALSE)) {stop('error: "delete" is not a valid logical argument')}
  
#---------------------------------------------------------------------------------------------#

  # check for and load packages
  pkgTest <- function(x){
    if (!require(x,character.only = TRUE)){
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
    library(x)
  }
  pkgTest("raster")
  pkgTest("gdalUtils")
  
#---------------------------------------------------------------------------------------------#
  
  # list hdf files
  hdf.ls <- list.files(iPath, '.hdf', full.names=T)
  
  # check for existing collections and build file structure
  col <- as.character(sapply(hdf.ls, function(x) {strsplit(basename(x), '[.]')[[1]][4]}))
  uc <- unique(col) # unique collections
  for (u in 1:length(uc)) {
    path <- paste0(path, '/', uc[u], '/') # collection directory
    if (!dir.exists(path)) {dir.create(path)}
    path <- paste0(path, 'QA/') # quality data
    if (!dir.exists(path)) {dir.create(path)}
    path <- paste0(path, 'VI/') # vegetation indices
    if (!dir.exists(path)) {dir.create(path)}
  }
  
#---------------------------------------------------------------------------------------------#  
  
  # extract files
  for (h in 1:hdf.ls) {
    bname <- strsplit(basename(hdf.ls[h]), '.hdf')[[1]][1] # base file name
    oname <- paste0(oPath, '/', col[h], '/VI/', bname, '.ndvi.tif')  
    gdal_translate(hdf.ls[h], oname, sd_index=1) # ndvi
    oname <- paste0(oPath, '/', col[h], '/VI/', bname, '.evi.tif')  
    gdal_translate(hdf.ls[h], oname, sd_index=2) # evi
    oname <- paste0(oPath, '/', col[h], '/QA/', bname, '.doa.tif')  
    gdal_translate(hdf.ls[h], oname, sd_index=11) # day of acquisition
    oname <- paste0(oPath, '/', col[h], '/QA/', bname, '.prel.tif')  
    gdal_translate(hdf.ls[h], oname, sd_index=12) # pixel reliability
  }
  
  if(delete) {file.remove(hdf.ls[h])}
  
#---------------------------------------------------------------------------------------------#  
  
}