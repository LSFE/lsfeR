#-----------------------------------------------------------------------------------------------#
# Description
#-----------------------------------------------------------------------------------------------#
# Determines required tiles and returns list of acquisition data paths.

# variables:
# path: parent directory of the target sensor.
# col: "String" elements specifying the data collection.
# ct: Numeric. Maximum cloud cover percentage (0-100).
# dates: "Date" object with the target dates.
# tiles: target tiles.
# ext: "Extent" object.
# rp: Projection for "ext".

# NOTES:
# - If the tiles are unknown, an extent ("ext") can is used to determine it.
#   If "tiles" is missing the code requires "ext" and "rp". Then, the function
#   uses this information to query the "tiles.shp" file within the "infos" folder.
#   "rp" is used to compare the projection of the reference extent object. If it is
#   not the same, the function reprojects it to match with "tiles.shp". If "tiles" 
#   is provided the remaining arguments are ignored.
# - If "dates" is provided, the function will search for images that match any of
#   the specified time steps. If "dates" is missing, it report on all images.
# - Set "col" if multiple collections exists (e.g. like with MODIS). The function
#   will focus on metadata files for that collection. Otherwise, all files are taken.

#------------------------------------------------------------------------------------------------#

satQuery <- function(path=path, ct=NULL, col=NULL, dates=NULL, tiles=NULL, ext=NULL, crs=NULL) {
  
  # check for and load packages
  pkgTest <- function(x){
    if (!require(x,character.only = TRUE)){
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  pkgTest("raster")

#------------------------------------------------------------------------------------------------#
  
  # check for paths
  mpath <- paste0(file.path(path), '/infos/metadata/')
  if (!dir.exists(mpath)) {return('error: metadata path not found')}
  if (!exists('col')) {return('error: "col" is missing')} else {
  
  # check dates
  if (!exists('dates')) {if (class(dates[1]!='Date')) {
    return('error: "dates" is not of a valid format')}}
  
  # check cloud thresold
  if (!is.null(ct)) {
    if (length(ct)>1) {return('error: "ct" has too many elemnents')}
    if(ct<0 | ct>100) {return('error: "ct" is not between 0 and 100')}
  } else {ct <- 100}
    
#------------------------------------------------------------------------------------------------#
  
  # if "tiles" is missing, query shapefile
  if (is.null(tiles)) {
    shp <- paste0(path, '/infos/tiles_shp.shp')
    if (file.exists(shp)) {return('error: "tiles.shp" not find in "infos" folder')}
    if (is.null(ext)) {return('error: "tiles" not set. "ext" is required')}
    if (is.null(rp)) {return('error: "ext" in use. "rp" is required')}
    shp <- shapefile(shp) # read shapefile
    tiles <- crop(shapefile(shp),ext)$tile # crop to extent
    rm(shp)
  }
    
#------------------------------------------------------------------------------------------------#
      
  # check if "col" is defined
  if(is.null(col)) {col=''}
    
  # check if metadata for tiles exists
  if (!is.character(tiles)) {return('error: "tiles" is not a character object')}
  files <- list.files(mpath, paste0(col, '.*metadata.csv'), full.names=T)
  ftile <- sapply(files, function(x) {strsplit(basename(x), '-')[[1]][1]})
  if (sum(tiles %in% ftiles)!=ength(tiles)) {
    return('error: one or more tiles not in the system')}
  
  # query metadata
  of <- list()
  for (t in 1:length(tiles)) {
    df <- read.csv(paste0(mpath, col, '_', tiles, '_metadata.csv')) # read metadata
    if (is.null(dates)) {ind <- which(df$ct < ct)}
    if (!is.null(dates)) {ind <- which(dates%in%as.Date(df$dates) & df$cloud<ct)}
    of[[t]] <- df$path[ind]
  }
    
#------------------------------------------------------------------------------------------------#
  
  # return list of paths
  return(unlist(of))
  
}