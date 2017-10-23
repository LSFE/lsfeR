#' @title satQuery
#'
#' @description Returns Satellite tile for a given location.
#' @param sensor Target sensor. One of \emph{modis}, \emph{landsat} and \emph{sentinel2}.
#' @param ext A spatially projected object (e.g. \emph{raster}, \emph{SpatialPolygons}, \emph{SpatialPoints}).
#' @param shp Object of class \emph{SpatialPolygonsDataFrame}. Alternative to \☺emph{ext}.
#' @param tile Character with name for field in \emph{shp} that contains the tile ID's.
#' @import raster sp ggplot2
#' @return A \emph{raster} file with the native extention of the input.
#' @details {The function determines the corresponding tile that intersect with a given extent 
#' (\emph{ext}). Additionally, the function will also provide a plot showing the overlap between 
#' the tiles and the extent. At the moment, this function deals with a predefined set of sensor 
#' (keyword \emph{sensor}). If other sensors are required, the users can provide their own shapefile 
#' - using the keyword \emph{shp} - and tile information - using the keyword \emph{tile}.}
#' @examples \dontrun{
#'
#' }
#' @export
#' @keywords data selection

#---------------------------------------------------------------------------------------------------------------------#

satQuery <- function(sensor=NULL, ext=NULL, shp=NULL, tile=NULL) {

#---------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#---------------------------------------------------------------------------------------------------------------------#
  
  if (is.null(sensor)) {
    if (is.null(shp)) {stop('"sensor" not specified. Please provide a valid keyword of assign "shp".')}
    if (class(shp)!='SpatialPolygonsDataFrame') {stop('"shp" is not of a valid class.')}
    if (is.null(tile)) {stop('"shp" in use. Please assign a valid column name ot "tile".')}
  } else {if (!sensor%in%c('modis', 'landsat', 'sentinel2')) {stop('"sensor" is not a valid keyword.')}}
  if (is.null(ext)) {stop('please specify "ext"')}
  
#---------------------------------------------------------------------------------------------------------------------#
# 1. unpack / read shapefile
#---------------------------------------------------------------------------------------------------------------------#
  
  if (!is.null(sensor)) {
    file <- system.file('extdata', paste0(sensor, '-tiles.shp'), package="rsMove")
    if (file=='') {
      unzip(system.file('extdata', paste0(sensor, '.zip'), package="rsMove"), 
            exdir=system.file('extdata', package="rsMove"))
      file <- system.file('extdata', paste0(sensor, '-tiles.shp'), package="rsMove")}
    shp <- shapefile(file)
    tile <- 'tile'}
  
#---------------------------------------------------------------------------------------------------------------------#
# 2. derive shapefiel from reference extent
#---------------------------------------------------------------------------------------------------------------------#
  
  ext <- as(extent(projectExtent(extent(ext), crs(shp))), 'SpatialPolygons')
  crs(ext) <- crs(shp)
  
#---------------------------------------------------------------------------------------------------------------------#
# 3. determine required tiles and report
#---------------------------------------------------------------------------------------------------------------------#
  
  tiles <- intersect(shp, ext)@data[tile]
  return(tiles)

}
