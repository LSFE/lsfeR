#' @title estimateUTM
#'
#' @description Determines adequate UTM zone for geographical coordinates.
#' @param x Vector of x coordinates.
#' @param y Vector of y coordinates.
#' @param ext Object of class \emph{Extent}.
#' @details {For each coordinate pair, this function returns:
#' \itemize{
#' \item{\emph{"zones"} - UTM zone.}
#' \item{\emph{"orientation"} - UTM zone hemisphere ("N" or "S").}
#' \item{\emph{"zone.crs"} - Correct zone projection code.}
#' \item{\emph{"landsat.crs"} - Landsat projection code (always uses the northern hemisphere projection).}}
#' @importFrom raster crs
#' @export
#' @keywords Landsat, Storage
#' @seealso \code{\link{satQuery}}

#--------------------------------------------------------------------------------------------------------------------------------------------#

estimateUTM <- function(x=NULL, y=NULL, ext=NULL) {

#--------------------------------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#--------------------------------------------------------------------------------------------------------------------------------------------#

  # check x and y
  if ((is.null(x) & !is.null(y))) {warning('"x" is missing but "y" is provided. Trying to use "ext".')}
  if ((!is.null(x) & is.null(y))) {warning('"x" is provided but "y" is missing. Trying to use "ext".')}  

  # use x and y (if both are provided)
  if (!is.null(x) & !is.null(y)) {
    if (length(x)!=length(y)) {stop('lengths of "x" and "y" do not match')}
    xy <- data.frame(x=x, y=y)}
  
  # retrieve coordinates from extent (if x and y are not provided)
  if (is.null(x) & is.null(y)) {
    if (class(ext)!="Extent") {stop('"ext" is not of class "extent"')} else {
      xy <- rbind(c(ext[1],ext[4]), c(ext[2],ext[4]), c(ext[2],ext[3]), c(ext[1],ext[3]), c(mean(ext[1:2]), mean(ext[3:4])))
      xy <- data.frame(x=xy[,1], y=xy[,1])
    }
  }
  
  # stop if no variable is provided
  if (!is.null(x) & !is.null(y) & !is.null(ext)) {stop('no variable provided. aborting operation.')}
    
#--------------------------------------------------------------------------------------------------------------------------------------------#
# 1. determine adequate UTM zone
#--------------------------------------------------------------------------------------------------------------------------------------------#
  
  zones <- do.call(rbind, lapply(1:nrow(xy), function(i) {
    if (xy[i,1] > 0) {o <-'N'} else {o <-'S'}
    if (xy[i,2] > 0) {cz <- round(xy[i,2]/6.)+31} else {cz <- round((180+xy[i,2])/6)+1}
    return(data.frame(zone=cz, orientation=o))}))
  
#--------------------------------------------------------------------------------------------------------------------------------------------#
# 2. build proj4 string
#--------------------------------------------------------------------------------------------------------------------------------------------#
  
  ltp <- paste0('+proj=utm +zone=', as.character(zones$zone), '+ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
  if (o == 'S') {crp <- ltp}
  if (o == 'S') {crp <- paste0('+proj=utm +zone=', as.character(zones$zone), '+south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')}
  
#--------------------------------------------------------------------------------------------------------------------------------------------#
# 4. return list of outputs
#--------------------------------------------------------------------------------------------------------------------------------------------#
  
  return(list(zone=zones$zone, orientation=zone$orientation, zone.crs=crs(crp), landsat.crs=crs(ltp)))
  
}
