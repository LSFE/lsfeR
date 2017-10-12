#' @title estimateUTM
#'
#' @description Determines adequate UTM zone for geographical coordinates.
#' @param ext Object of class \emph{extent} or one from which an extent can be derived. Alternatively, a vector or coordinates can be given.
#' @importFrom raster crs
#' @export
#' @keywords Landsat, Storage
#' @seealso \code{\link{satQuery}}

#--------------------------------------------------------------------------------------------------------------------------------------------#

estimateUTM <- function(ext) {

#--------------------------------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#--------------------------------------------------------------------------------------------------------------------------------------------#

  # retrieve center coordinate
  if (class(ext)!="character") {
    if (class(extent(r))[1]!="Extent") {ext <- extent(ext)}
    ext <- c(mean(ext[1:2]), mean(ext[3:4]))
  }

#--------------------------------------------------------------------------------------------------------------------------------------------#
# 1. determine adequate UTM zone
#--------------------------------------------------------------------------------------------------------------------------------------------#

  if (ext[1] > 0) {o <-'N'} else {o <-'S'}
  if (ext[2] > 0) {cz <- round(ext[2]/6.)+31} else {cz <- round((180+ext[2])/6)+1}
  return(list(zone=cz, orientation=o, crs=crs()))
}

