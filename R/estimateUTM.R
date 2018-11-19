#' @title estimateUTM
#'
#' @description Determines adequate UTM zone for geographical coordinates.
#' @param x An object of class \emph{matrix}, \emph{data.frame} - with 2 columns containing x and y coordinates - or an \emph{extent} object.
#' @return A \emph{data.frame}.
#' @details {If \emph{x} is a \emph{matrix} or a \emph{data.frame}, the function looks at each rown and reports on:
#' \itemize{
#' \item{\emph{"zone"} - UTM zone.}
#' \item{\emph{"orientation"} - UTM zone hemisphere ("N" or "S").}
#' \item{\emph{"correct.crs"} - Correct zone projection code.}
#' \item{\emph{"landsat.crs"} - Landsat projection code (always uses the northern hemisphere projection).}}
#' If \emph{x} is an \emph{extent} object, the function will first derivre coordinate pairs for the corner and center of the object.
#'
#' }
#' @importFrom raster crs
#' @export
#' @keywords Landsat, Storage
#' @seealso \code{\link{satQuery}}

#--------------------------------------------------------------------------------------------------------------------------------------------#

estimateUTM <- function(x) {

#--------------------------------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#--------------------------------------------------------------------------------------------------------------------------------------------#

  if (!class(x)[1] %in% c("data.frame", "matrix", "extent")) {stop('"x" is not of a valid class')}

  # if x is an extent, derive coordinates for corners and center
  if (class(x)=="Extent") {x <- rbind(c(x[1],x[4]), c(x[2],x[4]), c(x[2],x[3]), c(x[1],x[3]), c(mean(x[1:2]), mean(x[3:4])))}

#--------------------------------------------------------------------------------------------------------------------------------------------#
# 1. determine adequate UTM zone
#--------------------------------------------------------------------------------------------------------------------------------------------#

  zones <- do.call(rbind, lapply(1:nrow(x), function(i) {
    if (x[i,1] > 0) {o <-'N'} else {o <-'S'}
    if (x[i,2] > 0) {cz <- round(x[i,2]/6.)+31} else {cz <- round((180+x[i,2])/6)+1}
    ltp <- paste0('+proj=utm +zone=', as.character(cz), '+ellps=WGS84 +datum=WGS84 +units=m +no_defs')
    if (o == 'N') {crp <- ltp}
    if (o == 'S') {crp <- paste0('+proj=utm +zone=', as.character(cz), '+south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')}
    return(data.frame(zone=cz, orientation=o, correct.crs=crp, landsat.crs=ltp, stringsAsFactors=FALSE))}))

  return(zones)

}
