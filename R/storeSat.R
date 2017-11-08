#' @title storeSat
#'
#' @description Temporal linear interpolation of raster data.
#' @param ipath Path where the original data is stored.
#' @param oPath Path where the unpacked data will be stored and sorted.
#' @param sensor {Target satellite data. One of:
#' \itemize{
#' \item{\emph{"landsat"} - Landsat Collection 1 Surface reflectances acquired through \href{https://espa.cr.usgs.gov/}{ESPA}.}
#' \item{\emph{"modis09q1"} - MODIS 250m 8-day surface reflectances (NDVI estimated within script).}
#' \item{\emph{"modis13q1"} - MODIS 250m 16-day NDVI composites.}}}
#' @param remove.files Should the zipped files be deleted after unpacking? Optional logical argument. Default is FALSE.
#' @import raster sp
#' @importFrom LSFE storeLandsat store09q1 store13q1
#' @return A folder structure containing the input satellite data
#' @details {Local storing and sorting of satellite data. A folder
#' named after the specified product (e.g. \emph{"LANDSAT"}) will be create in the target directory and the output will
#' be split between two sub-folder:
#' \itemize{
#' \item{\emph{"SR"} - Surface reflectances.}
#' \item{\emph{"infos"} - Metadata for each tile.}}
#' This function unpacks the downloaded files and buils a standardized data structure from it within \emph{SR}
#' (i.e. TILE -> ACQUISITION -> FILES). Additionally, the function translates bit quality information when necessary.
#' In the case of Landsat,the function will provide an FMASK equivalent. In the case of MODIS, the quality mask will
#' be applied to the NDVI when using the 09Q1 product.However, for the 13Q1 product, the function will return EVI,
#' quality information and day of acquisition layer. Finnaly, the function stores per-tile metadata within
#' the \emph{infos} folder reporting on:
#' \itemize{
#' \item{\emph{"Date"} - Acquisition date.}
#' \item{\emph{"Directory"} - Path to where the data is stored.}
#' \item{\emph{"sensor"} - Landsat Sensor (e.g. Landsat 8).}
#' \item{\emph{"\% Clear"} -  Percent of usable pixels (excluding snow, shadows and clouds).}
#' \item{\emph{"Processed"} - Date when the file was processed.}}
#' @note The output imagery is stored in signed-integer format and scaled by a factor of 10000.
#' @examples \dontrun{
#'
#' }
#' @note All MODIS products will be stored within the same tile folder.
#' @export
#' @keywords Landsat MODIS Storage

#---------------------------------------------------------------------------------------------------------------------#

storeSat <- function(ipath, opath, sensor, remove.files=FALSE) {

#---------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#---------------------------------------------------------------------------------------------------------------------#

  # test source/output directories
  if (!is.logical(remove.files)) {stop('"remove.files" is not a logical argument')}
  if (!exists('ipath')) {stop('"ipath" missing')} else {
    ipath <- file.path(ipath)
    if(!dir.exists(ipath)) {stop('"ipath" does not exist')}}
  if (!exists('opath')) {stop('"opath" missing')} else {
    ipath <- file.path(opath)
    if(!dir.exists(ipath)) {stop('"ipath" does not exist')}}
  if (sensor%in%c("landsat", "modis09q1", "modis13q1")) {stop('"sensor" is not a valid keyword')}

#---------------------------------------------------------------------------------------------------------------------#
# 1. build data structure
#---------------------------------------------------------------------------------------------------------------------#

  if (sensor=="landsat") {storeLandsat(ipath, opath, remove.files=remove.files)}
  if (sensor=="modis09q1") {store09q1(ipath, opath, remove.files=remove.files)}
  if (sensor=="modis13q1") {store13q1(ipath, opath, remove.files=remove.files)}

}
