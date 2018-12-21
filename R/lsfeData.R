#' @title lsfeData
#'
#' @description Creates a standardized folder structure.
#' @param path output data path.
#' @details {The function creates a standardized data structure to ease the transferability of
#' tools developed within the institute and thus facilitate internal cooperations. The proposed
#' structure consists of:
#' \itemize{
#' \item{\emph{"01_DATA"} - Storage of satellite data and metadata.}
#' \item{\emph{"02_ANALYSIS"} - Storage of output analysis based on data in "01_DATA".}
#' \item{\emph{"03_DOCUMENTS"} - General document folder.}
#' \item{\emph{"04_CODE"} - Folder to store developed code and/or required software.}
#' \item{\emph{"05_TMP"} - Temporary folder used for data "play".}}}
#' @examples \dontrun{
#'
#' }
#' @export
#' @seealso \code{\link{storeLandsat}}

#----------------------------------------------------------------------------------------------------#

lsfeData <- function(path) {

#----------------------------------------------------------------------------------------------------#
# 0. check input variables
#----------------------------------------------------------------------------------------------------#

  if (!dir.exists(path)) {stop('data path not found')} else {file.path(path)}

#----------------------------------------------------------------------------------------------------#
# 1. create folders
#----------------------------------------------------------------------------------------------------#

  oPath <- paste0(path, '/01_DATA/')
  if (!dir.exists(oPath)) {dir.create(oPath)}
  oPath <- paste0(path, '/02_ANALYSIS/')
  if (!dir.exists(oPath)) {dir.create(oPath)}
  oPath <- paste0(path, '/03_DOCUMENTS/')
  if (!dir.exists(oPath)) {dir.create(oPath)}
  oPath <- paste0(path, '/04_CODE/')
  if (!dir.exists(oPath)) {dir.create(oPath)}
  oPath <- paste0(path, '/05_SANDBOX/')
  if (!dir.exists(oPath)) {dir.create(oPath)}

#----------------------------------------------------------------------------------------------------#

}
