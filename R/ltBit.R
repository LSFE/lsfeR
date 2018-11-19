#' @title ltBit
#'
#' @description Translation of Landsat Collection 1 quality information.
#' @param path Acquisition path.
#' @import raster
#' @return A \emph{raster} file with the native extention of the input.
#' @details {The function translates Landsat Collection 1 bit quality information into an FMASK equivalent
#' with labels for Water (1), Cloud Shadow (2),Snow (3), Cloud (4) and No Data (255) and
#' provides metadata reporting on scene cloud cover. The output file will be names as \emph{"pixel_qa_mask"}}
#' @examples \dontrun{
#'
#' }
#' @export
#' @keywords Landsat, Quality, Storage

#---------------------------------------------------------------------------------------------------------------------#

ltBit <- function(path) {

#---------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#---------------------------------------------------------------------------------------------------------------------#

  if (!dir.exists(path)) {stop('data path not found')}

#---------------------------------------------------------------------------------------------------------------------#
# 1. determine input / output files
#---------------------------------------------------------------------------------------------------------------------#

# determine
  ii <- list.files(path, 'pixel_qa.tif', full.names=T)
  fe = extension(ii)
  oi <- paste0(strsplit(ii, fe), '_mask', fe)

#---------------------------------------------------------------------------------------------------------------------#
# 2. interpret quality information
#---------------------------------------------------------------------------------------------------------------------#

  # control variables for bit conversion
  a<-2^(0:15)
  b<-2*a

  rr0 <- raster(ii)
  rr1 = getValues(rr0)
  rb = as.integer((rr1 %% b[3])>=a[3]) # water
  rb = rb + (as.integer((rr1 %% b[4])>=a[4])*2) # cloud shadow
  rb = rb + (as.integer((rr1 %% b[6])>=a[6])*4) # cloud
  rb = rb + ((as.integer((rr1 %% b[5])>=a[5])*3)*(rb==0)) # snow 
  rb[is.na(rb)] = 255
  ccp <- sum(rb < 2) / sum(rb!=255) * 100
  rr0 = setValues(rr0, rb)

  rm(rr1, rb)

#---------------------------------------------------------------------------------------------------------------------#
# 3. return output
#---------------------------------------------------------------------------------------------------------------------#

  writeRaster(rr0, filename=oi, datatype='INT1U', overwrite=TRUE)

  return(ccp)
}
