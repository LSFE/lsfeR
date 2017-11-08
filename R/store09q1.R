#' @title store09q1
#'
#' @description Storage and sorting of MODIS 09Q1 data.
#' @param ipath Path where the zipped landsat data is stored
#' @param opath Path where the unzipped landsat data should be stored.
#' @param remove.files Should the hdf files be deleted after unpacking? Optional logical argument. Default is FALSE.
#' @importFrom raster raster writeRaster
#' @importFrom gdalUtils gdal_translate
#' @return A folder structure containing the input Landsat data
#' @details {Local storing of landsat data as downloaded from the ESPA downloading service. A folder
#' named \emph{LANDSAT}} will be create in the target directory and the output will be split between
#' two sub-folder:
#' \itemize{
#' \item{\emph{"SR"} - Surface reflectances.}
#' \item{\emph{"infos"} - Metadata.}}
#' This function unzips the downloaded files and buils a standardized data structure from it with \emph{SR}
#' (i.e. TILE -> ACQUISITION -> FILES). Additionally, the function translates thebit quality information into
#' a clear pixel mask and applies it to the NDVI before writing.
#' provides metadata reporting on:
#' \itemize{
#' \item{\emph{"Date"} - Acquisition date.}
#' \item{\emph{"Product"} - Information on sensor and product.}
#' \item{\emph{"Directory"} - Path to where the data is stored.}
#' \item{\emph{"\% Clear"} -  Percent of usable pixels (excluding snow, shadows and clouds).}
#' \item{\emph{"Processed"} - Date when the file was processed.}}
#' @examples \dontrun{
#'
#' }
#' @export
#' @keywords internal

#---------------------------------------------------------------------------------------------------------------------#

store09q1 <- function(ipath, opath, remove.files=FALSE) {

  #---------------------------------------------------------------------------------------------------------------------#
  # 0. check input variables
  #---------------------------------------------------------------------------------------------------------------------#

  # test source/output directories
  if (!is.logical(remove.files)) {stop('"remove.files" is not a logical argument')}
  if (!exists('ipath')) {stop('"ipath" missing')} else {ipath <- file.path(ipath)}
  if (!exists('opath')) {stop('"opath" missing')} else {opath <- file.path(opath)}

  #---------------------------------------------------------------------------------------------------------------------#
  # 1. create base directories
  #---------------------------------------------------------------------------------------------------------------------#

  # create base directory
  satPath <- paste0(opath, '/modis/')
  if (!dir.exists(satPath)) {dir.create(satPath)}

  # create SR/infos folder
  mPath <- paste0(satPath, '/infos/') #  infos
  if (!dir.exists(mPath)) {dir.create(mPath)}
  rPath <- paste0(satPath, '/sr/') # reflectances
  if (!dir.exists(rPath)) {dir.create(rPath)}

  # control variables for bit conversion
  a<-2^(0:15)
  b<-2*a

  #---------------------------------------------------------------------------------------------------------------------#
  # 2. extract file info
  #---------------------------------------------------------------------------------------------------------------------#

  # list zip files
  files <- list.files(ipath, 'tar.gz', full.names=T)
  cc <- vector('numeric', length(files)) # clear pixels %

  # extract date information
  adate <- do.call('c', lapply(files, function(x) {
    tmp <- strsplit(basename(x), '[.]')[[1]][2]
    tmp1 <- substr(tmp, 2, 5)
    tmp2 <-  substr(tmp, 6, 9)
    tmp3 <- as.Date(paste0(tmp1, '-01-01')) + (as.numeric(tmp2)-1)
    return(tmp3)}))
  pdate <- sapply(files, function(x){as.Date(file.info(x)$ctime)})

  # extract sensor information
  sat <- sapply(files, function(x) {strsplit(basename(x), "[.]")[[1]][1]})

  # determine tiles
  tiles <- sapply(files, function(x) {strsplit(basename(x), '[.]')[[1]][3]})
  ut <- unique(tiles)

  #---------------------------------------------------------------------------------------------------------------------#
  # 3. unzip and store files
  #---------------------------------------------------------------------------------------------------------------------#

  # determine output directories
  odr <- as.character(sapply(files, function(x){paste0(tPath, strsplit(basename(x), '.hdf')[[1]][1])}))

  for (t in 1:length(ut)) {

    # make/check target directory
    tPath <- paste0(rPath, ut[t], '/')
    if(!dir.exists(tPath)) {dir.create(tPath)}
    ind <- which(tiles==ut[t])

    for (f in 1:length(ind)) {

      # dread raster (1)
      tmp1 <- tempfile(pattern="tmp1", tmpdir=tempdir(), fileext=".tif")
      gdal_translate(files[f], tmp1, sd_index=1)
      r1 <- raster(tmp1)

      # read raster (2)
      tmp2 <- tempfile(pattern="tmp2", tmpdir=tempdir(), fileext=".tif")
      gdal_translate(files[f], tmp2, sd_index=2)
      r2 <- raster(tmp2)

      # read raster (3)
      tmp3 <- tempfile(pattern="tmp3", tmpdir=tempdir(), fileext=".tif")
      gdal_translate(files[f], tmp3, sd_index=3)
      r3 <- raster(tmp3)

      # add raster data to raster list
      ndvi <- (r2-r1) / (r2+r1)
      r3 <- ((r3 %% b[1])>=a[1])^2 + ((r3 %% b[2])>=a[2])^2 + ((r3 %% b[3])>=a[3])^2
      ndvi[r3>0 | ndvi < -1 | ndvi > 1] <- NA
      ndvi <- ndvi * 10000

      oname <- paste0(odr[ind[f]], '/', basename(odr[ind[f]]), '.vi.tif')
      writeRaster(ndvi, oname, dataType="INT2S", overwrite=T)

      # if dealing with collection 1 translate quality layer
      cc[ind[f]] <- cellStats(!is.na(ndvi), sum) / ncell(ndvi)

      # remove
      rm(r1, r2, r3, ndvi)
      file.remove(tmp1, tmp2, tmp3)

    }

    # save metadata
    df <- data.frame(Date=adate[ind], Path=odr[ind], Product=sat[ind],
                     Cover=cc[ind], Processed=pdate[ind], stringsAsFactors=F)
    write.csv(df, paste0(mPath, ut[t], '_metadata.csv'))

  }

  # if prompted, delete zipped files
  if (remove.files) {for (f in 1:length(files)) {file.remove(files[f])}}

}
