#' @title storeLandsat
#'
#' @description Storage and sorting of Landsat Collection 1 data.
#' @param zpPath Path where the zipped landsat data is stored
#' @param dPath Path where the unzipped landsat data should be stored.
#' @param remove.files Should the zipped files be deleted after unpacking? Optional logical argument. Default is FALSE.
#' @import raster sp
#' @return A folder structure containing the input Landsat data
#' @details {Local storing of landsat data as downloaded from the ESPA downloading service. A folder
#' named \emph{LANDSAT}} will be create in the target directory and the output will be split between
#' two sub-folder:
#' \itemize{
#' \item{\emph{"sr"} - Surface reflectances.}
#' \item{\emph{"infos"} - Metadata.}}
#' This function unzips the downloaded files and buils a standardized data structure from it with \emph{sr}
#' (i.e. TILE -> ACQUISITION -> FILES). Additionally, the function translates thebit quality information into
#' an FMASK equivalent with labels for Water (1), Cloud Shadow (2),Snow (3), Cloud (4) and No Data (255) and
#' provides metadata reporting on:
#' \itemize{
#' \item{\emph{"Date"} - Acquisition date.}
#' \item{\emph{"Product"} - Information on sensor and product.}
#' \item{\emph{"Directory"} - Path to where the data is stored.}
#' \item{\emph{"Cover"} - Cloud cover percent.}
#' \item{\emph{"Processed"} - Date when the file was processed.}}
#' @examples \dontrun{
#'
#' }
#' @export
#' @keywords internal

#---------------------------------------------------------------------------------------------------------------------#

storeLandsat <- function(zpPath, dPath, remove.files=FALSE) {

#---------------------------------------------------------------------------------------------------------------------#
# 0. check input variables
#---------------------------------------------------------------------------------------------------------------------#

  # test source/output directories
  if (!is.logical(remove.files)) {stop('"remove.files" is not a logical argument')}
  if (!exists('zpPath')) {stop('"zpPath" missing')} else {zpPath <- file.path(zpPath)}
  if (!exists('dPath')) {stop('"dPath" missing')} else {dPath <- file.path(dPath)}

#---------------------------------------------------------------------------------------------------------------------#
# 1. create base directories
#---------------------------------------------------------------------------------------------------------------------#

  # create base directory
  ltPath <- paste0(dPath, '/landsat/')
  dir.create(ltPath)

  # create SR/infos folder
  mPath <- paste0(ltPath, '/infos/') #  infos
  if (!dir.exists(mPath)) {dir.create(mPath)}
  rPath <- paste0(ltPath, '/sr/') # reflectances
  if (!dir.exists(rPath)) {dir.create(rPath)}

#---------------------------------------------------------------------------------------------------------------------#
# 2. extract file info
#---------------------------------------------------------------------------------------------------------------------#

  # list zip files
  files <- list.files(zpPath, 'tar.gz', full.names=T)
  cc <- vector('numeric', length(files)) # clear pixels %

  # extract date information
  adate <- as.Date(paste0(substr(basename(files), 11, 14), '-',
                          substr(basename(files), 15, 16), '-',
                          substr(basename(files), 17, 18)))
  pdate <- sapply(files, function(x){strsplit(basename(x), '-')[[1]][2]})
  pdate <- as.Date(paste0(substr(basename(pdate), 3, 6), '-',
                          substr(basename(pdate), 7, 8), '-',
                          substr(basename(pdate), 9, 10)))

  # extract sensor information
  sat <- sapply(files, function(x) {strsplit(x, '_')[[1]][1]})

  # determine tiles
  tiles <- sapply(files, function(x) {substr(strsplit(basename(x), '-')[[1]][1], 5, 10)})
  ut <- unique(tiles)

#---------------------------------------------------------------------------------------------------------------------#
# 3. unzip and store files
#---------------------------------------------------------------------------------------------------------------------#

  # determine output directories
  odr <- as.character(sapply(files, function(x){paste0(tPath, strsplit(basename(x), '-')[[1]][1])}))

  for (t in 1:length(ut)) {

    # make/check target directory
    tPath <- paste0(rPath, ut[t], '/')
    if(!dir.exists(tPath)) {dir.create(tPath)}
    ind <- which(tiles==ut[t])

    for (f in 1:length(ind)) {

      # unzip file
      untar(files[ind[f]], exdir=odr[ind[f]], tar="internal")

      # if dealing with collection 1 translate quality layer
      cc[ind[f]] <- ltBit(odr[ind[f]])

    }

    # save metadata
    df <- data.frame(Date=adate[ind], Path=odr[ind], Product=sat[ind],
                     Cover=cc[ind], Processed=pdate[ind], stringsAsFactors=F)
    write.csv(df, paste0(mPath, ut[t], '_metadata.csv'))

  }

  # if prompted, delete zipped files
  if (remove.files) {for (f in 1:length(files)) {file.remove(files[f])}}

}
