#--------------------------------------------------------------------------------------------------------#
# description
#--------------------------------------------------------------------------------------------------------------------#
# Establishes a standardized data structure with which Landsat metadata is stored after download.
# After acquiring the latest metadata files and retrieving a shapefile for the tiles the script
# returns a list of product ID's that can be used within ESPA's download servide (xxxx) to acquire
# LAndsat Surface Reflectance data.
#If the script needs to download metadata it will require a few extra minutes to complete the query.

# Input variables:
# - opath: path in which the metadata and scene ID's will be stored.
# - input: either an Extent object (in Lat/lon) or a vector of characters containing tile numbers.
# - sdt: Optional. date object (returned with as.Date()) containing the starting data for the query.
# - edt: Optional. date object (returned with as.Date()) containing the ending data for the query.
# - sensor: vector of target sensors.
# - cloud: Optional. Maximum percentage of cloud cover (0-100)

# author: Ruben Remelgado (ruben.remelgado@uni-wuerzburg.de)
#--------------------------------------------------------------------------------------------------------------------#

pickLandsat <- function(opath=opath, input=input, sdt=NULL, edt=NULL, cloud=NULL, sensor=c('TM', 'ETM', '8')) {
  
#--------------------------------------------------------------------------------------------------------------------#
# 0. url's for auxiliary data download
#--------------------------------------------------------------------------------------------------------------------#
  
  mdURL <- 'https://landsat.usgs.gov/landsat/metadata_service/bulk_metadata_files/' # metadata url
  shpURL <- 'https://landsat.usgs.gov/sites/default/files/documents/wrs2_descending.zip' # shapefile url
  
#--------------------------------------------------------------------------------------------------------------------#
# 1. check input data
#--------------------------------------------------------------------------------------------------------------------#

  if (!exists(opath)) {stop('error: opath is missing')}
  if (!exists(input)) {stop('error: input is missing')}
  if ((input)!='Extent') {
    if (class(input)!='character') {stop('error: input is neither an extent object or a valid tile number')} else {
      uv <- unique(sapply(input, function(x){nchar(x)}))
      if (length(uv > 1)) {stop('error: one or more tile numbers is incorrect')}
      if (uv!=5) {stop('error: one or more tile numbers is incorrect')}
      method <- 1
    }
  } else {method<-0}
  if (!exists(sensor)) {stop('error: sensor is missing')}
  st <- sensor %in% c('TM','ETM', '8')
  if (max(st)!=1) {stop('error: specified sensors not recognized')} else {
    if (min(st)==0) {
      if (sum(st==F)>1)) {st <- paste0(sensor[which(st==F)], sep=',')} else {st<-sensor[which(st==F)]}
      stop(paste0('error: sensor ', st, ' not recognized'))}
  }
  if (sum(c(is.null(sdt), is.null(edt))<2)) {stop('error: only one of sdt/edt provided. set both or keep as NULL')}
  if (!is.null(sdt) & !is.null(edt)) {
    if ((as.numeric(edt)-as.numeric(sdt))<0) {
      stop('error: starting date later than ending date')
      qm <- function(x) {which(x$cloudCover < cloud | 
                                 as.Date(x$aquisitionDate) >= sdt | 
                                 as.Date(x$aquisitionDate) <= edt | 
                                 x$pr %in% input)
    }
    } else {qm <- function(x) {which(x$cloudCover < cloud | x$pr %in% input)}
  if (!is.null(cloud)) {if(cloud>100 | cloud < 0) {stop('error: check cloud threshold value')}} else {cloud<-100}
  
#--------------------------------------------------------------------------------------------------------------------#
# 2. set base environment
#--------------------------------------------------------------------------------------------------------------------#
  
  # create directories
  mpath <- paste0(opath, '/infos/all/') #  metadata files
  if (!dir.exists(mpath)) {dir.create(mpath)}
  apath <- paste0(opath, '/infos/aux/') # tile shp
  if (!dir.exists(apath)) {dir.create(apath)}

  # download metadata (OLI)
  ofile <- paste0(mpath, 'LANDSAT_8_C1.csv.gz')
  dfile <- paste0(mdURL, 'LANDSAT_8_C1.csv.gz')
  if (!file.exists(file)) {download.file(dfile, destfile=ofile, mode="wb")} else {
    ct <- as.Date(file.info(file)$ctime)
    if (!is.null(edt)) {if((as.numeric(ct)-as.numeric(edt))<0) {download.file(dfile, destfile=ofile, mode="wb")}}
    if (is.null(edt)) {if((as.numeric(ct)-as.numeric(Sys.Date()))<0) {download.file(dfile, destfile=ofile, mode="wb")}}
    unzip(ofile, exdir=mpath)
    file <- paste0(mpath, 'LANDSAT_8_C1.csv')
    tmp <- read.csv(file, stringsAsFactors=F)
    file.remove(ofile)
    pr <- paste0(sprintf("%03d", tmp$path), sprintf("%03d", tmp$row))
    ad <- tmp$acquisitionDate
    id <- tmp$LANDSAT_PRODUCT_ID
    cc <- tmp$cloudCoverFull
    tmp <- data.frame(productID=id, cloudCover=cc, aquisitionDate=ad, pathRow=pr, stringsAsFactors=F)
    write.csv(tmp, file)
    rm(id, cc, ad, pr)
  }
  
  # download metadata (ETM)
  ofile <- paste0(mpath, 'LANDSAT_ETM_C1.csv.gz')
  dfile <- paste0(mdURL, 'LANDSAT_ETM_C1.csv.gz')
  if (!file.exists(file)) {download.file(dfile, destfile=ofile, mode="wb")} else {
    ct <- as.Date(file.info(file)$ctime)
    if (!is.null(edt)) {if((as.numeric(ct)-as.numeric(edt))<0) {download.file(dfile, destfile=ofile, mode="wb")}}
    if (is.null(edt)) {if((as.numeric(ct)-as.numeric(Sys.Date()))<0) {download.file(dfile, destfile=ofile, mode="wb")}}
    unzip(ofile, exdir=mpath)
    file <- paste0(mpath, 'LANDSAT_ETM_C1.csv')
    tmp <- read.csv(file, stringsAsFactors=F)
    file.remove(ofile)
    pr <- paste0(sprintf("%03d", tmp$path), sprintf("%03d", tmp$row))
    ad <- tmp$acquisitionDate
    id <- tmp$LANDSAT_PRODUCT_ID
    cc <- tmp$cloudCoverFull
    tmp <- data.frame(productID=id, cloudCover=cc, aquisitionDate=ad, pathRow=pr, stringsAsFactors=F)
    write.csv(tmp, file)
    rm(id, cc, ad, pr)
  }
  
  # download metadata (TM)
  ofile <- paste0(mpath, 'LANDSAT_TM_C1.csv.gz')
  dfile <- paste0(mdURL, 'LANDSAT_TM_C1.csv.gz')
  if (!file.exists(file)) {download.file(dfile, destfile=ofile, mode="wb")} else {
    ct <- as.Date(file.info(file)$ctime)
    if (!is.null(edt)) {if((as.numeric(ct)-as.numeric(edt))<0) {download.file(dfile, destfile=ofile, mode="wb")}}
    if (is.null(edt)) {if((as.numeric(ct)-as.numeric(Sys.Date()))<0) {download.file(dfile, destfile=ofile, mode="wb")}}
    unzip(ofile, exdir=mpath)
    file <- paste0(mpath, 'LANDSAT_TM_C1.csv')
    tmp <- read.csv(file, stringsAsFactors=F)
    file.remove(ofile)
    pr <- paste0(sprintf("%03d", tmp$path), sprintf("%03d", tmp$row))
    ad <- tmp$acquisitionDate
    id <- tmp$LANDSAT_PRODUCT_ID
    cc <- tmp$cloudCoverFull
    tmp <- data.frame(productID=id, cloudCover=cc, aquisitionDate=ad, pathRow=pr, stringsAsFactors=F)
    write.csv(tmp, file)
    rm(id, cc, ad, pr)
  }
  
  # check for/download tile shapefile
  ofile <- paste0(apath, 'wrs2_descending.shp')
  if (!file.exists(ofile)) {
    download.file(shpURL, destfile=ofile, mode="wb"
    unzip(ofile, exdir=apath)
    file.remove(paste0(apath, basename(fileURL)))
  }
  
  #--------------------------------------------------------------------------------------------------------------------#  
  # 3. query metadata
  #--------------------------------------------------------------------------------------------------------------------#
  
  # if input is an extent find required tiles
  if (method==0) {input <- crop(shapefile(paste0(apath,'wrs2_descending.shp')), se)@data$WRSPR}
  
 # find acquisitions
  ids <- vector('list', length(sensors))
  for (s in 1:length(sensors)) {
    meta <- read.csv(paste0(mpath, 'LANDSAT_', sensor[s], '_C1.csv'))
    ids[[s]] <- meta$productID=id[qm(meta)]
  }
  ids <- unlist(ids)
  write.csv(ids, paste0(mpath, 'data-query_', paste0(input, colapse='-'), as.character(Sys.Date()), '.csv'))
  
}