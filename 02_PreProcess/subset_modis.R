#---------------------------------------------------------------------------------------------------------------------------------------------------#
# Description
#---------------------------------------------------------------------------------------------------------------------------------------------------#
# Pre-processing of MODIS 13Q1 data. Allows for cropping, masking and reprojecting.

# Variables
# ipath: path to the 13Q1 parent directory.
# opath: path to output folder.
# tile: Character vector of target tiles (e.g. c('h18v04', 'h18v05')
# td: Vector of class "Date" with target dates.
# tvi: one of "ndvi" or "evi". Specifies which vegetation index will be used.
# cc: one of "strict", "permissive" or "none".
# col: String element specifying the target collection (e.g. "006").
# prod: Specifies the target product. One of "MOD" (for TERRA), "MYD" (for AQUA) or "all".
# rr: object of class "RasterLayer", "RasterStack" or "RasterBrick".

#Notes:
# - "cc" is used for cloud control. If "strict" only good quality pixels are mantained. 
#     The "permissive" setting allows for the inclusion of pixels with "marginal" quality.
#     "none" will not perform any masking. If selected, the code will also write the pixel reliability layer.
# - "rr" provides access to a reference raster layer which will be used for cropping.
#     If "rr" has different projection the output will be reprojected. When reprojecting, 
#     the resolution of "rr" will define the resolution of the output.

#---------------------------------------------------------------------------------------------------------------------------------------------------#
#subset modis data

pp13Q1 = function(files=files, opath=opath, tvi='ndvi', cc='strict', rr=NULL) {

#---------------------------------------------------------------------------------------------------------------------------------------------------#
  
  #check if package is isntalled
  pkgTest <- function(x){
    if (!require(x,character.only = TRUE)){
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  pkgTest("raster")
  
  # base projection system (sinusoidal)
  bp <- crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
#---------------------------------------------------------------------------------------------------------------------------------------------------#
  
  # check paths
  if (!exists('ipath')) {stop('error: "ipath" is missing')}
  if (!dir.exists(ipath)) {stop('error: input path does not exist')}
  ipath <- file.path(ipath) # remove unwanted separators
  if (!exists('opath')) {stop('error: "opath" is missing')}
  opath <- file.path(opath) # remove unwanted separators
  if (!dir.exists(opath)) {stop('error: output path does not exist')}
  
  # check if target collection exists
  if (length(col)!=1) {stop('error: "col" has too many arguments')}
  if (!dir.exists(paste0(ipath, '/', col))) {stop('error: collection not found')}
  
  # check tiles
  if (!exists('tile')) {stop('error: "tile" is missing')}
  tpath <- vector('character', length(tile))
  for (t in 1:length(tile)) {
    tpath[t] <- paste0(ipath, '/', tile[t], '/')
    if (!dir.exists(tpath[t])) {stop(paste0('error: tile ', tile[t], ' missing'))}
  }
  
  # check data constraints
  if (!exists('td')) {stop('error: "td" is missing')}
  if (class('td')[1]!='Date') {stop('error: "td" is not a "Date" object')}
  if (length(tvi)!=1) {stop('error: "tvi" has too many arguments')}
  if (!tvi %in% c('ndvi', 'evi')) {stop('error: "tvi" is not valid (use one of "ndvi" or "evi")')}
  
  # check data manipulation constraints
  if (length(cc)>1) {stop('error: "cc" has too  many arguments')}
  if (!is.null(cc)) {if (min(cc%in%c('strict', 'permissive', 'none'))==0) {
    stop('error: "cc" not valid (use "strict", "permissive" or "none")')}} else {cc<-'strict'}
  
  # check reference raster
  if (!is.null(rr)) {
    if (!class(rr)[1] %in% c('RasterLayer', 'RasterStack', 'RasterBrick')) {
      stop('error: "re" is not a valid raster object')
    } else {
      rp <- crs(rr) # target projection
      pr <- res(rr) # target resolution
      if (is.null(rp@projargs)) {stop('error: "re" does not contain a valid projection')}
      if (bp@projargs!=rp@projargs) {re<-projectExtent(rr, bp)} else {re<-extent(rr)}
    }
  }

#---------------------------------------------------------------------------------------------------------------------------------------------------#
  
  # list files
  vi.ls <- list.files(tpath, paste0(tvi, '.tif$'), recursive=T, full.names=T) # vegetation index
  da.ls <- list.files(tpath, 'doa.tif$', recursive=T, full.names=T) # day of acquisition
  pr.ls <-list.files(tpath, 'prel.tif$', recursive=T, full.names=T) # pixel reliability
  
  # check if number of files is consistent
  l1 <- length(vi.ls)
  l2 <- length(da.ls)
  l3 <- length(pr.ls)
  if (length(unique(c(l1, l2, l3)))>1) {
    print(paste0('nr. of vi images:', as.character(l1)))
    print(paste0('nr. of doa images:', as.character(l3)))
    print(paste0('nr. of prel images:', as.character(l3)))
    stop('error: different number of files per category')
  }
  
#---------------------------------------------------------------------------------------------------------------------------------------------------#
  
  # check if image pairs come from the same acquistion)
  c1 <- sapply(vi.ls, function(x) {strsplit(basename(x), paste0(tvi, '.tif'))[[1]][1]})
  c2 <- sapply(da.ls, function(x) {strsplit(basename(x), 'doa.tif')[[1]][1]})
  c3 <- sapply(pr.ls, function(x) {strsplit(basename(x), 'prel.tif')[[1]][1]})
  cv <- (c1==c2) + (c1==c3) + (c2==c3)
  if (min(cv)<3) {
    return(which(cv<3))
    stop('error: one or more image pair are not from the same acquistion')
  }

  rm(c1, c2, c3, cv)

#---------------------------------------------------------------------------------------------------------------------------------------------------#

  # create output directories
  viPath <- paste0(opath, '/VI/')
  if (!dir.exists(viPath)) {dir.create(viPath)}
  qaPath <- paste0(opath, '/QA/')
  if (!dir.exists(qaPath)) {dir.create(qaPath)}
  
#---------------------------------------------------------------------------------------------------------------------------------------------------#
  
  for (i in 1:l1) {
    
    # read images
    if (is.null(rr)) {
      vi <- raster(vi.ls[i])
      da <- raster(da.ls[i])
      pr <- raster(pr.ls[i])
    } else {
      vi <- crop(raster(vi.ls[i]), re)
      da <- crop(raster(da.ls[i]), re)
      pr <- crop(raster(pr.ls[i]), re)
    }
    
    # mask vi image (if cc is "none" no masking is performed)
    if (cc=='strict') {
      vi[pr>0] <- NA
      da[pr>0] <- NA
    }
    if (cc=='premissive') {
      vi[pr>1] <- NA
      da[pr>1] <- NA
    }
    
    # reproject images if required
    if (!is.null(rr)) {
      if (bp@projargs!=rp@projargs) {
        vi <- projectRaster(vi, rr, res=pr)
        da <- projectRaster(da, rr, res=pr)
        if (cc=='none') {pr <- projectRaster(da, rr, res=pr)}
      }
    }
    
    # write images
    oname <- paste0(viPath, basename(vi.ls[i]))
    writeRaster(vi*0.0001^2, oname, overwrite=T)
    oname <- paste0(qaPath, basename(da.ls[i]))
    writeRaster(da, oname, overwrite=T)
    if (cc=='none') {
      oname <- paste0(qaPath, basename(pr.ls[i]))
      writeRaster(pr, oname, overwrite=T)
    }
    
  }

#---------------------------------------------------------------------------------------------------------------------------------------------------#

}