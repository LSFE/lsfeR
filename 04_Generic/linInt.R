#-------------------------------------------------------------------------------------------------#
# Description
#-------------------------------------------------------------------------------------------------#
# Linear interpolation of MODIS 13Q1 product data for a reference list of dates.

# Variables
# ipath: Input tile path
# opath: output path
# tvi: target vegetation index (one of 'ndvi' or 'evi'). Default is 'ndvi'.
# td: Vector object of type "Date" with the dates that will be interpolated.
# bs: Time buffer expressed in number of days. Default is 60.
# xy: Object of class 'SpatialPoints' or 'SpatialPointsDataFrame'

# NOTES:
# - If "xy" is provided, the function only interpolates for the target samples.

#-------------------------------------------------------------------------------------------------#

linInt <- function(ipath=ipath, opath=opath, tvi='ndvi', td=td, bs=60, xy=NULL) {
  
  #check if package is isntalled
  pkgTest <- function(x){
    if (!require(x,character.only = TRUE)){
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  pkgTest("raster")
  
#-------------------------------------------------------------------------------------------------#
  
  # check variables
  if (!exists('ipath')) {stop('error: "ipath" is missing')}
  if (!dir.exists(ipath)) {stop('error: input path does not exist')}
  ipath <- file.path(ipath)
  if (!exists('opath')) {stop('error: "opath" is missing')}
  if (!dir.exists(opath)) {stop('error: output path does not exist')}
  opath <- file.path(opath)
  if (!exists('td')) {stop('error: "td" is missing')}
  if(class(td)!='Date') {stop('error: "td" is not a "Date" object')}
  if (length(tvi)!=1) {stop('error: "tvi" has too many arguments')}
  if (!tvi %in% c('ndvi', 'evi')) {stop('error: "tvi" is not valid (use one of "ndvi" or "evi")')}
  if (!is.null(xy)) {if (!class(xy)[1]%in%c('SpatialPoints', 'SpatialPointsDataFrame')) {
      stop('error: "shp is nor a valid point shapefile object')}}
  
#-------------------------------------------------------------------------------------------------#
  
  # list/read image data
  img.ls <- list.files(paste0(ipath, '/VI'), paste0(tvi, '.tif$'), full.names=T)
  iyr <- as.Date(sapply(img.ls, function(x) {
    paste0(substr(strsplit(basename(x), '[.]')[[1]][2], 2, 5),'-01-01')}))
  vis <- stack(img.ls)
  das <- stack(list.files(paste0(ipath, '/QA'), 'doa.tif$', full.names=T))
  if (nlayers(vis)!=nlayers(das)) {stop('error: different number of layers in VI and DOA stacks')}
  
#-------------------------------------------------------------------------------------------------#
  
  # identify target pixels
  if (!is.null(xy)) {
    re <- extent(vis) # raster extent
    pr <- res(vis)[1]/2 # 1/2 raster resolution
    nr <- dim(vis)[1] # number of rows in raster
    re[1] <- re[1]+pr # center xmn
    re[2] <- re[2]-pr # center xmx
    re[3] <- re[3]+pr # center ymn
    re[4] <- re[4]-pr # center ymx
    xc <- xy@coords[,1] # extract shapefile x coordinates
    yc <- xy@coords[,2] # extract shapefile y coordinates
    pr <- pr*2 # original pixel resolution
    up <- (round((re[4]-yc)/pr)+1) + nr * (round((xc-re[1])/pr)+1) # pixel positions
    up <- unique(up) # remove duplicates
  } else {
    nr <- dim(vis) # stack dimensions
    up <- 1:(nr[1]*nr[2]) # all pixels
  }
  
#-------------------------------------------------------------------------------------------------#  
  
  # extract data used during interpolation
  viData <- vis[up]
  daData <- das[up]
  nr <- nrow(viData)
  tmp <- vis[[1]]
  if (is.null(xy)) {tmp[,]<-NA} else {
    odf<-data.frame(date=td, matrix(0,length(td),nr), stringsAsFactors=F)}
  
#-------------------------------------------------------------------------------------------------#  
  
  # interpolation function
  int <- function(i) {
    vi <- as.numeric(viData[i,])
    da <- as.numeric(julian((iyr+daData[i,])-1))
    di <- which(da==pd)
    if (length(di)>0) {return(mean(vi[di]))} else {
      bi <- rev(which(!is.na(vi) & da < pd & da >= (pd-bs)))
      ai <- which(!is.na(vi) & da > pd & da <= (pd+bs))
      if (length(bi)>0 & length(ai)>0) {
        lc <- lm(c(vi[bi[1]],vi[ai[1]])~c(da[bi[1]],da[ai[1]]))
        return(as.numeric(pd*lc$coefficients[2]+lc$coefficients[1]))
      } else {return(NA)}}
  }
  
#-------------------------------------------------------------------------------------------------#  
  
  # build /write image for each date
  bn <- strsplit(basename(img.ls[1]), '[.]')[[1]][c(1,3,4,6)]
  for (d in 1:length(td)) {
    pd <- julian(td[d])
    r <- sapply(1:nr, int)
    pd <- paste0(strsplit(as.character(td[d]), '-')[[1]], collapse='') # processed date
    cd <- paste0(strsplit(as.character(Sys.Date()), '-')[[1]], collapse='') # processing date
    if (is.null(xy)) {
      r <- setValues(r, tmp)
      oname <- paste(bn[1], paste0('A', pd), bn[2], bn[3], bn[4], cd, 'tif', sep='.')
      writeRaster(r, paste0(opath, '/', oname), datatype='FLT4S')
    } else {odf[d,2:(nr+1)] <- r}
  }
  
  if (!is.null(xy)) {
    cd <- paste0(strsplit(as.character(Sys.Date()), '-')[[1]], collapse='') 
    pd1 <- paste0(strsplit(as.character(td[1]), '-')[[1]], collapse='') # start date
    pd2 <- paste0(strsplit(as.character(td[length(td)]), '-')[[1]], collapse='') # end date
    oname <- paste(bn[1], paste0(pd1, '-', pd2), bn[2], bn[3], bn[4], cd, 'csv', sep='.')
    write.csv(odf, paste0(opath, '/', oname), row.names=F)
  }
  
#-------------------------------------------------------------------------------------------------#  
  
}