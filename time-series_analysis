# ##################
# R code for time-series analysis using MODIS data
# February 2016
# author: Ruben Remelgado
# mail: first.lastname@uni-wuezburg.de
# purpose:
# MODIS data are automatically downloaded and converted to geotiff plus NDVI and quality layer extracted
# additionally the data sets are cropped and masked using the quality summary information (layer 12)
# simple temporal statistical metrics are derived, such as quarterly mean or s.d.

# load packages
library(gdalUtils)
library(raster)
library(rts)

  print('1. Setting working environment')
  setwd("path/to/folder/time-series/")
  
  
  iPath = "/modis/" # input path (directory containing hdf files)
  viPath = "/ndvi/" # ndvi path (directory for the ndvi images)
  qcPath = "/quality/" # data quality path (directory for the pixel quality images)
  tsPath = "/time-series/" # time-series analysis path (directory for the results)
  
  # create folder in file system
  dir.create("modis/")
  dir.create("ndvi/")
  dir.create("quality/")
  dir.create("time-series/")
  
  subset.ls = "study_sites.txt" # file containing subset coordinates
  
  #-----------------------------------------------------------------------------------------------------#
  
  print('2. reading subset coordinates')
  
  coord.ls = read.table(subset.ls, header=T, sep="")
  
  
  #-----------------------------------------------------------------------------------------------------#
  
  print('3. listing input files')
  
  img.ls <-  list.files(path=paste(".",iPath,sep=""), pattern="*.hdf$")
  
  #-----------------------------------------------------------------------------------------------------#
  
  print('4. extracting relevant information from files')
  
  nr = length(img.ls)
  
  for(i in 1:nr){
    
    print(paste('   ', 'processing', ' ', as.character(i), '/', as.character(nr), sep=''))
    
    #retrieve file base name
    bname = strsplit(basename(img.ls[i]), ".hdf")
      
    #retrieve data from hdf
    gdal_translate(paste(getwd(),iPath,img.ls[i],sep=""),paste(getwd(),viPath, bname, ".ndvi.tif",sep=""), sd_index=1, of="GTiff") # ndvi
    gdal_translate(paste(getwd(),iPath,img.ls[i],sep=""),paste(getwd(),qcPath, bname, ".qa.tif",sep=""), sd_index=12, of="GTiff") # pixel reliability info.              
                   
  }
  
  rm(img.ls)
  
  #-----------------------------------------------------------------------------------------------------#
  
  #read and mask ndvi data (for each bounding box) and derive temporal statistics
  
  ndvi.ls <-  list.files(paste(getwd(),viPath,sep=""), ".tif$") # list ndvi files
  qc.ls <-  list.files(paste(getwd(),qcPath,sep=""), ".tif$") # list quality data files
  
  for(c in 1:nrow(coord.ls)){
    
    #define extent object from coordinate list
    e = extent(c(coord.ls$xMin[c],coord.ls$xMax[c],coord.ls$yMin[c],coord.ls$yMax[c]))
    
    # create output path
    oPath = paste(getwd(),tsPath, coord.ls$sites[c], "/", sep="")
    if(file.exists(oPath)==FALSE){dir.create(oPath)}
    
    # read, mask and stack images
    ndvi.stack = stack()
    for(i in 1:length(ndvi.ls)){
      
      ndvi = crop(raster(paste(getwd(),viPath, ndvi.ls[i],sep="")), e) / (10000^2) # reda/crop ndvi image
      r.mask = crop(raster(paste(getwd(),qcPath,qc.ls[i],sep="")), e) # read / crop qc image
      ndvi[r.mask!=0] = NA
      
      ndvi.stack = addLayer(ndvi.stack, ndvi) # add image to ndvi stack
      
      rm(r.mask, ndvi) # clear memory
      
    }
  
    #-----------------------------------------------------------------------------------------------------#
      
    # create raster time series object
    
    # extract day of the year from starting / end image and infer corresponding dates
    # don't forget to consider the year
    s.ds = (strsplit(ndvi.ls[1], ".", fixed=T))[[1]][2] # string of date 1
    e.ds = (strsplit(ndvi.ls[length(ndvi.ls)], ".", fixed=T))[[1]][2] # string of date 2
    sdate = as.Date(as.numeric(substr(s.ds,6,8)), origin = paste(substr(s.ds,2,5), "-01-01", sep="")) - 1
    edate = as.Date(as.numeric(substr(e.ds,6,8)), origin = paste(substr(e.ds,2,5), "-01-01", sep="")) - 1
    
    rm(s.ds, e.ds) # clear memory
    
    # derive date object for 16 day NDVI
    # (if we have serveral years, we should build a new object for each!)
    time = seq(sdate,edate, by = 16)
    
    # derive time series raster object
    r.ts = rts(ndvi.stack, time)
    
    write.rts(r.ts, paste(getwd(),tsPath, "ndvi_time-series_2011", sep=""), overwrite=T)
    
    #-----------------------------------------------------------------------------------------------------#
    
    # perform time series analyis
    
    # define functions to use
    ts.fun.a = function(x){mean(x, na.rm=T)}
    ts.fun.b = function(x){sd(x, na.rm=T)}
    
    
    # apply functions for different time lenghts
    monthly.mean = apply.monthly(r.ts, ts.fun.a)
    monthly.sd = apply.monthly(r.ts, ts.fun.b)
    
    write.rts(monthly.mean, paste(getwd(),tsPath, "ndvi_time-series_2011_monthly-mean", sep=""), overwrite=T)
    write.rts(monthly.sd, paste(getwd(),tsPath, "ndvi_time-series_2011_monthly-sd", sep=""), overwrite=T)
    
    #save result plots
    p = plot(monthly.mean)
    jpeg(p, paste(getwd(),tsPath, "ndvi_time-series_2011_monthly-mean_plot", sep=""))
    dev.off()
    rm(p)
    p = plot(monthly.sd)
    jpeg(p, paste(getwd(),tsPath, "ndvi_time-series_2011_monthly-sd_plot", sep=""))
    dev.off()
    rm(p)
    
  }
