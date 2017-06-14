#setwd('D:/01_DATA/LANDSAT/SR')
#img.ls = list.files('.', 'pixel_qa.tif$', recursive=T, full.names=T)

# works for all landsat's (but specific to collection 1)
ltBitGet <- function(img) {
  
  a<-2^(0:15)
  b<-2*a
  
  for (i in 1:length(img.ls)) {
    rr0 = raster::raster(img.ls[i])
    rr1 = raster::getValues(rr0)
    rb = as.integer((rr1 %% b[3])>=a[3])
    rb = rb + (as.integer((rr1 %% b[4])>=a[4])*2)
    rb = rb + (as.integer((rr1 %% b[6])>=a[6])*4)
    rb = rb + ((as.integer((rr1 %% b[5])>=a[5])*3)*(rb==0))
    rb[is.na(rb)] = 255
    rr0 = raster::setValues(rr0, rb)
    fe = raster::extension(img.ls[i])
    oname = paste0(strsplit(img.ls[i], fe, '_mask', fe))
    raster::writeRaster(rr0, filename=oname, format='GTiff', datatype='INT1U', overwrite=TRUE)
    rm(rr0, rr1, rb, oname)
  }
  
}