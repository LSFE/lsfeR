#---------------------------------------------------------------------------------------------------------------------------------------------------#
#subset modis data

modis_subset = function(ipath=ipath, opath=opath, se=se, oproj=oproj, tiles=tiles, year=year, bs=bs) {
  
  library(raster)
  
  # determine subset coordinates and target tiles
  shp = shapefile(tiles)
  ss = as(se, 'SpatialPolygons')
  crs(ss) <- oproj
  e = extent(spTransform(ss, crs(shp)))
  s = crop(shapefile(tiles),e)
  h = s$h
  v = s$v
  tile = c()
  for(t in 1:length(s$h)) {
    if(s$v[t] < 10){v = paste('0', as.character(s$v[t]), sep='')} else {v = as.character(s$v[t])}
    if(s$v[t] < 10){v = paste('0', as.character(s$v[t]), sep='')} else {v = as.character(s$v[t])}
    tile = c(tile, paste('h', h, 'v', v, sep=''))
  }
  tile = unique(tile)
  
  # test if tiles really overlap (shapefile does not fit to actual data)
  check = c()
  for (t in 1:length(tile)) {
    re = extent(raster((list.files(paste(ipath,tile[t], sep=''), paste('A', as.character(year), '.*.ndvi.tif', sep=''), recursive=T, full.names=T))[1]))
    check = c(check,re@xmin > e@xmax || re@xmax < e@xmin || re@ymax < e@ymin || re@ymin > e@ymax)
  }
  tile = tile[which(check == FALSE)]
  
  # update year list
  yr.ls = year
  if(bs > 0) {
    for (y in 1:bs) {
      yr.ls = c(yr.ls, year-y)
      yr.ls = c(yr.ls, year+y)
    }
  }
  yr.ls = sort(unique(yr.ls))
  
  # mosaic time-steps (if dealing with more than one tile)
  for (y in 1:length(yr.ls)) {
    
    if (length(tile) > 1) {
      
      # list images
      imgls1_a = list.files(paste(ipath,tile[1], sep=''), paste('A', yr.ls[y], '.*.ndvi.tif', sep=''), recursive=T, full.names=T)
      imgls1_b = list.files(paste(ipath,tile[2], sep=''), paste('A', yr.ls[y], '.*.ndvi.tif', sep=''), recursive=T, full.names=T)
      imgls2_a = list.files(paste(ipath,tile[1], sep=''), paste('A', yr.ls[y], '.*.prel.tif', sep=''), recursive=T, full.names=T)
      imgls2_b = list.files(paste(ipath,tile[2], sep=''), paste('A', yr.ls[y], '.*.prel.tif', sep=''), recursive=T, full.names=T)
      imgls3_a = list.files(paste(ipath,tile[1], sep=''), paste('A', yr.ls[y], '.*.doa.tif', sep=''), recursive=T, full.names=T)
      imgls3_b = list.files(paste(ipath,tile[2], sep=''), paste('A', yr.ls[y], '.*.doa.tif', sep=''), recursive=T, full.names=T)
      
      # set output path
      setwd(opath)
      
      # extract/mosaic image subset
      for(i in 1:length(imgls1_a)) {
        
        # mosaic and reproject
        r1 = crop(raster(imgls1_a[i]), e) / 10000^2
        r1[r1 <= -0.3] = NA
        r1[crop(raster(imgls2_a[i]), e) > 1] = NA
        r2 = crop(raster(imgls1_b[i]), e)/ 10000^2
        r2[r2 <= -0.3] = NA
        r2[crop(raster(imgls2_b[i]), e) > 1] = NA
        ndvi = projectRaster(mosaic(r1,r2, fun=mean), res=250, crs=crs(oproj))
        r1 = crop(raster(imgls3_a[i]), e)
        r2 = crop(raster(imgls3_b[i]), e)
        doa = projectRaster(mosaic(r1,r2, fun=mean), res=250, crs=crs(oproj))
        
        # write layer
        writeRaster(ndvi, filename=basename(imgls1_a[i]), driver='GTiff', datatype='FLT4S')
        writeRaster(doa, filename=basename(imgls3_a[i]), driver='GTiff', datatype='INT4S')
        
      }
      
      rm(imgls1_a, imgls1_b, imgls2_a, imgls2_b, imgls3_a, imgls3_b, r1, r2)
      
    } else {
      
      # list images
      imgls1 = list.files(paste(ipath,tile[1], sep=''), paste('A', yr.ls[y], '.*.ndvi.tif', sep=''), recursive=T, full.names=T)
      imgls2 = list.files(paste(ipath,tile[1], sep=''), paste('A', yr.ls[y], '.*.prel.tif', sep=''), recursive=T, full.names=T)
      imgls3 = list.files(paste(ipath,tile[1], sep=''), paste('A', yr.ls[y], '.*.doa.tif', sep=''), recursive=T, full.names=T)
      
      # set output path
      setwd(opath)
      
      # extract image subset
      for(i in 1:length(imgls1)) {
        
        # mask outliers / bad quality pixels
        tmp = crop(raster(imgls1[i]),e) / 10000^2
        tmp[tmp <= -0.3] = NA
        tmp[crop(raster(imgls2[i]), e) > 1] = NA
        
        # reproject layers
        ndvi = projectRaster(tmp, res=250, crs=crs(oproj))
        doa = projectRaster(crop(raster(imgls3[i]),e), res=250, crs=crs(oproj))
        
        # write layer
        writeRaster(ndvi, filename=basename(imgls1[i]), driver='GTiff', datatype='FLT4S')
        writeRaster(doa, filename=basename(imgls3[i]), driver='GTiff', datatype='INT4S')
        
      }
      
      rm(imgls1, imgls2, imgls3, tmp)
      
    }
  }
}