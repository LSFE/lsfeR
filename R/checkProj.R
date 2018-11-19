#' @title checkProj
#'
#' @description Plots the distribution of images and area per spatial projection.
#' @param x \emph{character} vector with a list of paths for the target images or a \emph{RasterLayer} objects.
#' @importFrom raster crs res, extent
#' @importFrom tmaptools get_proj4
#' @return A \emph{list} object.
#' @details {For each element in \emph{x}, the function extracts the projection string and estimates the area 
#' based on the pixel resolution and the number of cells. returns its area. Then, The function identifies the 
#' unique projections and reports on the number of occurrences and the total area for each of them. The area 
#' is defined by the maximum extent off all images of a given projection avoiding the over-estimation of the 
#' area covered by e.g. tiles with a higher temporal coverage.. The output consists of:
#' @itemize{
#'  \item{\emph{image.df} - \emph{data.frame} with image-wise reports on projection and area.}
#'  \item{\emph{proj.df} - \emph{data.frame} with the overal image count and area per unique projection.}
#'  \item{\emph{count.plot} - ggplot object with the distribution of images per unique projection.}
#'  \item{\emph{area.plot} - ggplot object with the distribution of area per unique projection.}
#' }}
#' @export
#' @keywords Landsat, Storage

#---------------------------------------------------------------------------------------------------------------------------------------#

checkProj <- function(x) {
  
#---------------------------------------------------------------------------------------------------------------------------------------#
# 0. check and pre-process input data
#---------------------------------------------------------------------------------------------------------------------------------------#

  # if x is a character vector, build list of raster objects
  if (is.character(x)) {
    x <- lapply(x, function(i) {
      r <- raster(i)
      e <- try(extent(i), silent=TRUE)
      if (class(e)[1] == "try-error") {return(NULL)} else {return(r)}
    })
  }

  # check if x is of a valid format
  if (!class(x)[1] %in% c("character", "list")) {stop('"x" is not of a valid class')}

  # count the number of images for which a projection could not be read
  failed.nr <- sum(sapply(x, function(i) {is.null(x)}))
  if (failed.nr > 0) {warning('one or more elements in "x" could not be checked (see output projection list for NULL occurrences)')}
  if (failed.nr == length(x)) {stop('there are no valid raster objects in "x"')}

#---------------------------------------------------------------------------------------------------------------------------------------#
# 1. analyze each image
#---------------------------------------------------------------------------------------------------------------------------------------#

  # extract projection data for each image
  odf1 <- do.call(rbind, lapply(x, function(i) {
    if (!is.null(i)) {
      ip <- get_proj4(i, output=c("character", "espg")) # extarct projection data
      return(odf=data.frame(crs=ip[1], espg=ip[2], res=res(i)[1]))
    } else {return(data.frame(crs=NA, espg=NA, res=res(i)[1]))}
  }))


  # extract data on the extent of each image
  ext <- do.call(rbind, function(x, function(i) {if (!is.null(i)) {return(as.numeric(as.character(extent(i))))} else {return(c(0, 0, 0, 0))}}))

#---------------------------------------------------------------------------------------------------------------------------------------#
# 2. projection frequency analysis
#---------------------------------------------------------------------------------------------------------------------------------------#

  odf2 <- odf1[!duplicated(odf1$espg),1:2] # limit data frame to unique occurrences
  odf2$count <- sapply(odf2$espg, function(i) {sum(odf1$espg == i)}) # frequency of unique occurrences
  odf2$rel.count <- odf2$count / sum(odf2$count) * 100 # add relative count to the data.frame
  
  # area of the extent covered by each occurrence
  odf2$area <- sapply(odf2$espg, function(i) {
    ind <- which(odf1$espg == i)
    e <- extent(c(min(ext[ind,1]), max(ext[ind,2]), max(ext[ind,3]), max(ext[ind,4])))
    r <- raster(e, res=mean(odf1$res[ind]), crs=crs(odf1$crs[ind[1]]))
    return(area(r))
  })

  odf2$rel.area <- odf2$area / sum(odf2$area)

#---------------------------------------------------------------------------------------------------------------------------------------#
# 3. build informative plots
#---------------------------------------------------------------------------------------------------------------------------------------#

  p1 <- ggpplot(odf2, aes_string(x="espg", y="rel.count")) + geom_bar(stat="identity") + them_bw() + ylim(0,100) # relative count plot
  p2 <- ggpplot(odf2, aes_string(x="espg", y="rel.area")) + geom_bar(stat="identity") + them_bw() + ylim(0,100) # relative area plot

#---------------------------------------------------------------------------------------------------------------------------------------#
# 4. build output
#---------------------------------------------------------------------------------------------------------------------------------------#
  
  return(list(image.df=odf1, proj.df=odf2, count.plot=p1, area.plot=p2))

}
