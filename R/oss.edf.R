#' Calculate Euclidean Distance Fields from a raster
#'
#' Calculate Euclidean distance fields for an input raster.
#' Calculate distance from each raster cell to NW, NE, SW,
#' SE, Y max, X max and middle of the raster. Outputs to a SpatRaster.
#'
#' @param x SpatRaster, SpatialPixelsDataFrame or RasterLayer* object
#'
#' @return SpatRaster* object
#'
#' @importFrom terra "rast"
#'
#' @export
#'
#' @examples
#' #Calculate Euclidean distance fields from a DEM raster
#' library(terra)
#' data(keene)
#' keene<- terra::rast(keene, type="xyz")
#' terra::crs(keene)<- "epsg:26917"
#' oss.edf(keene)
#'
#'
oss.edf<- function(x){

  # for compatibility with raster, if x is a RasterLayer, will convert to SpatRaster
  if(is(x,("RasterLayer"))){x<- terra::rast(x)}
  if(is(x,("SpatialPixelsDataFrame"))){x<- terra::rast(x)}

  # first we set up window and plot source raster
  graphics::par(mfrow=c(2,4), mar=c(0.2,0.2,1.5,0.2), oma=c(0.2,0.2,2,2))
  terra::plot(x, main = "Source", legend=FALSE, axes=FALSE)

  # next convert the SpatRaster to a dataframe for calculating the XDIST and YDIST
  d2<- data.frame(terra::crds(x))
  d2$xdist<- max(d2$x) - d2$x
  d2$ydist<- max(d2$y) - d2$y

  # calculate the XDIST, which is the X distance from every raster cell to xmax coordinate
  xgrid<- terra::subset(d2,select=c("x","y",'xdist'))
  xgrid<- terra::rast(xgrid, type="xyz")
  xgrid<- terra::resample(xgrid,x,method="near")
  terra::plot(xgrid, main='Dist XMax',legend=FALSE,axes=FALSE)
  terra::crs(xgrid)<- terra::crs(x)
  xgrid<- terra::mask(x = xgrid, mask = x)
  print("DISTANCE FROM XMAX COMPLETE")

  # calculate the YDIST, which is the Y distance from every raster cell to ymax coordinate
  ygrid<- terra::subset(d2,select=c("x","y",'ydist'))
  ygrid<- terra::rast(ygrid, type="xyz")
  ygrid<- terra::resample(ygrid,x,method="near")
  terra::plot(ygrid, main='Dist YMax',legend=FALSE,axes=FALSE)
  terra::crs(ygrid) <- terra::crs(x)
  ygrid<- terra::mask(x = ygrid, mask = x)
  print("DISTANCE FROM YMAX COMPLETE")

  # now we need to generate vectors representing the 4 corners and the center of the raster
  nw  <- cbind(min(d2$x),max(d2$y))
  sw  <- cbind(min(d2$x),min(d2$y))
  ne  <- cbind(max(d2$x),max(d2$y))
  se  <- cbind(max(d2$x),min(d2$y))
  mid<- cbind(round(max(d2$x) - ((max(d2$x) - min(d2$x))/2),0),round(max(d2$y) - ((max(d2$y) - min(d2$y))/2),0))

  # generate distance to NW corner grid
  NW<- terra::distance(x,terra::vect(nw,crs=terra::crs(x)))
  NW<- terra::mask(x = NW, mask = x)
  terra::crs(NW) <- terra::crs(x)
  terra::plot(NW, main='Dist NW',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM NW COMPLETE')

  # generate distance to SW corner grid
  SW<- terra::distance(x,terra::vect(sw,crs=terra::crs(x)))
  SW<- terra::mask(x = SW, mask = x)
  terra::crs(SW) <- terra::crs(x)
  terra::plot(SW, main='Dist SW',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM SW COMPLETE')

  # generate distance to NE corner grid
  NE<- terra::distance(x,terra::vect(ne,crs=terra::crs(x)))
  NE<- terra::mask(x = NE, mask = x)
  terra::crs(NE) <- terra::crs(x)
  terra::plot(NE, main='Dist from NE',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM NE COMPLETE')

  # generate distance to SE corner grid
  SE<- terra::distance(x,terra::vect(se,crs=terra::crs(x)))
  SE<- terra::mask(x = SE, mask = x)
  terra::crs(SE) <- terra::crs(x)
  terra::plot(SE, main='Dist SE',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM SE COMPLETE')

  # generate distance to CENTRE grid
  ctr<- terra::distance(x,terra::vect(mid,crs=terra::crs(x)))
  ctr<- terra::mask(x = ctr, mask = x)
  terra::crs(ctr) <- terra::crs(x)
  terra::plot(ctr, main='Dist MID',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM CENTRE COMPLETE')

  edf.stack<- terra::rast(c(xgrid,ygrid,NW,SW,NE,SE,ctr))
  names(edf.stack)<- c('distx','disty','distnw','distsw','distne','distse','distmid')
  return(edf.stack)
}
