#' Calculate Euclidean Distance Fields from a raster
#'
#' Calculate Euclidean distance fields for an input raster.
#' Calculate distance from each raster cell to NW, NE, SW,
#' SE, Y max, X max and middle of the raster. Outputs to a rasterstack.
#'
#' @param x Raster* object
#'
#' @return RasterStack* object
#' @export
#'
#' @examples
#' #Calculate Euclidean distance fields from a DEM raster
#' library(raster)
#' data(keene)
#' oss.edf(keene)
#'
#'
oss.edf<- function(x){

  # first we set up window and plot source raster
  graphics::par(mfrow=c(2,4), mar=c(0.2,0.2,1.5,0.2), oma=c(0.2,0.2,2,2))
  raster::plot(x, main = "Source", legend=FALSE, axes=FALSE)

  # next convert the raster to a dataframe for calculating the XDIST and YDIST
  d2<- methods::as(x,"SpatialPointsDataFrame")
  d2<- as.data.frame(d2@coords)
  d2$xdist<- x@extent@xmax - d2$x
  d2$ydist<- x@extent@ymax - d2$y

  # calculate the XDIST, which is the X distance from every raster cell to xmax coordinate
  xgrid<- subset(d2,select=c("x","y",'xdist'))
  sp::coordinates(xgrid)<- c("x","y")
  sp::gridded(xgrid)<-TRUE
  xgrid<- raster::raster(xgrid)
  xgrid<- raster::resample(xgrid,x,method="ngb")
  raster::plot(xgrid, main='Dist XMax',legend=FALSE,axes=FALSE)
  raster::projection(xgrid)<- raster::crs(x)
  xgrid<- raster::mask(x = xgrid, mask = x)
  print("DISTANCE FROM XMAX COMPLETE")

  # calculate the YDIST, which is the Y distance from every raster cell to ymax coordinate
  ygrid<- subset(d2,select=c("x","y",'ydist'))
  sp::coordinates(ygrid)<- c("x","y")
  sp::gridded(ygrid)<-TRUE
  ygrid<- raster::raster(ygrid)
  ygrid<- raster::resample(ygrid,x,method="ngb")
  raster::plot(ygrid, main='Dist YMax',legend=FALSE,axes=FALSE)
  raster::projection(ygrid) <- raster::crs(x)
  ygrid<- raster::mask(x = ygrid, mask = x)
  print("DISTANCE FROM YMAX COMPLETE")

  # now we need to generate vectors representing the 4 corners and the center of the raster
  nw  <- c(x@extent@xmin,x@extent@ymax)
  sw  <- c(x@extent@xmin,x@extent@ymin)
  ne  <- c(x@extent@xmax,x@extent@ymax)
  se  <- c(x@extent@xmax,x@extent@ymin)
  mid<- c(round(x@extent@xmax - ((x@extent@xmax - x@extent@xmin)/2),0),round(x@extent@ymax - ((x@extent@ymax - x@extent@ymin)/2),0))

  # generate distance to NW corner grid
  NW<- raster::distanceFromPoints(x,nw)
  NW<- raster::mask(x = NW, mask = x)
  raster::projection(NW) <- raster::crs(x)
  raster::plot(NW, main='Dist NW',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM NW COMPLETE')

  # generate distance to SW corner grid
  SW<- raster::distanceFromPoints(x,sw)
  SW<- raster::mask(x = SW, mask = x)
  raster::projection(SW) <- raster::crs(x)
  raster::plot(SW, main='Dist SW',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM SW COMPLETE')

  # generate distance to NE corner grid
  NE<- raster::distanceFromPoints(x,ne)
  NE<- raster::mask(x = NE, mask = x)
  raster::projection(NE) <- raster::crs(x)
  raster::plot(NE, main='Dist from NE',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM NE COMPLETE')

  # generate distance to SE corner grid
  SE<- raster::distanceFromPoints(x,se)
  SE<- raster::mask(x = SE, mask = x)
  raster::projection(SE) <- raster::crs(x)
  raster::plot(SE, main='Dist SE',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM SE COMPLETE')

  # generate distance to CENTRE grid
  ctr<- raster::distanceFromPoints(x,mid)
  ctr<- raster::mask(x = ctr, mask = x)
  raster::projection(ctr) <- raster::crs(x)
  raster::plot(ctr, main='Dist MID',legend=FALSE,axes=FALSE)
  print('DISTANCE FROM CENTRE COMPLETE')

  edf.stack<- raster::stack(xgrid,ygrid,NW,SW,NE,SE,ctr)
  names(edf.stack)<- c('distx','disty','distnw','distsw','distne','distse','distmid')
  return(edf.stack)
}
