#'
#' Use particle size analysis raster data to create a texture
#' class raster based on the the Canadian System of Soil Classification.
#' If sand fraction data is provided, modifiers (coarse, fine, very fine)
#' will be assigned to the sands, loamy sands and sandy loams.
#'
#' @param sand RasterLayer, RasterStack or RasterBrick
#' @param silt RasterLayer, RasterStack or RasterBrick
#' @param clay RasterLayer, RasterStack or RasterBrick
#' @param vcs RasterLayer, RasterStack or RasterBrick
#' @param cs RasterLayer, RasterStack or RasterBrick
#' @param ms RasterLayer, RasterStack or RasterBrick
#' @param fs RasterLayer, RasterStack or RasterBrick
#' @param vfs RasterLayer, RasterStack or RasterBrick
#'
#' @return list
#' @export
#'
#' @examples
#' #create sample data which includes all combinations of sand, silt and clay
#' dat<- data.frame(expand.grid(sand=seq(0,100,1), silt=seq(0,100,1), clay=seq(0,100,1)))
#' dat$sum<- dat$sand+dat$silt+dat$clay
#' dat<- dat[dat$sum==100,]
#'
#' # assign these values to rasters
#' sand<- raster::raster(nrows=51, ncols=101, vals=dat$sand)
#' silt<- raster::raster(nrows=51, ncols=101, vals=dat$silt)
#' clay<- raster::raster(nrows=51, ncols=101, vals=dat$clay)
#'
#' #Create sand fraction data for testing
#' vcs<- clay/(clay+silt+sand+clay+clay)*100
#' cs<- silt/(clay+silt+sand+clay+clay)*100
#' ms<- sand/(clay+silt+sand+clay+clay)*100
#' fs<- clay/(clay+silt+sand+clay+clay)*100
#' vfs<- clay/(clay+silt+sand+clay+clay)*100
#'
#' #Create a texture class raster without sand fractions
#' tex<- oss.texture.r(sand,silt,clay)
#'
#' #Create a texture class raster without sand fractions
#' tex<- oss.texture.r(sand,silt,clay, vcs, cs, ms, fs, vfs)
#'
#'
oss.texture.r<- function(sand, silt, clay, vcs=NULL, cs=NULL, ms=NULL, fs=NULL, vfs=NULL){

  #create the standardized legend
  tex.legend<- data.frame(TextureClass=c("coarse sand","sand", "fine sand", "very fine sand",
                                         "loamy coarse sand", "loamy sand", "loamy fine sand", "loamy very fine sand",
                                         "coarse sandy loam", "sandy loam", "fine sandy loam", "very fine sandy loam",
                                         "loam", "silt loam", "silt","sandy clay loam", "clay loam", "silty clay loam",
                                         "sandy clay", "silty clay", "clay", "heavy clay"),
                          Code=c(seq(1,22,1)))

  #determine the class of the objects
  if(class(sand)[1]=="RasterLayer" & class(silt)[1]=="RasterLayer" & class(clay)[1]=="RasterLayer"){

    #convert the raster layers to vector
    s<- as.vector(round(sand,0))
    si<- as.vector(round(silt,0))
    c<- as.vector(round(clay,0))

    #we can add the sand fractions here only if provided to the function, else we set them as NULL
    if(!is.null(vcs)|!is.null(cs)|!is.null(ms)|!is.null(fs)|!is.null(vfs)){
      s1<- as.vector(round(vcs,0))
      s2<- as.vector(round(cs,0))
      s3<- as.vector(round(ms,0))
      s4<- as.vector(round(fs,0))
      s5<- as.vector(round(vfs,0))
    } else {s1<- s2<- s3<- s4<- s5<- NULL}

    # generate matrix of xy coordinates for the raster layers
    # we need to convert all NAs to a number (0 in this case) so we get all the coordinates
    # since when we convert to SpatialPointsDataFrame it drops all the NA cells
    xy<- sand
    xy[is.na(xy)] <- 0
    xy<- methods::as(xy,"SpatialPointsDataFrame")
    xy<- xy@coords

    # here we use mapply to convert to texture class. We use either with or without fractions, based on inputs
    if(is.null(s1)){
      z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c)
    }else{
      z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, vcs=s1, cs=s2, ms=s3, fs=s4, vfs=s5)}

    z<- tex.legend$Code[match(z,tex.legend$TextureClass)]
    z.legend<- unique(z)
    z.legend<- z.legend[!is.na(z.legend)]
    z.legendclass<- tex.legend$TextureClass[match(z.legend,tex.legend$Code)]
    rat<- data.frame(Code=z.legend, Class=z.legendclass)
    rm(z.legend,z.legendclass)

    xyz<- cbind(xy,z)
    tex<- raster::rasterFromXYZ(xyz)

    texout <- list("texture_raster"=tex, "legend"=rat)

    # if the objects provided to the function are RasterStacks or Bricks, we need to create a for-loop to iterate
  }else if(class(sand)[1]=="RasterStack" & class(silt)[1]=="RasterStack" & class(clay)[1]=="RasterStack" |
           class(sand)[1]=="RasterBrick" & class(silt)[1]=="RasterBrick" & class(clay)[1]=="RasteBrick"){

    #create an empty list outside the loop to store the outputs
    texout<- list()

    for (i in 1:raster::nlayers(sand)){

      #convert the raster layers to vector
      s<- as.vector(round(sand[[i]],0))
      si<- as.vector(round(silt[[i]],0))
      c<- as.vector(round(clay[[i]],0))

      #we can add the sand fractions here only if provided to the function, else we set them as NULL
      if(!is.null(s1)|!is.null(cs)|!is.null(ms)|!is.null(fs)|!is.null(vfs)){
        s1<- as.vector(round(vcs,0))
        s2<- as.vector(round(cs,0))
        s3<- as.vector(round(ms,0))
        s4<- as.vector(round(fs,0))
        s5<- as.vector(round(vfs,0))
      } else {s1<- s2<- s3<- s4<- s5<- NULL}

      # generate matrix of xy coordinates for the raster layers
      xy<- sand[[i]]
      xy[is.na(xy)] <- 0
      xy<- methods::as(xy,"SpatialPointsDataFrame")
      xy<- xy@coords

      # here we use mapply to convert to texture class. We use either with or without fractions, based on inputs
      if(is.null(s1)){
        z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c)
      }else{
        z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, vcs=s1, cs=s2, ms=s3, fs=s4, vfs=s5)}

      z<- tex.legend$Code[match(z,tex.legend$TextureClass)]
      z.legend<- unique(z)
      z.legend<- z.legend[!is.na(z.legend)]
      z.legendclass<- tex.legend$TextureClass[match(z.legend,tex.legend$Code)]
      rat<- data.frame(Code=z.legend, Class=z.legendclass)
      rm(z.legend,z.legendclass)

      xyz<- cbind(xy,z)
      tex<- raster::rasterFromXYZ(xyz)

      temp <- list("texture_raster"=tex, "legend"=rat)
      texout[[i]]<- temp
    }

    return(texout)

    # last option is that the objects provided were not RasterLayer or RasterStack or Bricks
  }else{print("Input data must be RasterLayer or RasteStack or RasterBrick, please check your input objects")
  }
}
