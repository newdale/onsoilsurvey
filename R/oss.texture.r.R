#'
#' Use particle size analysis raster data to create a texture
#' class raster based on the the Canadian System of Soil Classification.
#' If sand fraction data is provided, modifiers (coarse, fine, very fine)
#' will be assigned to the sands, loamy sands and sandy loams.
#'
#' @param sand RasterLayer, RasterStack or RasterBrick of sand (0.5 - 2 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param silt RasterLayer, RasterStack or RasterBrick of silt (0.002 - 0.05 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param clay RasterLayer, RasterStack or RasterBrick of clay (<0.002 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param vcs RasterLayer, RasterStack or RasterBrick of very coarse sand (1 - 2 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param cs RasterLayer, RasterStack or RasterBrick of coarse sand (0.5 - 1 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param ms RasterLayer, RasterStack or RasterBrick of medium sand (0.25 - 0.50 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param fs RasterLayer, RasterStack or RasterBrick of fine sand (0.10 - 0.25 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param vfs RasterLayer, RasterStack or RasterBrick of very fine sand (0.05 - 0.10 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param triangle character, current choices are "CSSC" for Canadian System of Soil Classification (default), or "USDA" for United States Department of Agriculture
#'
#' @return When the input data is RasterLayer, returns a list of two objects:
#' texture_raster
#'    RasterLayer of soil texture class as per the Canadian System of Soil Classification
#'
#' legend
#'    data frame containing the factor levels for the soil texture class raster
#'
#' When the input data is a RasterStack or a RasterBrick, the function will return a list of lists.
#' Each outer list item will represent the list of outputs for each layer of the RasterStack or Rasterbrick.
#' The inner list will contain the 'texture_raster' and 'legend' objects as described above.
#'
#'@importFrom methods "as"
#'@importFrom raster "nlayers"
#'@importFrom raster "rasterFromXYZ"
#'@importFrom raster "raster"
#'@importFrom raster "ratify"
#'@importFrom raster "levels"
#'@importFrom sp "coordinates"
#'@importFrom sp "gridded"
#'@importFrom rasterVis "rasterTheme"
#'@importFrom rasterVis "levelplot"
#'@importFrom grDevices "colorRampPalette"
#'@importFrom RColorBrewer "brewer.pal"
#'
#' @export
#'
#' @examples
#' require(sp)
#' require(raster)
#' require(rasterVis)
#' require(RColorBrewer)
#'
#' # create sample data which includes all combinations of sand, silt and clay
#' dat<- data.frame(expand.grid(sand=seq(0,100,1), silt=seq(0,100,1), clay=seq(0,100,1)))
#' dat$sum<- dat$sand+dat$silt+dat$clay
#' dat<- dat[dat$sum==100,]
#' dat$x<- dat$sand
#' dat$y<- dat$clay
#'
#' coordinates(dat)<- ~x+y
#' gridded(dat)<- TRUE
#' sand<- raster(dat[1])
#' silt<- raster(dat[2])
#' clay<- raster(dat[3])
#'
#' # Create sand fraction data for testing
#' vcs<- clay/(clay+silt+sand+clay+clay)*100
#' cs<- silt/(clay+silt+sand+clay+clay)*100
#' ms<- sand/(clay+silt+sand+clay+clay)*100
#' fs<- clay/(clay+silt+sand+clay+clay)*100
#' vfs<- clay/(clay+silt+sand+clay+clay)*100
#'
#' # Create a texture class raster without sand fractions
#' tex<- oss.texture.r(sand,silt,clay)
#'
#' # And we can visualize using levelplot
#' texture.map<- ratify(tex[[1]])
#' rat<- data.frame(levels(texture.map))
#' rat[["Texture"]]<- tex[[2]]$Class[match(rat$ID,tex$legend$Code)]
#' levels(texture.map)<- rat
#' myTheme<- rasterTheme(region=(colorRampPalette(brewer.pal(12, "Set3"))(13)))
#' levelplot(texture.map, par.settings=myTheme)
#'
#' # Create a texture class raster with sand fractions
#' tex_fractions<- oss.texture.r(sand,silt,clay, vcs, cs, ms, fs, vfs)
#'
oss.texture.r<- function(sand, silt, clay, vcs=NULL, cs=NULL, ms=NULL, fs=NULL, vfs=NULL, triangle="CSSC"){

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
    xy<- as(xy,"SpatialPointsDataFrame")
    xy<- xy@coords

    # here we use mapply to convert to texture class. We use either with or without fractions, based on inputs
    if(is.null(s1)){
      z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, triangle=triangle)
    }else{
      z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, vcs=s1, cs=s2, ms=s3, fs=s4, vfs=s5, triangle=trianle)}

    z<- tex.legend$Code[match(z,tex.legend$TextureClass)]
    z.legend<- unique(z)
    z.legend<- z.legend[!is.na(z.legend)]
    z.legendclass<- tex.legend$TextureClass[match(z.legend,tex.legend$Code)]
    rat<- data.frame(Code=z.legend, Class=z.legendclass)
    rm(z.legend,z.legendclass)

    xyz<- cbind(xy,z)
    tex<- rasterFromXYZ(xyz)

    texout <- list("texture_raster"=tex, "legend"=rat)

    # if the objects provided to the function are RasterStacks or Bricks, we need to create a for-loop to iterate
  }else if(class(sand)[1]=="RasterStack" & class(silt)[1]=="RasterStack" & class(clay)[1]=="RasterStack" |
           class(sand)[1]=="RasterBrick" & class(silt)[1]=="RasterBrick" & class(clay)[1]=="RasteBrick"){

    #create an empty list outside the loop to store the outputs
    texout<- list()

    for (i in 1:nlayers(sand)){

      #convert the raster layers to vector
      s<- as.vector(round(sand[[i]],0))
      si<- as.vector(round(silt[[i]],0))
      c<- as.vector(round(clay[[i]],0))

      #we can add the sand fractions here only if provided to the function, else we set them as NULL
      if(!is.null(vcs)|!is.null(cs)|!is.null(ms)|!is.null(fs)|!is.null(vfs)){
        s1<- as.vector(round(vcs,0))
        s2<- as.vector(round(cs,0))
        s3<- as.vector(round(ms,0))
        s4<- as.vector(round(fs,0))
        s5<- as.vector(round(vfs,0))
      } else {s1<- s2<- s3<- s4<- s5<- NULL}

      # generate matrix of xy coordinates for the raster layers
      xy<- sand[[i]]
      xy[is.na(xy)] <- 0
      xy<- as(xy,"SpatialPointsDataFrame")
      xy<- xy@coords

      # here we use mapply to convert to texture class. We use either with or without fractions, based on inputs
      if(is.null(s1)){
        z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, triangle=triangle)
      }else{
        z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, vcs=s1, cs=s2, ms=s3, fs=s4, vfs=s5, triangle=triangle)}

      z<- tex.legend$Code[match(z,tex.legend$TextureClass)]
      z.legend<- unique(z)
      z.legend<- z.legend[!is.na(z.legend)]
      z.legendclass<- tex.legend$TextureClass[match(z.legend,tex.legend$Code)]
      rat<- data.frame(Code=z.legend, Class=z.legendclass)
      rm(z.legend,z.legendclass)

      xyz<- cbind(xy,z)
      tex<- rasterFromXYZ(xyz)

      temp <- list("texture_raster"=tex, "legend"=rat)
      texout[[i]]<- temp
    }

    return(texout)

    # last option is that the objects provided were not RasterLayer or RasterStack or Bricks
  }else{print("Input data must be RasterLayer or RasteStack or RasterBrick, please check your input objects")
  }
}
