#'
#' Use particle size analysis SpatRaster data to create a texture
#' class SpatRaster based on the the Canadian System of Soil Classification or USDA.
#' If sand fraction data is provided, modifiers (coarse, fine, very fine)
#' will be assigned to the sands, loamy sands and sandy loams.
#'
#' @param sand RasterLayer, or SpatRaster of sand (0.5 - 2 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param silt RasterLayer, or SpatRaster of silt (0.002 - 0.05 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param clay RasterLayer, or SpatRaster of clay (<0.002 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param vcs RasterLayer, or SpatRaster of very coarse sand (1 - 2 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param cs RasterLayer, or SpatRaster of coarse sand (0.5 - 1 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param ms RasterLayer, or SpatRaster of medium sand (0.25 - 0.50 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param fs RasterLayer, or SpatRaster of fine sand (0.10 - 0.25 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param vfs RasterLayer, or SpatRaster of very fine sand (0.05 - 0.10 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param tri character, current choices are "CSSC" for Canadian System of Soil Classification (default), or "USDA" for United States Department of Agriculture
#'
#' @return When the input data is SpatRaster with only one layer, returns a list of two objects:
#' texture_raster
#'    SpatRaster of soil texture class as per the selected soil classification system
#'
#' legend
#'    data frame containing the factor levels for the soil texture class raster
#'
#' When the input data is a SpatRaster with more than one layer (i.e., stack), the function will return a list of lists.
#' Each outer list item will represent the list of outputs for each layer of the SpatRaster.
#' The inner list will contain the 'texture_raster' and 'legend' objects as described above.
#'
#'@importFrom RColorBrewer "brewer.pal"
#'
#' @export
#'
#' @examples
#' require(sp)
#' require(terra)
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
#' sand<- terra::rast(dat[1])
#' silt<- terra::rast(dat[2])
#' clay<- terra::rast(dat[3])
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
#' # And we can visualize
#' texture.map<- as.factor(tex[[1]])
#' rat<- data.frame(levels(texture.map))
#' rat[["Texture"]]<- tex[[2]]$Class[match(rat$ID,tex$legend$Code)]
#' rat<- rat[,c(1,3)]
#' levels(texture.map)<- rat
#' coltb<- data.frame(value=rat$ID, col=colorRampPalette(brewer.pal(12, "Set3"))(nrow(rat)))
#' coltab(texture.map)<- coltb
#' plot(texture.map)
#'
#' # Create a texture class raster with sand fractions
#' # Create a texture class raster with sand fractions

#' # generate random values for sand, silt and clay, normalize to sum 100, assign to SpatRaster
#' sand<- sample(seq(0,100,1),10000,replace=TRUE)
#' silt<- sample(seq(0,100,1),10000,replace=TRUE)
#' clay<- sample(seq(0,100,1),10000,replace=TRUE)

#' vals<- data.frame(sand=(sand/(sand+silt+clay))*100,
#'                   silt=(silt/(sand+silt+clay))*100,
#'                   clay=(clay/(sand+silt+clay))*100)
#'
#' sand<- rast(ncol=100, nrow=100, vals=vals$sand, xmin=0, xmax=100, ymin=0, ymax=100)
#' silt<- rast(ncol=100, nrow=100, vals=vals$silt, xmin=0, xmax=100, ymin=0, ymax=100)
#' clay<- rast(ncol=100, nrow=100, vals=vals$clay, xmin=0, xmax=100, ymin=0, ymax=100)

#' # generate random values for sand fractions, normalize to sum 100, assing to SpatRaster
#' vfs<- sample(seq(0,100,1),10000,replace=TRUE)
#' fs<- sample(seq(0,100,1),10000,replace=TRUE)
#' ms<- sample(seq(0,100,1),10000,replace=TRUE)
#' cs<- sample(seq(0,100,1),10000,replace=TRUE)
#' vcs<- sample(seq(0,100,1),10000,replace=TRUE)

#' vals<- data.frame(vfs=(vfs/(vfs+fs+ms+cs+vcs))*100,
#'                   fs=(fs/(vfs+fs+ms+cs+vcs))*100,
#'                   ms=(ms/(vfs+fs+ms+cs+vcs))*100,
#'                   cs=(cs/(vfs+fs+ms+cs+vcs))*100,
#'                   vcs=(vcs/(vfs+fs+ms+cs+vcs))*100)

#' vfs<- rast(ncol=100, nrow=100, vals=vals$vfs, xmin=0, xmax=100, ymin=0, ymax=100)
#' fs<- rast(ncol=100, nrow=100, vals=vals$fs, xmin=0, xmax=100, ymin=0, ymax=100)
#' ms<- rast(ncol=100, nrow=100, vals=vals$ms, xmin=0, xmax=100, ymin=0, ymax=100)
#' cs<- rast(ncol=100, nrow=100, vals=vals$cs, xmin=0, xmax=100, ymin=0, ymax=100)
#' vcs<- rast(ncol=100, nrow=100, vals=vals$vcs, xmin=0, xmax=100, ymin=0, ymax=100)

#' tex_fractions<- oss.texture.r(sand,silt,clay, vcs, cs, ms, fs, vfs)
#' texture.map<- as.factor(tex_fractions[[1]])
#' rat<- data.frame(levels(texture.map))
#' rat[["Texture"]]<- tex_fractions[[2]]$Class[match(rat$ID,tex_fractions$legend$Code)]
#' rat<- rat[,c(1,3)]
#' levels(texture.map)<- rat
#' coltb<- data.frame(value=rat$ID, col=colorRampPalette(brewer.pal(12, "Set3"))(nrow(rat)))
#' coltab(texture.map)<- coltb
#' plot(texture.map)
#'
#'
oss.texture.r<- function(sand, silt, clay, vcs=NULL, cs=NULL, ms=NULL, fs=NULL, vfs=NULL, tri="CSSC"){

  #set the triangle
  tri<- tri

  #create the standardized legend
  tex.legend<- data.frame(TextureClass=c("coarse sand","sand", "fine sand", "very fine sand",
                                         "loamy coarse sand", "loamy sand", "loamy fine sand", "loamy very fine sand",
                                         "coarse sandy loam", "sandy loam", "fine sandy loam", "very fine sandy loam",
                                         "loam", "silt loam", "silt","sandy clay loam", "clay loam", "silty clay loam",
                                         "sandy clay", "silty clay", "clay", "heavy clay"),
                          Code=c(seq(1,22,1)))

  #determine the class of the objects
  if(class(sand)[1]=="SpatRaster" & class(silt)[1]=="SpatRaster" & class(clay)[1]=="SpatRaster"){

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
    xy<- terra::subst(xy,NA,0)
    xy<- terra::crds(xy)

    # here we use mapply to convert to texture class. We use either with or without fractions, based on inputs
    if(is.null(s1)){
      z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, tri=tri)
    }else{
      z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, vcs=s1, cs=s2, ms=s3, fs=s4, vfs=s5, tri=tri)}

    z<- tex.legend$Code[match(z,tex.legend$TextureClass)]
    z.legend<- unique(z)
    z.legend<- z.legend[!is.na(z.legend)]
    z.legendclass<- tex.legend$TextureClass[match(z.legend,tex.legend$Code)]
    rat<- data.frame(Code=z.legend, Class=z.legendclass)
    rm(z.legend,z.legendclass)

    xyz<- data.frame(cbind(xy,z))
    tex<- rast(xyz)

    texout <- list("texture_raster"=tex, "legend"=rat)

    # if the objects provided to the function are RasterStacks or Bricks, we need to create a for-loop to iterate
  }else if(class(sand)[1]=="SpatRaster" & class(silt)[1]=="SpatRaster" & class(clay)[1]=="SpatRaster"){

    #create an empty list outside the loop to store the outputs
    texout<- list()

    for (i in 1:nlyr(sand)){

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
      xy<- terra::subst(xy,NA,0)
      xy<- terra::crds(xy)

      # here we use mapply to convert to texture class. We use either with or without fractions, based on inputs
      if(is.null(s1)){
        z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, tri=tri)
      }else{
        z<- mapply(onsoilsurvey::oss.texture,sand=s, silt=si, clay=c, vcs=s1, cs=s2, ms=s3, fs=s4, vfs=s5, tri=tri)}

      z<- tex.legend$Code[match(z,tex.legend$TextureClass)]
      z.legend<- unique(z)
      z.legend<- z.legend[!is.na(z.legend)]
      z.legendclass<- tex.legend$TextureClass[match(z.legend,tex.legend$Code)]
      rat<- data.frame(Code=z.legend, Class=z.legendclass)
      rm(z.legend,z.legendclass)

      xyz<- data.frame(cbind(xy,z))
      tex<- rast(xyz)

      temp <- list("texture_raster"=tex, "legend"=rat)
      texout[[i]]<- temp
    }

    return(texout)

    # last option is that the objects provided were not RasterLayer or SpatRaster
  }else{print("Input data must be SpatRaster, please check your input objects")
  }
}
