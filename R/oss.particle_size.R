#'
#' Use particle size analysis data to determine the particle size
#' class in the Canadian System of Soil Classification or USDA
#' If sand separates data is provided, these are considered in
#' the classification. Sand, silt and clay must be provided in percentage
#' and should sum to 100. Sand separates should sum to total sand.
#'
#' @param sand numeric, sand (0.5 - 2 mm) content in percentage
#' @param silt numeric, silt (0.002 - 0.05 mm) content in percentage
#' @param clay numeric, clay (<0.002 mm) content in percentage
#' @param vcs numeric, very coarse sand (1 - 2 mm) content expressed as portion of total sand
#' @param cs numeric, coarse sand (0.5 - 1 mm) content expressed as portion of total sand
#' @param ms numeric, medium sand (0.25 - 0.50 mm) content expressed as portion of total sand
#' @param fs numeric, fine sand (0.10 - 0.25 mm) content expressed as portion of total sand
#' @param vfs numeric, very fine sand (0.05 - 0.10 mm) content expressed as portion of total sand
#' @param tri character, current choices are "CSSC" for Canadian System of Soil Classification (default), or "USDA" for United States Department of Agriculture
#'
#' @return character
#' @export
#'
#' @examples
#' #Determine particle size class for a single observation without sand fractions
#' oss.particle_size(sand=67, silt=23, clay=10)
#'
#' #Determine particle size class for a single observation with sand fractions
#' oss.particle_size(sand=67, silt=23, clay=10, vcs=60, cs=10, ms=10, fs=10, vfs=10)
#'
#' #Determine particle size class for multiple observations without sand fractions
#' dat<- data.frame(sand=c(20,40,80), silt= c(15,30,10), clay= c(65,30,10),
#' vcs=c(10,10,10), cs=c(5,5,5), ms=c(5,5,5), fs=c(60,60,60), vfs=c(20,20,20))
#' mapply(oss.particle_size,sand=dat$sand, silt=dat$silt, clay=dat$clay,
#' vcs=dat$vcs, cs=dat$cs, ms=dat$ms, fs=dat$fs, vfs=dat$vfs)
#'
#' #Determine particle size class for multiple observations without sand fractions
#' dat<- data.frame(sand=c(20,40,80), silt= c(15,30,10), clay= c(65,30,10))
#' mapply(oss.texture,sand=dat$sand, silt=dat$silt, clay=dat$clay)
#'
#' #or return it as a new column in the data frame
#' dat$class<- mapply(oss.particle_size,sand=dat$sand, silt=dat$silt, clay=dat$clay)
#'

oss.particle_size<- function(sand, silt, clay, vcs=NULL, cs=NULL, ms=NULL, fs=NULL, vfs=NULL, tri="CSSC"){

  clay<-clay; silt<-silt; sand<-sand; vcs<-vcs; cs<-cs; ms<-ms; fs<-fs; vfs<-vfs; tri<-tri

  # start by looking at the sums of the sand, silt and clay
  psa<- sand + silt + clay

  # if psa=NA, we want the function to skip to the end and return NA, so we wrap the entire thing in an if statement
  if(is.na(psa)){ps.class<- NA

  }else{

    # next we will ensure we normalize to 100, and print an error statement if values deviate too greatly
    if (psa>=105 | psa<= 95) warning('Sum of sand, silt and clay deviates from 100 by more than 5. Check your input data. Values will be normalized to sum 100')

    # and now we will normalize to 100 for the sand, silt and clay
    # create copies for the calculation
    clay.t<- clay; silt.t<- silt; sand.t<- sand

    # and now we normalize to 100
    clay<- clay.t/(clay.t+silt.t+sand.t)*100
    silt<- silt.t/(clay.t+silt.t+sand.t)*100
    sand<- sand.t/(clay.t+silt.t+sand.t)*100
    rm(clay.t, silt.t, sand.t)

    # next we look at the sums of the sand fractions, but only if we need to
    # if they are NULL, we do the simple texture and particle size

    if(is.null(vcs)|is.null(cs)|is.null(ms)|is.null(fs)|is.null(vfs)){

      # and here is the start of the main section where texture class is determined
      if(is.na(clay) | is.na(silt) | is.na(sand)){tclass <- NA

      }else if(silt + 1.5 * clay < 15){tclass<- 'sand'
      }else if(silt + 1.5 * clay >= 15 & silt + 2 * clay < 30) {tclass<- 'loamy sand'
      }else if((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | (clay < 7 & silt < 50 & silt + 2 * clay >= 30)) {tclass<- 'sandy loam'
      }else if(clay >= 7 & clay < 27 & silt >= 28 & silt < 50 & sand <= 52) {tclass<- "loam"
      }else if((silt >= 50 & clay >= 12 & clay < 27) | (silt >= 50 & silt < 80 & clay < 12)) {tclass<- 'silt loam'
      }else if(silt >= 80 & clay < 12) {tclass<- 'silt'
      }else if(clay >= 20 & clay < 35 & silt < 28 & sand > 45) {tclass<- 'sandy clay loam'
      }else if(clay >= 27 & clay < 40 & sand <= 45 & sand > 20) {tclass<- 'clay loam'
      }else if(clay < 40 & clay >= 27 & sand <= 20) {tclass<- 'silty clay loam'
      }else if(clay >= 35 & clay <= 60 & sand > 45 ) {tclass<- 'sandy clay'
      }else if(clay >= 40 & clay <= 60 & silt >= 40) {tclass<- 'silty clay'
      }else if(clay >= 40 & clay <= 60 & silt < 40 & sand <= 45) {tclass<- 'clay'
      }else if(clay >60 & tri=="USDA"){tclass <- 'clay'
      }else if(clay >60 & tri=="CSSC"){tclass<- 'heavy clay'
      }else{tclass<- 'texture class error'}

      # and now we add the particle size when no sand fractions are provided
      if(tclass=="coarse sand"|tclass=="sand"|tclass=="fine sand"|tclass=="loamy coarse sand"|tclass=="loamy sand"|tclass=="loamy fine sand"){ps.class<- 'sandy'
      } else if(tclass=="loamy very fine sand" | tclass=="very fine sand") {ps.class<- 'coarse loamy'
      } else if(sand>= 15 & clay<18){ps.class<-'coarse loamy'
      } else if(sand>=15 & clay>=18 & clay<35) {ps.class<- 'fine loamy'
      } else if(sand<15 & clay<18) {ps.class<- 'coarse silty'
      } else if(sand<15 & clay>=18 & clay<35) {ps.class<- 'fine silty'
      } else if (clay>=35 & clay<=60) {ps.class<- "fine clayey"
      } else if (clay>60) {ps.class<- "very fine clayey"
      } else {ps.class<- "particle size class error"}


    }else{

      # and here we do the same but we consider the sand separates this time

      # we need to create a warning if things are not adding up
      sfrac<- vcs+cs+ms+fs+vfs

      # we check to see if sand fractions sum closer to sand content (reported as proportional of total sand)
      # and we check to see if the sum deviates by more than 2 from total sand and issue warning if it does
      if (sfrac>sand+(sand*0.05) | sfrac< sand-(sand*0.05)) warning('Sum of sand separates deviates by more than 5% from total sand. Check your input data. Values will be normalized to total sand')

      # we need to maintain sand fraction data as percentage of total sand (sum=100) for texture class assignment
      # we also need it as proportion of total sand (sum = sand) for particle size class assignment

      # normalize to total sand. This is in case the sum is slightly off.
      # create copies for the calculation
      vcs.t<- vcs; cs.t<- cs; ms.t<- ms; fs.t<- fs; vfs.t<- vfs

      # we start by normalizing to total sand so the numbers add up to correct rounding errors
      if(vcs==0){vcs.p<-0}else{vcs.p<- vcs.t*sand/(vcs.t+cs.t+ms.t+fs.t+vfs.t)}
      if(cs==0){cs.p<-0}else{cs.p<- cs.t*sand/(vcs.t+cs.t+ms.t+fs.t+vfs.t)}
      if(ms==0){ms.p<-0}else{ms.p<- ms.t*sand/(vcs.t+cs.t+ms.t+fs.t+vfs.t)}
      if(fs==0){fs.p<-0}else{fs.p<- fs.t*sand/(vcs.t+cs.t+ms.t+fs.t+vfs.t)}
      if(vfs==0){vfs.p<-0}else{vfs.p<- vfs.t*sand/(vcs.t+cs.t+ms.t+fs.t+vfs.t)}

      # and then we use the above to compute these as percentage
      if(vcs.p==0){vcs=0}else{vcs<- (vcs.p/sand)*100}
      if(cs.p==0){cs=0}else{cs<- (cs.p/sand)*100}
      if(ms.p==0){ms=0}else{ms<- (ms.p/sand)*100}
      if(fs.p==0){fs=0}else{fs<- (fs.p/sand)*100}
      if(vfs.p==0){vfs=0}else{vfs<- (vfs.p/sand)*100}

      rm(vcs.t, cs.t, ms.t, fs.t, vfs.t)

      #############################################################################################################
      # and here is the start of the main section where texture class is determined with sand separates
      if(is.na(clay) | is.na(silt) | is.na(sand)){tclass <- NA

      # here we do the sands
      } else if(silt + 1.5 * clay < 15){
        if(vcs + cs >=25 & ms<50 & fs<50 & vfs<50){tclass<- 'coarse sand'
        } else if (vcs + cs + ms >=25 & vcs + cs <25 & fs<50 & vfs<50 | vcs + cs >=25 & ms>=50){tclass<- 'sand'
        } else if (fs>=50 | vcs+cs+ms<25 & vfs<50){tclass<- 'fine sand'
        } else if (vfs>=50){ tclass<- 'very fine sand'
        } else {tclass<- 'sands error'}

        # here we do the loamy sands
      }else if(silt + 1.5 * clay >= 15 & silt + 2 * clay < 30) {
        if(vcs + cs >=25 & ms<50 & fs<50 & vfs<50){tclass<- 'loamy coarse sand'
        } else if (vcs + cs + ms >=25 & vcs + cs <25 & fs<50 & vfs<50 | vcs + cs >=25 & ms>=50){tclass<- 'loamy sand'
        } else if (fs>=50 | vfs<50 & vcs + cs + ms <25){tclass<- 'loamy fine sand'
        } else if (vfs>=50){tclass<- 'loamy very fine sand'
        } else {tclass<- "loamy sands error"}

        # here we do the sandy loams
      }else if((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | (clay < 7 & silt < 50 & silt + 2 * clay >= 30)) {
        if(vcs + cs >= 25 & ms<50 & fs<50 & vfs<50){tclass<- 'coarse sandy loam'
        } else if (vcs + cs + ms >=30 & fs<50 & vfs<50){tclass<- 'sandy loam'
        } else if (fs>=30 & vfs<30 | vcs+cs+ms>=15 & vcs+cs+ms < 30 | fs + vfs >40 & fs>=20 & vcs + cs + ms <15 ){tclass<- 'fine sandy loam'
        } else if (vfs>=30 | vfs + fs >40 & vfs>=20 & vcs + cs + ms <15){tclass<- 'very fine sandy loam'
        } else {tclass<- 'sandy loams error'}

        # and now the sand fractions no longer matter
      } else if(clay >= 7 & clay < 27 & silt >= 28 & silt < 50 & sand <= 52) {tclass<- "loam"
      } else if((silt >= 50 & clay >= 12 & clay < 27) | (silt >= 50 & silt < 80 & clay < 12)) {tclass<- 'silt loam'
      } else if(silt >= 80 & clay < 12) {tclass<- 'silt'
      } else if(clay >= 20 & clay < 35 & silt < 28 & sand > 45) {tclass<- 'sandy clay loam'
      } else if(clay >= 27 & clay < 40 & sand <= 45 & sand > 20) {tclass<- 'clay loam'
      } else if(clay < 40 & clay >= 27 & sand <= 20) {tclass<- 'silty clay loam'
      } else if(clay >= 35 & clay <= 60 & sand > 45 ) {tclass<- 'sandy clay'
      } else if(clay >= 40 & clay <= 60 & silt >= 40) {tclass<- 'silty clay'
      } else if(clay >= 40 & clay <= 60 & silt < 40 & sand <= 45) {tclass<- 'clay'
      } else if(clay >60 & tri=="USDA"){tclass <- 'clay'
      } else if(clay >60 & tri=="CSSC"){tclass<- 'heavy clay'
      } else {tclass<- 'texture class error'}

      # and now for particle size when we have sand separates

      # here we assign the sandy class
      if(tclass=="coarse sand"|tclass=="sand"|tclass=="fine sand"|tclass=="loamy coarse sand"|tclass=="loamy sand"|tclass=="loamy fine sand"){ps.class<- 'sandy'

      # here we assign the loamy classes
      } else if(tclass=="loamy very fine sand" | tclass=="very fine sand" | clay<35) {
        if(fs.p+ms.p+cs.p+vcs.p>=15 & clay<18) {ps.class<- 'coarse loamy'
        } else if (fs.p+ms.p+cs.p+vcs.p>=15 & clay>=18 & clay<35) {ps.class<- 'fine loamy'
        } else if (fs.p+ms.p+cs.p+vcs.p<15 & clay<18) {ps.class<- 'coarse silty'
        } else if (fs.p+ms.p+cs.p+vcs.p<15 & clay>=18 & clay<35) {ps.class<- 'fine silty'
        } else {ps.class<- 'loamy error'}

        # here we assign the clayey classes
      } else if (clay>=35 & clay<=60) {ps.class<- "fine clayey"
      } else if (clay>60) {ps.class<- "very fine clayey"
      } else {ps.class<- "particle size class error"}
    }
  }
  return(ps.class)
}
