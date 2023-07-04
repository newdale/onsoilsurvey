#'
#' Use particle size analysis data to determine the texture
#' class in the Canadian System of Soil Classification or USDA
#' If sand fraction data is provided, modifiers (coarse, fine, very fine)
#' will be assigned to the sands, loamy sands and sandy loams.
#'
#' @param sand numeric, sand (0.5 - 2 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param silt numeric, silt (0.002 - 0.05 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param clay numeric, clay (<0.002 mm) content either in decimal or percentage (e.g. 0.25 or 25)
#' @param vcs numeric, very coarse sand (1 - 2 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param cs numeric, coarse sand (0.5 - 1 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param ms numeric, medium sand (0.25 - 0.50 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param fs numeric, fine sand (0.10 - 0.25 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param vfs numeric, very fine sand (0.05 - 0.10 mm) content expressed as percentage of the sum of the sand fractions or portion of total sand
#' @param tri character, current choices are "CSSC" for Canadian System of Soil Classification (default), or "USDA" for United States Department of Agriculture
#'
#' @return character
#' @export
#'
#' @examples
#' #Determine texture class for a single observation without sand fractions
#' oss.texture(sand=67, silt=23, clay=10)
#'
#' #Determine texture class for a single observation with sand fractions
#' oss.texture(sand=67, silt=23, clay=10, vcs=60, cs=10, ms=10, fs=10, vfs=10)
#'
#' #Determine texture class for multiple observations without sand fractions
#' dat<- data.frame(sand=c(20,40,80), silt= c(15,30,10), clay= c(65,30,10),
#' vcs=c(10,10,10), cs=c(5,5,5), ms=c(5,5,5), fs=c(60,60,60), vfs=c(20,20,20))
#' mapply(oss.texture,sand=dat$sand, silt=dat$silt, clay=dat$clay,
#' vcs=dat$vcs, cs=dat$cs, ms=dat$ms, fs=dat$fs, vfs=dat$vfs)
#'
#' #Determine texture class for multiple observations without sand fractions
#' dat<- data.frame(sand=c(20,40,80), silt= c(15,30,10), clay= c(65,30,10))
#' mapply(oss.texture,sand=dat$sand, silt=dat$silt, clay=dat$clay)
#'
#' #or return it as a new column in the data frame
#' dat$class<- mapply(oss.texture,sand=dat$sand, silt=dat$silt, clay=dat$clay)
#'
oss.texture<- function(sand, silt, clay, vcs=NULL, cs=NULL, ms=NULL, fs=NULL, vfs=NULL, tri="CSSC"){

  clay<- clay
  silt<- silt
  sand<- sand
  vcs<- vcs
  cs<- cs
  ms<- ms
  fs<- fs
  vfs<- vfs
  tri<- tri

  # start by looking at the sums of the sand, silt and clay
  psa<- sand + silt + clay

  # if psa=NA, we want the function to skip to the end and return NA, so we wrap the entire thing in an if statement
  if(!is.na(psa)){

    # if the sum is closest to 1, meaning user has decimals, convert to percentages
    if(which.min(abs(c(1,100)-psa))==1){
      clay<- clay*100
      silt<- silt*100
      sand<- sand*100
    }

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

    if(!is.null(vcs)|!is.null(cs)|!is.null(ms)|!is.null(fs)|!is.null(vfs)){

      # we need to create a warning if things are not adding up
      sfrac<- vcs+cs+ms+fs+vfs
      sfrac<- mean(sfrac, na.rm=TRUE)

      # we check to see if sand fractions sum closer to sand content
      # and we check to see if the sum deviates by more than 2 from total sand and issue warning if it does
      if(which.min(abs(c(sand,100)-sfrac))==1){
        if(sfrac>=(sand+5) | sfrac<= (sand-5)) warning('Sum of sand fractions deviates by more than 5 from total sand. Check your input data. Values will be normalized to sum 100')
      }

      # we check to see if sand fractions sum closer to 100% (reported as percent of total sand)
      # and if closer to 100, we check to see if the sum deviates by more than 2 from 100
      if(which.min(abs(c(sand,100)-sfrac))==2){
        if(sfrac>=(105) | sfrac<= (95)) warning('Sum of sand fractions deviates by more than 5 from 100. Check your input data. Values will be normalized to sum 100')
      }

      # and now we will normalize sand fractions in the case they are a proportion of total sand
      # create copies for the calculation
      vcs.t<- vcs; cs.t<- cs; ms.t<- ms; fs.t<- fs; vfs.t<- vfs
      if(which.min(c(sand,100)-sfrac)==1){
        vcs<- vcs.t/(vcs.t+cs.t+ms.t+fs.t+vfs.t)*100
        cs<- cs.t/(vcs.t+cs.t+ms.t+fs.t+vfs.t)*100
        ms<- ms.t/(vcs.t+cs.t+ms.t+fs.t+vfs.t)*100
        fs<- fs.t/(vcs.t+cs.t+ms.t+fs.t+vfs.t)*100
        vfs<- vfs.t/(vcs.t+cs.t+ms.t+fs.t+vfs.t)*100
      }
      rm(vcs.t, cs.t, ms.t, fs.t, vfs.t)

      # and now we will normalize sand fractions
      # if they were just converted in last step, they will already be in percent, but it doesn't hurt
      if(which.min(c(sand,100)-sfrac)==2){
        vcs<- vcs*100
        cs<- cs*100
        ms<- ms*100
        fs<- fs*100
        vfs<- vfs*100
      }
    }else{

      # if no sand fraction data is provided, we assign these dummy values which lands in the texture class
      # where no sand fraction qualifier is used
      vcs<-10
      cs<- 10
      ms<- 30
      fs<- 25
      vfs<-25
    }
    # end of the section where we manipulate the sand fractions if present

    #############################################################################################################
    # and here is the start of the main section where texture class is determined
    if(is.na(clay) | is.na(silt) | is.na(sand)){tclass <- NA

    # here we do the sands
    } else if(sand > 85 & silt + 1.5 * clay < 15){
      if(vcs + cs >=25 & ms<50 & fs<50 & vfs<50){tclass<- 'coarse sand'
      } else if (vcs + cs + ms >=25 & vcs + cs <25 & fs<50 & vfs<50 | vcs + cs >=25 & ms>=50){tclass<- 'sand'
      } else if (fs>=50 | vcs+cs+ms<25 & vfs<50){tclass<- 'fine sand'
      } else if (vfs>=50){ tclass<- 'very fine sand'
      } else {tclass<- 'sands error'}

      # here we do the loamy sands
    }else if(sand >=70 & sand <= 90 & silt + 1.5 * clay >= 15 & silt + 2 * clay < 30) {
      if(vcs + cs >=25 & ms<50 & fs<50 & vfs<50){tclass<- 'loamy coarse sand'
      } else if (vcs + cs + ms >=25 & vcs + cs <25 & fs<50 & vfs<50 | vcs + cs >=25 & ms>=50){tclass<- 'loamy sand'
      } else if (fs>=50 | vfs<50 & vcs + cs + ms <25){tclass<- 'loamy fine sand'
      } else if (vfs>=50){tclass<- 'loamy very fine sand'
      } else {tclass<- "loamy sands error"}

      # here we do the sandy loams
    }else if((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | (clay < 7 & silt < 50 & silt + 2 * clay >= 30)) {
      if(vcs + cs >= 25 & ms<50 & fs<50 & vfs<50 | vcs + cs + ms >= 30 & vfs <=30 & vfs <50){tclass<- 'coarse sandy loam'
      } else if (vcs + cs + ms >=30 & vcs + cs <30 & fs<30 & vfs<30 | vcs + cs + ms <=15 & fs<30 & vfs<30 & fs + vfs <=40 | vcs + cs >=25 & ms >=50){tclass<- 'sandy loam'
      } else if (fs>=30 & vfs<30 & vcs + cs <25 | vcs+cs+ms>=15 & vcs+cs+ms<30 & vcs+cs<25 | vfs+fs>=40 & fs>=vfs & vcs+cs+ms<=15 | vcs+cs >=25 & fs>=50){tclass<- 'fine sandy loam'
      } else if (vfs>=30 & vcs+cs+ms<15 & vfs>fs | vfs + fs >=40 & vfs>fs & vcs + cs + ms <15 | vfs>=50 & vcs+cs>=25 | vcs+cs+ms>=30 & vfs>=50){tclass<- 'very fine sandy loam'
      } else {tclass<- 'sandy loams error'}

      # and now the sand fractions no longer matter
    } else if(clay >= 7 & clay < 27 & silt >= 28 & silt < 50 & sand <= 52) {tclass<- "loam"
    } else if((silt >= 50 & clay >= 12 & clay < 27) | (silt >= 50 & silt < 80 & clay < 12)) {tclass<- 'silt loam'
    } else if(silt >= 80 & clay < 12) {tclass<- 'silt'
    } else if(clay >= 20 & clay < 35 & silt < 28 & sand > 45) {tclass<- 'sandy clay loam'
    } else if(clay >= 27 & clay < 40 & sand <= 45 & sand > 20) {tclass<- 'clay loam'
    } else if(clay >= 27 & clay < 40 & sand <= 20) {tclass<- 'silty clay loam'
    } else if(clay >= 35 & clay <= 60 & sand > 45 ) {tclass<- 'sandy clay'
    } else if(clay >= 40 & clay <= 60 & silt >= 40) {tclass<- 'silty clay'
    } else if(clay >= 40 & clay <= 60 & silt < 40 & sand <= 45) {tclass<- 'clay'
    } else if(clay >60 & tri=="USDA"){tclass <- 'clay'
    } else if(clay >60 & tri=="CSSC"){tclass<- 'heavy clay'
    } else {tclass<- 'texture class error'
    }

  }else{tclass<- NA}

  tclass
}
