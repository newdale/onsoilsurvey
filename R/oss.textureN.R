#' Determine Texture Class
#'
#'Use sand, silt and clay data to determine the texture
#' class in the Canadian System of Soil Classification.
#' Closely resembles oss.texture, but returns an integer
#' for creating raster
#'
#' @param sand numeric
#' @param silt numeric
#' @param clay numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#'
#' #Determine texture class for a single observation
#' oss.texture.n(67,10,23)
#'
#' #Determine texture class for multiple observations
#' dat<- data.frame(sand=c(20,40,80), silt= c(15,30,10), clay= c(65,30,10))
#' mapply(oss.texture.n,sand=dat$sand, silt=dat$silt, clay=dat$clay)
#'
#' #or return it as a new column in the data frame
#' dat$class<- mapply(oss.texture.n,sand=dat$sand, silt=dat$silt, clay=dat$clay)
#'
oss.texture.n<- function(sand, silt, clay){

  clay<- clay
  silt<- silt
  sand<- sand

  if(is.na(clay)){tclass <- NA
  } else if(is.na(silt)){tclass <- NA
  } else if (is.na(sand)){tclass <- NA
  } else if (sand + silt + clay > 102 | sand + silt + clay < 98) {tclass <- NA

  } else if(silt + 1.5 * clay < 15){tclass<- 1
  } else if(silt + 1.5 * clay >= 15 & silt + 2 * clay < 30) {tclass<- 2
  } else if((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | (clay < 7 & silt < 50 & silt + 2 * clay >= 30)) {tclass<- 3
  } else if(clay >= 7 & clay < 27 & silt >= 28 & silt < 50 & sand <= 52) {tclass<- 4
  } else if((silt >= 50 & clay >= 12 & clay < 27) | (silt >= 50 & silt < 80 & clay < 12)) {tclass<- 5
  } else if(silt >= 80 & clay < 12) {tclass<- 6
  } else if(clay >= 20 & clay < 35 & silt < 28 & sand > 45) {tclass<- 7
  } else if(clay >= 27 & clay < 40 & sand <= 45 & sand > 20) {tclass<- 8
  } else if(clay < 40 & clay >= 27 & sand <= 20) {tclass<- 9
  } else if(clay >= 35 & clay <= 60 & sand > 45 ) {tclass<- 10
  } else if(clay >= 40 & clay <= 60 & silt >= 40) {tclass<- 11
  } else if(clay >= 40 & clay <= 60 & silt < 40 & sand <= 45) {tclass<- 12
  } else if(clay >60){tclass<- 13
  } else {tclass<- NA
  }
  tclass
}
