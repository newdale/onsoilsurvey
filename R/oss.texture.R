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
#' oss.texture(sand=67, silt=23, clay=10, vcs=27, cs=10, ms=10, fs=10, vfs=10)
#'
#' #Determine texture class for multiple observations without sand fractions
#' dat<- data.frame(sand=c(20,40,80), silt= c(15,30,10), clay= c(65,30,10),
#' vcs=c(2,4,8), cs=c(1,2,4), ms=c(1,2,4), fs=c(12,24,48), vfs=c(4,8,16))
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
oss.texture<- function(sand, silt, clay, vcs=NA, cs=NA, ms=NA, fs=NA, vfs=NA, tri="CSSC"){

  # 0. Force all arguments into vectors of the same length
  n <- length(sand)
  vcs <- if(length(vcs) == 1) rep(vcs, n) else vcs
  cs  <- if(length(cs)  == 1) rep(cs, n)  else cs
  ms  <- if(length(ms)  == 1) rep(ms, n)  else ms
  fs  <- if(length(fs)  == 1) rep(fs, n)  else fs
  vfs <- if(length(vfs) == 1) rep(vfs, n) else vfs
  tri <- if(length(tri) == 1) rep(tri, n) else tri

  # 1. Handle decimal vs percentage (0.1 vs 10)
  psa_raw <- sand + silt + clay
  is_decimal <- abs(1 - psa_raw) < abs(100 - psa_raw)

  sand <- dplyr::if_else(is_decimal, sand * 100, sand)
  silt <- dplyr::if_else(is_decimal, silt * 100, silt)
  clay <- dplyr::if_else(is_decimal, clay * 100, clay)

  # 2. Normalize totals to 100
  total <- sand + silt + clay
  sand <- (sand / total) * 100
  silt <- (silt / total) * 100
  clay <- (clay / total) * 100

  # 3. Handle Sand Fractions (vcs, cs, etc.)
  # If any fraction is NA, use the dummy distribution from your original logic
  has_fractions <- !is.na(vcs) & !is.na(cs) & !is.na(ms) & !is.na(fs) & !is.na(vfs)

  vcs_f <- dplyr::if_else(has_fractions, vcs, 0.10 * sand)
  cs_f  <- dplyr::if_else(has_fractions, cs,  0.10 * sand)
  ms_f  <- dplyr::if_else(has_fractions, ms,  0.30 * sand)
  fs_f  <- dplyr::if_else(has_fractions, fs,  0.25 * sand)
  vfs_f <- dplyr::if_else(has_fractions, vfs, 0.25 * sand)

  # Normalize fractions to the actual sand total
  f_total <- vcs_f + cs_f + ms_f + fs_f + vfs_f
  vcs <- (vcs_f / f_total) * sand
  cs  <- (cs_f / f_total) * sand
  ms  <- (ms_f / f_total) * sand
  fs  <- (fs_f / f_total) * sand
  vfs <- (vfs_f / f_total) * sand

  # 4. Main Classification logic
  dplyr::case_when(
    is.na(sand) | is.na(silt) | is.na(clay) ~ NA_character_,

    # SANDS
    sand > 85 & silt + 1.5 * clay < 15 ~ dplyr::case_when(
      vcs + cs >= 25 & ms < 50 & fs < 50 & vfs < 50 ~ "coarse sand",
      (vcs + cs + ms >= 25 & vcs + cs < 25 & fs < 50 & vfs < 50) | (vcs + cs >= 25 & ms >= 50) ~ "sand",
      fs >= 50 | (vcs + cs + ms < 25 & vfs < 50) ~ "fine sand",
      vfs >= 50 ~ "very fine sand",
      TRUE ~ "sands error"
    ),

    # LOAMY SANDS
    sand >= 70 & sand <= 90 & silt + 1.5 * clay >= 15 & silt + 2 * clay < 30 ~ dplyr::case_when(
      vcs + cs >= 25 & ms < 50 & fs < 50 & vfs < 50 ~ "loamy coarse sand",
      (vcs + cs + ms >= 25 & vcs + cs < 25 & fs < 50 & vfs < 50) | (vcs + cs >= 25 & ms >= 50) ~ "loamy sand",
      fs >= 50 | (vfs < 50 & vcs + cs + ms < 25) ~ "loamy fine sand",
      vfs >= 50 ~ "loamy very fine sand",
      TRUE ~ "loamy sands error"
    ),

    # SANDY LOAMS
    (clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | (clay < 7 & silt < 50 & silt + 2 * clay >= 30) ~ dplyr::case_when(
      (vcs + cs >= 25 & ms < 50 & fs < 50 & vfs < 50) | (vcs + cs + ms >= 30 & vfs >= 30 & vfs < 50) ~ "coarse sandy loam",
      (vcs + cs + ms >= 30 & vcs + cs < 30 & fs < 30 & vfs < 30) | (vcs + cs + ms <= 15 & fs < 30 & vfs < 30 & fs + vfs <= 40) | (vcs + cs >= 25 & ms >= 50) ~ "sandy loam",
      (fs >= 30 & vfs < 30 & vcs + cs < 25) | (vcs + cs + ms >= 15 & vcs + cs + ms < 30 & vcs + cs < 25) | (vfs + fs >= 40 & fs >= vfs & vcs + cs + ms <= 15) | (vcs + cs >= 25 & fs >= 50) ~ "fine sandy loam",
      (vfs >= 30 & vcs + cs + ms < 15 & vfs > fs) | (vfs + fs >= 40 & vfs > fs & vcs + cs + ms < 15) | (vfs >= 50 & vcs + cs >= 25) | (vcs + cs + ms >= 30 & vfs >= 50) ~ "very fine sandy loam",
      TRUE ~ "sandy loams error"
    ),

    # BASIC CLASSES
    clay >= 7 & clay < 27 & silt >= 28 & silt < 50 & sand <= 52 ~ "loam",
    (silt >= 50 & clay >= 12 & clay < 27) | (silt >= 50 & silt < 80 & clay < 12) ~ "silt loam",
    silt >= 80 & clay < 12 ~ "silt",
    clay >= 20 & clay < 35 & silt < 28 & sand > 45 ~ "sandy clay loam",
    clay >= 27 & clay < 40 & sand <= 45 & sand > 20 ~ "clay loam",
    clay >= 27 & clay < 40 & sand <= 20 ~ "silty clay loam",
    clay >= 35 & clay <= 60 & sand > 45 ~ "sandy clay",
    clay >= 40 & clay <= 60 & silt >= 40 ~ "silty clay",
    clay >= 40 & clay <= 60 & silt < 40 & sand <= 45 ~ "clay",
    clay > 60 & tri == "USDA" ~ "clay",
    clay > 60 & tri == "CSSC" ~ "heavy clay",
    TRUE ~ "texture class error"
  )

}
