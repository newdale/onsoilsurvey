#' Sample DEM to demonstrate Euclidean Distance Fields
#'
#' Digital elevation model (DEM) for the Keene study area,
#' Peterborough County, Ontario. Data in raster format.
#' DEM derived at 20m resolution.
#'
#' @docType data
#'
#' @usage require(terra); data(keene)
#'
#' @format An object of class `"DataFrame"`.
#'
#' @keywords datasets
#'
#' @references Ontario Ministry of Natural Resources and Forestry. 2002. GTA DEM 2002.
#' (https://geohub.lio.gov.on.ca/datasets/b1ec60624b2f4f67bb9c4fb536e6b2fd)
#'
#' @source Ontario GeoHub, <https://geohub.lio.gov.on.ca/datasets/b1ec60624b2f4f67bb9c4fb536e6b2fd>
#'
#' @examples
#' library(terra)
#' data(keene)
#' keene<- terra::rast(keene, type="xyz")
#' terra::crs(keene)<- "epsg:26917"
#' plot(Keene)
#' pr <- crs(Keene)

