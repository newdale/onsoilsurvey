% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oss.edf.R
\name{oss.edf}
\alias{oss.edf}
\title{Calculate Euclidean Distance Fields from a raster}
\usage{
oss.edf(x)
}
\arguments{
\item{x}{Raster* object}
}
\value{
RasterStack* object
}
\description{
Calculate Euclidean distance fields for an input raster.
Calculate distance from each raster cell to NW, NE, SW,
SE, Y max, X max and middle of the raster. Outputs to a rasterstack.
}
\examples{
#Calculate Euclidean distance fields from a DEM raster
library(raster)
data(keene)
oss.edf(keene)


}
