\name{keene}
\alias{keene}
\docType{data}
\title{Keene Study Area Digital Elevation Model, Ontario, Canada}
\description{
Digital elevation model (DEM) for the Keene study area,
Peterborough County, Ontario. Data in raster format.
DEM derived at 20m resolution.
}
\usage{data("keene")}
\format{
  The format is:
Formal class 'RasterLayer' [package "raster"] with 12 slots
  ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  .. .. ..@ name        : chr "C:\\Users\\atiko\\Desktop\\Keene\\covariates\\DEM.tif"
  .. .. ..@ datanotation: chr "FLT4S"
  .. .. ..@ byteorder   : chr "little"
  .. .. ..@ nodatavalue : num -Inf
  .. .. ..@ NAchanged   : logi FALSE
  .. .. ..@ nbands      : int 1
  .. .. ..@ bandorder   : chr "BIL"
  .. .. ..@ offset      : int 0
  .. .. ..@ toptobottom : logi TRUE
  .. .. ..@ blockrows   : int 128
  .. .. ..@ blockcols   : int 128
  .. .. ..@ driver      : chr "gdal"
  .. .. ..@ open        : logi FALSE
  ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  .. .. ..@ values    : logi(0)
  .. .. ..@ offset    : num 0
  .. .. ..@ gain      : num 1
  .. .. ..@ inmemory  : logi FALSE
  .. .. ..@ fromdisk  : logi TRUE
  .. .. ..@ isfactor  : logi FALSE
  .. .. ..@ attributes: list()
  .. .. ..@ haveminmax: logi TRUE
  .. .. ..@ min       : num 187
  .. .. ..@ max       : num 274
  .. .. ..@ band      : int 1
  .. .. ..@ unit      : chr ""
  .. .. ..@ names     : chr "DEM"
  ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. ..@ type      : chr(0)
  .. .. ..@ values    : logi(0)
  .. .. ..@ color     : logi(0)
  .. .. ..@ names     : logi(0)
  .. .. ..@ colortable: logi(0)
  ..@ title   : chr(0)
  ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. ..@ xmin: num 719909
  .. .. ..@ xmax: num 732309
  .. .. ..@ ymin: num 4896217
  .. .. ..@ ymax: num 4913037
  ..@ rotated : logi FALSE
  ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. ..@ geotrans: num(0)
  .. .. ..@ transfun:function ()
  ..@ ncols   : int 620
  ..@ nrows   : int 841
  ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"
  ..@ history : list()
  ..@ z       : list()
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(keene)
## maybe str(keene) ; plot(keene) ...
}
\keyword{datasets}
