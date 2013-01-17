\name{ESRP_NED}

\alias{ESRP_NED}

\docType{data}

\title{Topographic Information on the ESRP}

\description{
The eastern Snake River Plain (ESRP) is a geologic feature located in the
state of Idaho. This data set gives topographic information for the ESRP and
vicinity on a 500 by 500 meter grid. The west, east, north, and
south bounding coordinates are about -115.4, -111.4, 44.5, and 42.2
decimal degrees, respectively.
}

\usage{
ESRP_NED
}

\format{
An object of \code{\link{SpatialGridDataFrame-class}} with 323,724
points (coordintates) and a single data attribute, \code{var2}, the
land-surface elevation.
Geographic coordinates are in units of meters, in conformance
with the North American Datum of 1927 (NAD 27), and placed in a
Albers Equal-Area Conic projection; standard parellels 42.83, 44.16;
central meridian -113.00, false easting 200,000 meters; false northing 0.
}

\source{
The National Elevation Dataset (\href{http://ned.usgs.gov/}{NED})
1-arc-second raster. Dataset resampled to 10 arc-seconds using a
cubic convolution assignment. All other data processing is shown in
\sQuote{Examples} section.
}

\references{
Gesch, D.B., 2007, The National Elevation Dataset, in Maune, D., ed.,
Digital Elevation Model Technologies and Applications: The DEM Users Manual,
2nd Edition: Bethesda, Maryland, American Society for Photogrammetry and
Remote Sensing, p. 99-118.

Gesch, D., Oimoen, M., Greenlee, S., Nelson, C., Steuck, M., and Tyler, D.,
2002, The National Elevation Dataset: Photogrammetric Engineering and
Remote Sensing, v. 68, no. 1, p. 5-11.
}

\examples{
data(ESRP_NED)
image(ESRP_NED)
summary(ESRP_NED)

# Commands used to construct this data object:
f <- system.file("extdata/ESRP_NED.tif", package = "ObsNetwork")
grd <- rgdal::readGDAL(f, band = 1)
names(grd) <- "var2"
summary(grd)
grd.attr <- as.data.frame(slot(grd, "grid"))
summary(grd.attr)

# Transform coordinates to meters; Albers Equal-Area Conic projection; NAD27
projargs <- paste("+proj=aea +lat_1=42.83333333333333 +lat_2=44.16666666666666",
                  "+lat_0=41.5 +lon_0=-113 +x_0=200000 +y_0=0 +ellps=clrk66",
                  "+datum=NAD27 +units=m +no_defs")
pts <- suppressWarnings(spTransform(grd, CRS(projargs)))
class(pts)

# Resample data using reduced grid size of 500 by 500 meters
dx <- 500
dy <- 500
xlim <- c(10000, 328000)
ylim <- c(81200, 335700)
cellcentre.offset <- c(min(xlim) + dx / 2, min(ylim) + dy / 2)
cellsize <- c(dx, dy)
cells.dim <- c(diff(range(xlim)) / dx, diff(range(ylim)) / dy)
newdata <- GridTopology(cellcentre.offset = cellcentre.offset,
                        cellsize = cellsize, cells.dim = cells.dim)
newdata <- SpatialGrid(newdata, proj4string = CRS(projargs))
coordnames(newdata) <- c("x", "y")
\donttest{
# Inverse distance weighting interpolation; computationally demanding
obj <- gstat::idw(var2 ~ 1, pts, newdata, idp = 2.0, maxdist = 250)
obj$var1.var <- NULL
names(obj) <- "var2"

# Compare with available data set
identical(obj, ESRP_NED)
}}

\keyword{datasets}
