\name{ESRP_NED500m}

\alias{ESRP_NED500m}

\docType{data}

\title{Topographic Information on the ESRP}

\description{
The Eastern Snake River Plain (ESRP) is a geologic feature located in the
state of Idaho. This data set gives topographic information for the ESRP and
vicinity on a 500 meter by 500 meter grid. The west, east, north, and
south bounding coordinates are -115.25, -111.50, 44.50, and 42.25
decimal degrees, respectively.
}

\usage{
ESRP_NED500m
}

\format{
An object of \code{\link{SpatialGridDataFrame-class}} with 393,600
points (coordinates) and a single data attribute (elevation).
Geographic coordinates are in units of decimal degrees, and in conformance
with the North American Datum of 1983 (NAD 83). All elevation values are
in meters and are referenced to the North American Vertical Datum of 1988
(NAVD 88).
}

\source{
The National Elevation Dataset (\href{http://ned.usgs.gov/}{NED})
1/3-arc-second raster. Dataset resampled to 500 meters using a nearest
neighbor assignment.
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
data(ESRP_NED500m)
image(ESRP_NED500m)
summary(ESRP_NED500m)

# Commands used to construct this data object:
f <- system.file("extdata/ESRP_NED500m.tif", package = "ObsNetwork")
obj <- rgdal::readGDAL(f, band = 1)
names(obj) <- "var2"

identical(obj, ESRP_NED500m)
}

\keyword{datasets}
