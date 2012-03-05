\name{ESRP_NED500m}

\alias{ESRP_NED500m}

\docType{data}

\title{Topographic Information on Eastern Snake River Plain}

\description{
The Eastern Snake River Plain (ESRP) is a geologic feature located in the
state of Idaho. This data set gives topographic information for the ESRP on
a 500m by 500m grid.
}

\usage{
ESRP_NED500m
}

\format{
An object of SpatialGridDataFrame-class with 393,600 points (coordinates) and
a sing data attribute (elevation).
}

\source{
The National Elevation Dataset (NED) 1/3-arc-second dataset.
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
summary(ESRP_NED500m)

# Commands used to construct this data set:
f <- system.file("extdata/ESRP_NED500m.tif", package = "ObsNetwork")
ESRP_NED500m <- rgdal::readGDAL(f, band = 1)
names(ESRP_NED500m) <- "var2"
}

\keyword{datasets}
