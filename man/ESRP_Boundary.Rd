\name{ESRP_Boundary}

\alias{ESRP_Boundary}

\docType{data}

\title{Outlines of the ESRP Aquifer and Buttes}

\description{
An outline of the generalized boundary of the
Eastern Snake River Plain (ESRP) aquifer,
East butte, Middle butte, and Big Southern butte.
}

\usage{
ESRP_Boundary
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing
4 \code{\link{Polygons}}. The three polygons representing buttes
are set as holes in the much larger aquifer polygon.
Geographic coordinates are in units of decimal degrees, and in conformance
with the North American Datum of 1983 (NAD 83).
}

\source{
Idaho Department of Water Resources
(\href{http://www.idwr.idaho.gov/GeographicInfo/GISdata/gis_data.htm}{IDWR}).
}

\examples{
data(ESRP_Boundary)
plot(ESRP_Boundary)
summary(ESRP_Boundary)

# Commands used to construct this data object:
dsn <- system.file("extdata/ESRP_Boundary", package = "ObsNetwork")
obj <- readOGR(dsn = dsn, layer = basename(dsn))

all.equal(obj, ESRP_Boundary)

# Polygons slots
p <- sapply(slot(obj, "polygons"), function(i) slot(i, "Polygons"))

# Generalized boundary of the ESRP aquifer
plot(SpatialPolygons(list(Polygons(list(p[[1]]), 1))))

# Generalized boundary of the East butte
plot(SpatialPolygons(list(Polygons(list(p[[2]]), 1))), add = TRUE)

# Generalized boundary of the Middle butte
plot(SpatialPolygons(list(Polygons(list(p[[3]]), 1))), add = TRUE)

# Generalized boundary of the Big Southern butte
plot(SpatialPolygons(list(Polygons(list(p[[4]]), 1))), add = TRUE)
}

\keyword{datasets}
