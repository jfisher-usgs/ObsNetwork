\name{ESRP_Boundary}

\alias{ESRP_Boundary}

\docType{data}

\title{Outlines of the ESRP and Buttes}

\description{
An outline of the generalized boundary of the eastern Snake River Plain (ESRP), East Butte, Middle Butte, and Big Southern Butte.
}

\usage{
ESRP_Boundary
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing 4 \code{Polygons}.
Three of the polygons represent buttes and are set as holes in the much larger aquifer polygon.
Geographic coordinates are in units of decimal degrees, and in conformance with the North American Datum of 1983 (NAD 83).
}

\source{
Idaho Department of Water Resources (\href{https://data-idwr.opendata.arcgis.com/pages/gis-data}{IDWR}).
}

\examples{
data(ESRP_Boundary)
plot(ESRP_Boundary)
summary(ESRP_Boundary)

# Commands used to construct this data object:
dsn <- system.file("extdata/ESRP_Boundary", package = "ObsNetwork")
obj <- readOGR(dsn = dsn, layer = basename(dsn))
identical(obj, ESRP_Boundary)

# Polygon slots
p <- sapply(slot(obj, "polygons"), function(i) slot(i, "Polygons"))

# Generalized boundary of the ESRP aquifer
plot(SpatialPolygons(list(Polygons(list(p[[1]]), 1))))

# Generalized boundary of the East Butte
plot(SpatialPolygons(list(Polygons(list(p[[2]]), 1))), add = TRUE)

# Generalized boundary of the Middle Butte
plot(SpatialPolygons(list(Polygons(list(p[[3]]), 1))), add = TRUE)

# Generalized boundary of the Big Southern Butte
plot(SpatialPolygons(list(Polygons(list(p[[4]]), 1))), add = TRUE)
}

\keyword{datasets}
