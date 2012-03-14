\name{ESRP_Lakes}

\alias{ESRP_Lakes}

\docType{data}

\title{ESRP Lakes}

\description{
Major lakes of the Eastern Snake River Plain (ESRP) and surrounding areas.
}

\usage{
ESRP_Lakes
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing
a set of \code{\link{Polygons}}.
Geographic coordinates are in units of decimal degrees, and in conformance
with the North American Datum of 1983 (NAD 83).
}

\source{
Idaho Department of Water Resources
(\href{http://www.idwr.idaho.gov/GeographicInfo/GISdata/gis_data.htm}{IDWR}).
}

\examples{
data(ESRP_Lakes)
plot(ESRP_Lakes)
}

\keyword{datasets}
