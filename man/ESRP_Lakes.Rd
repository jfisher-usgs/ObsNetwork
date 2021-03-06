\name{ESRP_Lakes}

\alias{ESRP_Lakes}

\docType{data}

\title{Outlines of the ESRP Lakes}

\description{
Major lakes of the eastern Snake River Plain (ESRP) and surrounding areas.
}

\usage{
ESRP_Lakes
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing a set of \code{Polygons}.
Geographic coordinates are in units of decimal degrees, and in conformance with the North American Datum of 1983 (NAD 83).
}

\source{
Idaho Department of Water Resources (\href{https://data-idwr.opendata.arcgis.com/pages/gis-data}{IDWR}).
}

\examples{
data(ESRP_Lakes)
plot(ESRP_Lakes)
}

\keyword{datasets}
