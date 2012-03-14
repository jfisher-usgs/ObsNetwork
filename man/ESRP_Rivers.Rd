\name{ESRP_Rivers}

\alias{ESRP_Rivers}

\docType{data}

\title{Traces of the ESRP Rivers}

\description{
Major rivers of the Eastern Snake River Plain (ESRP) and surrounding areas.
}

\usage{
ESRP_Rivers
}

\format{
An object of \code{\link{SpatialLinesDataFrame-class}} containing a
set of \code{\link{Lines}}.
Geographic coordinates are in units of decimal degrees, and in conformance
with the North American Datum of 1983 (NAD 83).
}

\source{
Idaho Department of Water Resources
(\href{http://www.idwr.idaho.gov/GeographicInfo/GISdata/gis_data.htm}{IDWR}).
}

\examples{
data(ESRP_Rivers)
plot(ESRP_Rivers)
}

\keyword{datasets}
