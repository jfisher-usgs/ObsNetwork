\name{INL_Boundary}

\alias{INL_Boundary}

\docType{data}

\title{Outline of the Idaho National Laboratory}

\description{
The political boundary of the Idaho National Laboratory
(\href{http://en.wikipedia.org/wiki/Idaho_National_Laboratory}{INL}).
}

\usage{
INL_Boundary
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing
1 \code{\link{Polygon}}.
Geographic coordinates are in units of decimal degrees, and in conformance
with the North American Datum of 1983 (NAD 83).
}

\source{
U.S. Geological Survey
\href{http://id.water.usgs.gov/projects/INL}{INL Project Office}.
}

\examples{
data(INL_Boundary)
plot(INL_Boundary)
}

\keyword{datasets}
