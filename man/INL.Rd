\name{INL}

\alias{INL}

\docType{data}

\title{Idaho National Laboratory}

\description{
Political boundary of the Idaho National Laboratory
(\href{http://en.wikipedia.org/wiki/Idaho_National_Laboratory}{INL}).
}

\usage{
INL
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing
1 \code{\link{Polygon}}.
Geographic coordinates are in units of decimal degrees, and in conformance
with the North American Datum of 1983 (NAD 83).
}

\source{
U.S. Geological Survey INL Project Office,
\url{http://id.water.usgs.gov/projects/INL}.
}

\examples{
data(INL)
plot(INL)
}

\keyword{datasets}
