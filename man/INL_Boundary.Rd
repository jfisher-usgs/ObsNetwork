\name{INL_Boundary}

\alias{INL_Boundary}

\docType{data}

\title{Outline of the Idaho National Laboratory}

\description{
The political boundary of the Idaho National Laboratory (\href{https://en.wikipedia.org/wiki/Idaho_National_Laboratory}{INL}).
}

\usage{
INL_Boundary
}

\format{
An object of \code{\link{SpatialPolygonsDataFrame-class}} containing 1 \code{Polygon}.
Geographic coordinates are in units of decimal degrees, and in conformance with the North American Datum of 1983 (NAD 83).
}

\source{
\href{https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office}{U.S. Geological Survey-INL Project Office}.
}

\examples{
data(INL_Boundary)
plot(INL_Boundary)
}

\keyword{datasets}
