\name{BuildGrid}

\alias{BuildGrid}

\title{Build Spatial Grid}

\description{
A grid is constructed from a polygon representing the spatial domain.
}

\usage{
BuildGrid(x, file, x.var, y.var, compute.chull = FALSE, dx = 0.03, dy = dx,
          projargs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
}

\arguments{
\item{x}{SpatialPointsDataFrame; something.}
\item{file}{character; the file name which the polygon data is to be read from.}
\item{x.var, y.var}{character; column name of spatial coordiantes \code{(x,y)}.}
\item{compute.chull}{logical; if \code{TRUE} the convex hull is computed from
  spatial coordinates; its default is \code{FALSE}.}
\item{dx, dy}{numeric; the resolution of the kriging grid in the \code{x}
  and \code{y} coordinate directions.}
\item{projargs}{character; A character string of projection arguments;
  the arguments must be entered exactly as in the PROJ.4 documentation.}
}

\details{
Something...
}

\value{
Returns an object of class \code{SpatialPixels}.
}

\author{J.C. Fisher}

\seealso{
\code{\link{SpatialPoints}}, \code{\link{CRS}}, \code{\link{expand.grid}}
}

\examples{
\donttest{
print("something")
}
}

\keyword{misc}
