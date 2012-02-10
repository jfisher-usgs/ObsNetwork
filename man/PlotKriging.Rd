\name{PlotKriging}

\alias{PlotKriging}

\title{Plot Kriging}

\description{
Something.
}

\usage{
PlotKriging(obs, v.fit, grd, rm.idxs, at.pred, at.se, debug.level = 0)
}

\arguments{
\item{obs}{SpatialPointsDataFrame; something.}
\item{v.fit}{variogramModel; something.}
\item{grd}{SpatialPixels; something.}
\item{rm.idxs}{integer; vector of something.}
\item{at.pred}{numeric; a vector giving breakpoints along the range
  of predicted values.}
\item{at.se}{numeric; a vector giving breakpoints along the range
  of standard error values.}
\item{debug.level}{integer; debug level, passed to \code{\link{predict.gstat}};
  use -1 to see progress in percentage, and 0 to suppress all printed
  information.}
}

\details{
Something...
}

\value{
Something
}

\author{J.C. Fisher}

\seealso{
\code{\link{krige}}, \code{\link{spplot}}
}

\examples{
\donttest{
print("something")
}
}

\keyword{misc}
