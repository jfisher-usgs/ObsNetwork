\name{RemoveSites}

\alias{RemoveSites}

\title{Site Removal From Observation Network}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information.
}

\usage{
RemoveSites(nsites = 10, niters = 200, pop.size = 200,
            dx = 0.03, dy = dx)
}

\arguments{
\item{nsites}{integer; the desired number of observation sites to remove.}
\item{niters}{integer; the number of iterations in the genetic algorithm.}
\item{pop.size}{integer; the population size in the genetic algorithm.}
\item{dx, dy}{numeric; the resolution of the kriging grid in the longitude
  and latitude coordinate directions.}
}

\details{
Set the working directory so that \file{ObservationData.txt}
and \file{SpatialDomain.ply} are located in
\file{./inst/exdata/}.
}

\value{
Returns a list containing the following components:
  \item{obs}{SpatialPointsDataFrame; a table of all site information.}
  \item{obs.rm}{SpatialPointsDataFrame; a table of site information
    for removed sites.}
  \item{obj.values}{matrix; objective values for each iteration of the
    genetic algorithm.}
}

\author{Fisher, J.C.}

\seealso{
\code{\link{read.table}}, \code{\link{variogram}}, \code{gstat},
\code{predict}, \code{rbga}
}

\examples{
\donttest{
ans <- RemoveSites(nsites = 20)
ans$obj.values
format(ans$obs.rm$Site_ID, scientific = FALSE)
}
}

\keyword{misc}
