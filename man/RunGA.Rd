\name{RunGA}

\alias{RunGA}

\title{Run Genetic Algorithm}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information.
}

\usage{
RunGA(obs, network, grd, nsites, vg.model, formula, nmax = Inf,
      niters = 200, pop.size = 200, obj.weights = c(1, 1, 1, 1))
}

\arguments{
\item{obs}{SpatialPointsDataFrame; something.}
\item{network}{character; something.}
\item{grd}{something.}
\item{nsites}{integer; the desired number of observation sites to remove.}
\item{vg.model}{something}
\item{formula}{something}
\item{nmax}{something}
\item{niters}{integer; the number of iterations in the genetic algorithm.}
\item{pop.size}{integer; the population size in the genetic algorithm.}
\item{obj.weights}{numeric; something.}
}

\details{
Something...
}

\value{
Something
}

\author{J.C. Fisher}

\seealso{
\code{\link{krige}}, \code{\link{rbga}}
}

\examples{
\donttest{
print("something")
}
}

\keyword{misc}
