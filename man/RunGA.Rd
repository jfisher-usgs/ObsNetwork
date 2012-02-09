\name{RunGA}

\alias{RunGA}

\title{Run Genetic Algorithm}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information.
}

\usage{
RunGA(obs, v.fit, grd, nsites, niters = 200, pop.size = 200,
      obj.weights = c(1, 1, 1))
}

\arguments{
\item{obs}{something}
\item{v.fit}{something}
\item{grd}{something}
\item{nsites}{integer; the desired number of observation sites to remove.}
\item{niters}{integer; the number of iterations in the genetic algorithm.}
\item{pop.size}{integer; the population size in the genetic algorithm.}
\item{obj.weights}{something}
}

\details{
Something...
}

\value{
Something
}

\author{J.C. Fisher}

\seealso{
\code{\link{plot}}
}

\examples{
\donttest{
print("something")
}
}

\keyword{misc}
