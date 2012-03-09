\name{RunGA}

\alias{RunGA}

\title{Run Genetic Algorithm}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information.
}

\usage{
RunGA(pts, network, grd, nsites, vg.model, formula, nmax = Inf,
      niters = 200, pop.size = 200, obj.weights = c(1, 1, 1, 1))
}

\arguments{
\item{pts}{SpatialPointsDataFrame; data at observation sites.}
\item{network}{character; only observation sites in this network
  will be included in the optimization.}
\item{grd}{SpatialGridDataFrame; raster data.}
\item{nsites}{integer; the number of observation sites to remove from the
  network.}
\item{vg.model}{variogramModel; variogram model of dependent variable defined
  by a call to \code{\link{vgm}}.}
\item{formula}{formula; defines the dependent variable as a linear model of
  independent variables.}
\item{nmax}{numeric; for local kriging, the number of nearest observations
  that should be used for a kriging prediction or simulation, where nearest
  is defined in terms of the space of the spatial locations. By default,
  all observations are used.}
\item{niters}{integer; the number of iterations used in the genetic algorithm.}
\item{pop.size}{integer; the population size used in the genetic algorithm.}
\item{obj.weights}{numeric; vector of length 4 giving the weights for each
  objective function in the optimzation.}
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
data(ESRP_NED500m)
data(ESRP_WaterLevels_2008)
data(ESRP_SpatialDomain)

network <- "State"
xlim <- c(-115.25, -111.50)
ylim <- c(  42.25,   44.50)
fo <- var1 ~ var2
vg.model <- vgm(psill=4500, model = "Sph", range = 100, nugget = 0)
}

\keyword{optimize}
