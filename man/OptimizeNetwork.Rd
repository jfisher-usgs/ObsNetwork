\name{OptimizeNetwork}

\alias{OptimizeNetwork}

\title{Optimize Observation Network}

\description{
Determine sites to exclude from an existing observation network because they provide little or no beneficial added information.
A kriging-based genetic algorithm (GA) is used to solve the multi-objective optimization problem.
}

\usage{
OptimizeNetwork(pts, grd, ply, network.nm, nsites, model, formula, nmax = Inf,
                xlim = bbox(grd)[1, ], ylim = bbox(grd)[2, ], grd.fact = 1,
                obj.weights = c(1, 1, 1, 1), penalty.constant = 1E6,
                maxabort = 10, popSize = 50, pcrossover = 0.8, pmutation = 0.1,
                elitism = base::max(1, round(popSize * 0.05)),
                maxiter = 100, run = maxiter, suggestions = NULL, ...)
}

\arguments{
\item{pts}{SpatialPointsDataFrame; data at observation sites.
  Required data frame variables include:
    \code{site.no}, a unique site number;
    \code{var1}, the dependent variable (such as, the water-level elevation);
    \code{var1.acy}, the mean measurement error of the dependent variable;
    \code{var1.sd}, the standard deviation of the dependent variable.
  An optional \code{network.nm} variable may be included to identify a sites observation network(s).
  Sites belonging to multiple networks are specified using comma separation.
  Note that duplicate site numbers and (or) spatial coordinates are not permitted.}
\item{grd}{SpatialGridDataFrame; interpolation grid.
  For kriging with external drift (KED) a data frame variable \code{var2}, the independent variable (such as, land-surface elevation), is required.}
\item{ply}{SpatialPolygonsDataFrame; a polygon used to crop the raster grid (optional).}
\item{network.nm}{character; vector of observation network names.
  Only sites belonging to this network will be included in the analysis; this argument is optional, in its absence all sites are assumed to belong to a single network (that is, all sites are used).}
\item{nsites}{integer; number of sites to remove from the observation network.}
\item{model}{variogramModel; variogram model of dependent variable defined by a call to \code{\link{vgm}}.}
\item{formula}{formula; defines the dependent variable as a linear model of the independent variables.}
\item{nmax}{numeric; for local kriging, the number of nearest sites that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations.
  By default, all sites are used.}
\item{xlim}{numeric; vector of length 2 giving left and right limits for the x-axis, used to crop the interpolation grid.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for the y-axis, used to crop the interpolation grid.}
\item{grd.fact}{integer; aggregation factor for the interpolation grid, \code{grd}.}
\item{obj.weights}{numeric; vector of length 4 giving the weights for each objective in the multi-objective optimization problem, see \sQuote{details} section below.}
\item{penalty.constant}{numeric; constant in the penalty function, its value needs to be greater than the largest possible fitness value.}
\item{maxabort}{integer; maximum number of times an invalid child chromosome can be aborted during crossover.}
\item{popSize}{integer; population size.}
\item{pcrossover}{numeric; probability of crossover between pairs of chromosomes.}
\item{pmutation}{numeric; probability of mutation in a parent chromosome.}
\item{elitism}{integer; number of chromosomes to survive into the next generation.
  By default is about 5 percent of the population size.}
\item{maxiter}{integer; maximum number of iterations to run before the GA search is halted.}
\item{run}{integer; number of consecutive generations without any improvement in the \dQuote{best} fitness value before the GA is stopped.}
\item{suggestions}{matrix; initial population.}
\item{...}{additional arguments to be passed to \code{\link{ga}}.}
}

\details{
A solution to the multi-objective optimization problem is found by minimizing the aggregate objective function, the weighted linear sum of 4 objectives.
The objectives are given as:
\enumerate{
\item Mean standard error at points in interpolation grid.
\item Root-mean-square error, difference between predicted and measured values, at removed sites.
\item Mean standard deviation, variability of measurement over time, at removed sites.
\item Mean measurement error, at remaining sites.
}
The \dQuote{best} solution found will depend on the relative values of the weights specified in \code{obj.weights}.
For example, if a higher weight is specified for the mean standard error, the solution will be one that favors a smaller mean standard error over a small root-mean squared error, mean standard deviation, and mean measurement error.
Setting a weight equal to zero will remove an objective from the multi-objective function.

Spatial data is transformed to the map projection and datum of the raster data set in \code{grd}.

The optimization problem is solved using a GA with integer chromosomes; site identifiers are represented as binary strings using \href{http://en.wikipedia.org/wiki/Gray_Encoding}{Gray} encoding.
The initial population is randomly generated with valid chromosomes; that is, sites are not repeated within a single chromosome.
The GA uses linear-rank selection, single-point crossover, and uniform random mutation.
}

\value{
Returns a list with components:
\item{call}{character; function call with all specified arguments.}
\item{pts.rm}{SpatialPointsDataFrame; a subset of \code{pts} with row length equal to \code{nsites}.
  Includes data records for those sites identified by the GA for removal from the observation network(s).}
\item{is.net}{logical; vector of length equal to the number of rows in \code{pts} specifying sites belonging to the reduced network.}
\item{is.rm}{logical; vector of length equal to the number of rows in \code{pts} specifying sites to remove from the network.}
\item{obj.values}{matrix; objective values at each iteration of the GA.
  This matrix has \code{maxiter} rows and 5 columns (that is, the 4 objective values and their sum).}
\item{niter}{integer; number of completed iterations.}
\item{nrep.ans}{integer; number of iterations the \dQuote{best} solution was repeated.}
\item{proc.time}{proc_time; CPU time for running the GA, in seconds.}
\item{ncalls.penalty}{integer; vector giving the number of calls to the penalty function at each iteration of the GA.}
\item{kr}{SpatialGridDataFrame; a data frame containing the coordinates of \code{grd} cropped to the axis limits and polygon.
  Data attributes based on block kriging of the reduced network include: predictions, \code{var1.pred}; prediction variances, \code{var1.var}; and prediction standard errors, \code{var1.se}.
  Differences between the original and reduced network predictions are specified in the \code{var1.diff} attribute.}
\item{rmsd}{numeric; root-mean-square-deviation between the kriged surfaces using the original and reduced networks.}
\item{local.error}{numeric; percent local error between the kriged surfaces using the original and reduced networks.}
\item{obj.space}{matrix; range of objective values in solution space.}
\item{ga.ans}{ga; returned value of \code{\link{ga}}.}
\item{start.time}{POSIXct; system time at start of GA run.}
\item{diff.time}{numeric; vector of time differences since start of GA run, specified for each iteration in the GA, in hours.}
The status of the objective values after each iteration of the GA is plotted.
}

\author{J.C. Fisher}

\references{
Fisher, J.C., 2013, Optimization of water-level monitoring networks in the eastern Snake River Plain aquifer using a kriging-based genetic algorithm method: U.S. Geological Survey Scientific Investigations Report 2013-5120 (DOE/ID-22224), 74 p., \url{http://pubs.usgs.gov/sir20135120/}.

Scrucca, Luca, 2013, GA: a package for genetic algorithms in R, Journal of Statistical Software, v. 53, no. 4, 37 p., \url{http://www.jstatsoft.org/v53/i04/}.
}

\seealso{
\code{\link{WriteGAResults}}, \code{\link{krige}}, \code{\link{ga}}
}

\examples{
data(ESRP_NED)
data(ESRP_NWIS)
data(ESRP_Boundary)

# Formula and variogram
fo <- var1 ~ x + y
model <- vgm(psill = 1948.533, model = "Sph", nugget = 0, range = 153891.038)

# Optimize combined "State" and "INL" networks
xlim <- c(10000, 328000)
ylim <- c(81200, 335700)
ans <- OptimizeNetwork(ESRP_NWIS, ESRP_NED, ESRP_Boundary,
                       network.nm = c("State", "INL"), nsites = 20,
                       model = model, formula = fo, grd.fact = 5,
                       obj.weights = c(100, 1, 1, 1), popSize = 20,
                       maxiter = 3)
PlotRaster(ans$kr, "var1.pred", ESRP_NWIS,
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions", net.idxs = which(ans$is.net),
           rm.idxs = which(ans$is.rm), xlim = xlim, ylim = ylim)
PlotRaster(ans$kr, "var1.se", ESRP_NWIS,
           pal = terrain.colors, main = "Standard errors",
           net.idxs = which(ans$is.net), rm.idxs = which(ans$is.rm),
           xlim = xlim, ylim = ylim)

# Optimize "INL" network
xlim <- c(178000, 257500)
ylim <- c(202000, 272000)
ans <- OptimizeNetwork(ESRP_NWIS, ESRP_NED, ESRP_Boundary,
                       network.nm = "INL", nsites = 20, model = model,
                       formula = fo, xlim = xlim, ylim = ylim, grd.fact = 5,
                       obj.weights = c(100, 1, 1, 1),  maxiter = 3)
PlotRaster(ans$kr, "var1.diff", ESRP_NWIS, pal = GA::jet.colors,
           main = "Prediction Differences", net.idxs = which(ans$is.net),
           rm.idxs = which(ans$is.rm), xlim = xlim, ylim = ylim)

# Restart GA using previous "best" solution
ans <- OptimizeNetwork(ESRP_NWIS, ESRP_NED, ESRP_Boundary,
                       network.nm = "INL", nsites = 20, model = model,
                       formula = fo, xlim = xlim, ylim = ylim,
                       grd.fact = 5, obj.weights = c(100, 1, 1, 1),
                       maxiter = 3, suggestions = ans$ga.ans@population)
}

\keyword{optimize}
