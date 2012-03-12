\name{OptimizeNetwork}

\alias{OptimizeNetwork}

\title{Optimize Observation Network}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information. A kriging-based
genetic algorithm (GA) is used to solve the multiobjective optimization
problem.
}

\usage{
OptimizeNetwork(pts, grd, ply, network, nsites, vg.model, formula, nmax = Inf,
                xlim = bbox(grd)[1, ], ylim = bbox(grd)[2, ], grd.fact = 1,
                niters = 200, pop.size = 200, obj.weights = c(1, 1, 1, 1))
}

\arguments{
\item{pts}{SpatialPointsDataFrame; data at observation sites. A
  description of the required data frame variables is given in the
  \sQuote{Value} section of \code{\link{ProcessObservations}}.}
\item{grd}{SpatialGridDataFrame; raster data with independent variables.}
\item{ply}{SpatialPolygonsDataFrame; a polygon used to crop the raster grid
  (optional).}
\item{network}{character; the name of an observation network. Only sites
  included in this network will be recognized as decision variables in the
  optimization problem; this argument is optional, in its absence all
  sites will be recognized as decision variables.}
\item{nsites}{integer; the number of sites to remove from the observation
  network (the number of decision variables in the optimization problem).}
\item{vg.model}{variogramModel; variogram model of dependent variable
   defined by a call to \code{\link{vgm}}.}
\item{formula}{formula; defines the dependent variable as a linear model of
  the independent variables.}
\item{nmax}{numeric; for local kriging, the number of nearest sites
  that should be used for a kriging prediction or simulation, where nearest
  is defined in terms of the space of the spatial locations. By default,
  all sites are used.}
\item{xlim}{numeric; vector of length 2 giving left and right limits for
  the x-axis, used to crop the raster grid.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for
  the y-axis, used to crop the raster grid.}
\item{grd.fact}{integer; aggregation factor for the raster grid expressed as the
  number of cells in each direction (horizontally and vertically).}
\item{niters}{integer; the number of iterations used in the GA.}
\item{pop.size}{integer; the population size used in the GA.}
\item{obj.weights}{numeric; vector of length 4 giving the weights for each
  objective in the multi-objective optimzation problem,
  see \sQuote{details} section below.}
}

\details{
A solution to the multiobjective optimization problem is found by minimizing
the aggregate objective function, the weighted linear sum of 4 objectives. The
objectives are given as:
\enumerate{
  \item Mean prediction error from the application of kriging.
  \item Root-mean-square error, difference between predicted and measured
        values at removed sites.
  \item Mean standard deviation, variability of measurement over time at
        removed sites.
  \item Mean measurement error.
}
The optimal solution obtained will depend on the relative values of the weights
specified, \code{obj.weights}. For example, if a higher weight is specified for
the mean prediction error, the solution will be one that favors a smaller
prediction error over a small root-mean squared error, mean standard deviation,
and mean measurement error.
}

\value{
Returns a list with components:
\item{rm.pts}{SpatialPointsDataFrame; a subset of \code{pts} giving the
  optimal values for the decision variables. This data frame consists of
  \code{nsites} rows, each row corresponds to a site that is to be
  removed from the observation network.}
\item{is.rm.idx}{logical; vector of length equal to the number of rows in
  \code{pts} specifying sites to remove from the observation network.}
\item{obj.values}{matrix; objective values after each iteration of the GA.
  The matrix has \code{niters} rows and 5 columns, 4 objective values and
  their sum.}
\item{ans.rep}{numeric; number of times the final solution was repeated.}
\item{elapsed.time}{numeric; elapsed time for running the GA in hours.}
\item{kr}{SpatialGridDataFrame; a data frame containing the coordinates of
  \code{grd} cropped to the axis limits and polygon. Attributes columns contain
  prediction and standard error from kriging of the optimized observation
  network.}
\item{rbga.ans}{rbga; the returned value of \code{\link{rbga}}.}
The status of the objective values after each GA iteration is plotted.
}

\author{J.C. Fisher}

\seealso{
\code{\link{krige}}, \code{\link{rbga}}
}

\examples{\donttest{data(ESRP_NED500m)
data(ESRP_WaterLevels_2008)
data(ESRP_SpatialDomain)

# Variogram
vg.model <- vgm(psill = 4500, model = "Sph", range = 100, nugget = 0)
formula <- var1 ~ var2

# Optimize "State" network:
xlim <- c(-115.25, -111.50)
ylim <- c(  42.25,   44.50)
ga <- OptimizeNetwork(ESRP_WaterLevels_2008, ESRP_NED500m, ESRP_SpatialDomain,
                      network = "State", nsites = 20, vg.model = vg.model,
                      formula = formula, nmax = 50, grd.fact = 5, niters = 10,
                      pop.size = 300, obj.weights = c(10, 1, 1, 1))

# Plot prediction and standard error maps
PlotRaster(ga$kr, "var1.pred", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions", rm.idxs = which(ga$is.rm.idx),
           xlim = xlim, ylim = ylim)
PlotRaster(ga$kr, "var1.se", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           pal = terrain.colors, main = "Standard errors",
           rm.idxs = which(ga$is.rm.idx), xlim = xlim, ylim = ylim)

# Optimize "INL" network:
xlim <- c(-113.30, -112.20)
ylim <- c(  43.30,   44.00)
ga <- OptimizeNetwork(ESRP_WaterLevels_2008, ESRP_NED500m, ESRP_SpatialDomain,
                      network = "INL", nsites = 20, vg.model = vg.model,
                      formula = formula, nmax = Inf, grd.fact = 5, niters = 10,
                      pop.size = 300, obj.weights = c(10, 1, 1, 1),
                      xlim = xlim, ylim = ylim)

# Plot prediction and standard error maps
PlotRaster(ga$kr, "var1.pred", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions", rm.idxs = which(ga$is.rm.idx),
           xlim = xlim, ylim = ylim)
PlotRaster(ga$kr, "var1.se", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           pal = terrain.colors, main = "Standard errors",
           rm.idxs = which(ga$is.rm.idx),
           xlim = xlim, ylim = ylim)
}}

\keyword{optimize}
