\name{OptimizeNetwork}

\alias{OptimizeNetwork}

\title{Optimize Observatin Network}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information. A kriging-based
genetic algorithm is used to solve the optimization problem.
}

\usage{
OptimizeNetwork(pts, grd, ply, network, nsites, vg.model, formula,
                nmax = Inf, grd.fact = 1, niters = 200, pop.size = 200,
                obj.weights = c(1, 1, 1, 1))
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
\item{grd.fact}{integer; aggregation factor expressed as number of cells in
  each direction (horizontally and vertically).}
\item{niters}{integer; the number of iterations used in the genetic algorithm.}
\item{pop.size}{integer; the population size used in the genetic algorithm.}
\item{obj.weights}{numeric; vector of length 4 giving the weights for each
  objective function in the optimzation, see \sQuote{details} below.}
}

\details{
Something.
}

\value{
Returns a list with components:
\item{rm.pts}{numeric; vector of site numbers to remove from the network.}
\item{is.rm.idx}{logical; vector of something.}
\item{obj.values}{numeric; vector of length 4 giving something.}
\item{ans.rep}{integer; something.}
\item{elapsed.time}{numeric; something.}
\item{rbga.ans}{rbga; something.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{krige}}, \code{\link{rbga}}
}

\examples{\donttest{data(ESRP_NED500m)
data(ESRP_WaterLevels_2008)
data(ESRP_SpatialDomain)

# Variogram
vg.model <- vgm(psill=4500, model = "Sph", range = 100, nugget = 0)
formula <- var1 ~ var2

# Optimize "State" network:
ga <- OptimizeNetwork(ESRP_WaterLevels_2008, ESRP_NED500m, ESRP_SpatialDomain,
                      network = "State", nsites = 20, vg.model = vg.model,
                      formula = formula, nmax = 50, grd.fact = 10, niters = 10,
                      pop.size = 300, obj.weights = c(10, 1, 1, 1))
cat(summary(ga$rbga.ans))

# Run kriging
newdata <- ESRP_NED500m
newdata$var2 <- newdata$var2 * overlay(newdata, ESRP_SpatialDomain) # crop
locations <- ESRP_WaterLevels_2008[!ga$is.rm.idx, ]
kr <- krige(formula = formula, newdata = newdata, model = vg.model,
            locations = locations)
kr$var1.se <- sqrt(kr$var1.var)

# Plot prediction and standard error maps
PlotRaster(kr, "var1.pred", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           xlim = c(-115.25, -111.50), ylim = c(42.25, 44.50),
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions", rm.idxs = which(ga$is.rm.idx))
PlotRaster(kr, "var1.se", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           xlim = c(-115.25, -111.50), ylim = c(42.25, 44.50),
           pal = terrain.colors, main = "Standard errors",
           rm.idxs = which(ga$is.rm.idx))

# Optimize "INL" network:
ga <- OptimizeNetwork(ESRP_WaterLevels_2008, ESRP_NED500m, ESRP_SpatialDomain,
                      network = "INL", nsites = 20, vg.model = vg.model,
                      formula = formula, nmax = 50, grd.fact = 5, niters = 10,
                      pop.size = 300, obj.weights = c(10, 1, 1, 1))
cat(summary(ga$rbga.ans))

# Run kriging
locations <- ESRP_WaterLevels_2008[!ga$is.rm.idx, ]
kr <- krige(formula = formula, newdata = newdata, model = vg.model,
            locations = locations)
kr$var1.se <- sqrt(kr$var1.var)

# Plot prediction and standard error maps
PlotRaster(kr, "var1.pred", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           xlim = c(-113.30, -112.20), ylim = c(43.30, 44.00),
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions", rm.idxs = which(ga$is.rm.idx))
PlotRaster(kr, "var1.se", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           xlim = c(-113.30, -112.20), ylim = c(43.30, 44.00),
           pal = terrain.colors, main = "Standard errors",
           rm.idxs = which(ga$is.rm.idx))
}}

\keyword{optimize}
