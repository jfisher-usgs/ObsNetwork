\name{OptimizeNetwork}

\alias{OptimizeNetwork}

\title{Optimize Observatin Network}

\description{
Determine sites to exclude from an existing observation network because
they provide little or no beneficial added information.
}

\usage{
OptimizeNetwork(pts, grd, ply, network, nsites, vg.model, formula,
                nmax = Inf, grd.fact = 1, niters = 200, pop.size = 200,
                obj.weights = c(1, 1, 1, 1))
}

\arguments{
\item{pts}{SpatialPointsDataFrame; data at observation sites.}
\item{grd}{SpatialGridDataFrame; raster data.}
\item{ply}{SpatialPolygonsDataFrame; a polygon used to crop the raster data in
  \code{grd} (optional).}
\item{network}{character; only observation sites in this network
  will be included in the optimization (optional).}
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
\item{grd.fact}{integer; aggregation factor expressed as number of cells in
  each direction (horizontally and vertically).}
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
