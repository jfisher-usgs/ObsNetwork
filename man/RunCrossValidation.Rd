\name{RunCrossValidation}

\alias{RunCrossValidation}

\title{Run Cross Validation}

\description{
Cross validation function for kriging; a wrapper around the \code{\link[gstat]{krige.cv}} function in the \pkg{gstat} package.
This function may be used to identify negative aspects of a kriging model, such as the worst errors or the areas with consistent bias.
}

\usage{
RunCrossValidation(formula, pts, model, nfold = nrow(pts), ...,
                   projargs = proj4string(pts))
}

\arguments{
\item{formula}{\code{formula}; defines the dependent variable as a linear model of independent variables.}
\item{pts}{\code{SpatialPointsDataFrame}; the data at observation sites.}
\item{model}{\code{variogramModel}; the variogram model of dependent variable defined by a call to \code{\link{vgm}}.}
\item{nfold}{\code{numeric}; for local kriging, the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations.
  By default, all observations are used.}
\item{...}{other arguments that will be passed to \code{\link{predict.gstat}}.}
\item{projargs}{\code{character}; the projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation.}
}

\value{
Returns a \code{list} with components:
\item{cv}{\code{SpatialPointsDataFrame}; the attributes of prediction and prediction variance of cross validated data points, observed values, residuals, zscore (residual divided by kriging standard error), and fold.}
\item{me}{\code{numeric}; the mean error, ideally 0.}
\item{mspe}{\code{numeric}; the mean squared prediction error, ideally small.}
\item{cor.op}{\code{numeric}; the correlation of observed and predicted, ideally 1.}
\item{cor.pr}{\code{numeric}; the correlation of predicted and residual, ideally 0.}
}

\author{J.C. Fisher}

\seealso{
\code{\link[gstat]{krige.cv}}
}

\examples{
data(ESRP_NWIS)
data(ESRP_Boundary)
data(ESRP_NED)

# Set datum and projection
crs <- CRS(proj4string(ESRP_NED))
pts <- spTransform(ESRP_NWIS, crs)
ply <- spTransform(ESRP_Boundary, crs)

# Exclude grid points outside polygon
grd <- ESRP_NED
grd$var2 <- grd$var2 * over(as(grd, "SpatialPoints"),
                            as(ply, "SpatialPolygons"))

# Set axis limits
xlim <- c(10000, 328000)
ylim <- c(81200, 335700)

# Kriging function with plotting routine
Krige <- function(fo, model, xlim, ylim, ...) {
  kr <- krige(formula = fo, locations = pts, newdata = grd, model = model, ...)
  kr$var1.se <- sqrt(kr$var1.var)
  pal1 <- colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9"))
  pal2 <- terrain.colors
  PlotRaster(kr, "var1.pred", pts, xlim = xlim, ylim = ylim, pal = pal1,
             main = "Predictions", contour = TRUE)
  PlotRaster(kr, "var1.se",   pts, xlim = xlim, ylim = ylim, pal = pal2,
             main = "Standard errors")
}

### Universal Kriging (UK), accounts for linear spatial trend:
fo <- var1 ~ x + y
lm.trend <- lm(fo, data = pts)
summary(lm.trend)

vg <- variogram(fo, pts, cutoff = 150000)
model <- fit.variogram(vg, vgm(psill = 1500, model = "Sph", range = 100000,
                               nugget = 0), fit.method = 1)
plot(vg, model, main = "Residual variogram model (var1 ~ x + y)")

Krige(fo, model, xlim, ylim)
cv <- RunCrossValidation(fo, pts, model = model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

### Kriging with External Drift (KED); assumes var1 forms a subdued replica of var2:
fo <- var1 ~ var2
lm.trend <- lm(fo, data = pts)
summary(lm.trend)
plot(fo, pts, main = "Correlation plot and fitted regression model")
abline(lm.trend)

vg <- variogram(fo, pts)
model <- fit.variogram(vg, vgm(psill = 4500, model = "Sph", range = 100000,
                               nugget = 0), fit.method = 1)
plot(vg, model, main = "Residual variogram model (var1 ~ var2)")
Krige(fo, model, xlim, ylim)
cv <- RunCrossValidation(fo, pts, model = model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")
PlotBubble(cv$cv, "residual", ply , xlim , ylim, main = "Residuals")

# KED in a local neighborhood:
cv <- RunCrossValidation(fo, pts, model = model, nmax = 50)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")
}

\keyword{misc}
