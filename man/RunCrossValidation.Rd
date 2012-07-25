\name{RunCrossValidation}

\alias{RunCrossValidation}

\title{Run Cross Validation}

\description{
Cross validation function for kriging; a wrapper around
\code{\link[gstat]{krige.cv}} in the \pkg{gstat} package.
This function may be used to identify negative aspects of a kriging
model, such as the worst errors or the areas with consistent bias.
}

\usage{
RunCrossValidation(formula, pts, vg.model, nmax = Inf, nfold = nrow(pts),
                   projargs = proj4string(pts))
}

\arguments{
\item{formula}{formula; defines the dependent variable as a linear model of
  independent variables.}
\item{pts}{SpatialPointsDataFrame; data at observation sites.}
\item{vg.model}{variogramModel; variogram model of dependent variable defined
  by a call to \code{\link{vgm}}.}
\item{nmax}{numeric; for local kriging, the number of nearest observations
  that should be used for a kriging prediction or simulation, where nearest
  is defined in terms of the space of the spatial locations. By default,
  all observations are used.}
\item{nfold}{integer; if larger than 1, then apply n-fold cross validation;
  if \code{nfold} equals \code{nrow(pts)} (the default), apply leave-one-out
  cross validation.}
\item{projargs}{character; projection arguments; the arguments must be entered
  exactly as in the PROJ.4 documentation.}
}

\value{
Returns a list with components:
\item{cv}{SpatialPointsDataFrame; columns of prediction and prediction variance
  of cross validated data points, observed values, residuals, zscore (residual
  divided by kriging standard error), and fold.}
\item{me}{numeric; mean error, ideally 0.}
\item{mspe}{numeric; mean squared prediction error, ideally small.}
\item{cor.op}{numeric; correlation of observed and predicted, ideally 1.}
\item{cor.pr}{numeric; correlation of predicted and residual, ideally 0.}
}

\author{J.C. Fisher}

\seealso{
\code{\link[gstat]{krige.cv}}
}

\examples{
data(ESRP_WaterLevels_2008)
data(ESRP_Boundary)
data(ESRP_NED)

# Set datum and projection
crs <- CRS(proj4string(ESRP_NED))
pts <- spTransform(ESRP_WaterLevels_2008, crs)
ply <- spTransform(ESRP_Boundary, crs)

# Exclude grid points outside polygon
grd <- ESRP_NED
grd$var2 <- grd$var2 * sp::overlay(grd, ply)

# Set axis limits
xlim <- c(10000, 328000)
ylim <- c(81200, 335700)

# Kriging function
Krige <- function(fo, vg.model, xlim, ylim) {
  kr <- gstat::krige(formula = fo, locations = pts, newdata = grd,
                     model = vg.model)
  kr$var1.se <- sqrt(kr$var1.var)
  pal1 <- colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9"))
  pal2 <- terrain.colors
  PlotRaster(kr, "var1.pred", pts, xlim = xlim, ylim = ylim, pal = pal1,
             main = "Predictions", contour = TRUE)
  PlotRaster(kr, "var1.se",   pts, xlim = xlim, ylim = ylim, pal = pal2,
             main = "Standard errors")
}

### Ordinary Kriging (OK):
fo <- var1 ~ 1
vg <- variogram(fo, pts)
plot(variogram(fo, pts, cloud = TRUE), pch = 3)
vg.model <- fit.variogram(vg, vgm(model = "Lin", nugget = 0))
plot(vg, vg.model, main = "Linear variogram model (var1 ~ 1)")
cv <- RunCrossValidation(fo, pts, vg.model = vg.model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

### Universal Kriging (UK), accounts for spatial trend:
fo <- var1 ~ x + y
lm.trend <- lm(fo, data = pts)
summary(lm.trend)

vg <- variogram(fo, pts)
vg.model <- fit.variogram(vg, vgm(model = "Lin", nugget = 0))
plot(vg, vg.model, main = "Residual variogram model (var1 ~ x + y)")
Krige(fo, vg.model, xlim, ylim)
cv <- RunCrossValidation(fo, pts, vg.model = vg.model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

### Kriging with External Drift (KED); assumes var1 forms a subdued replica of var2:
fo <- var1 ~ var2
lm.trend <- lm(fo, data = pts)
summary(lm.trend)
plot(fo, pts, main = "Correlation plot and fitted regression model")
abline(lm.trend)

vg <- variogram(fo, pts) 
vg.model <- fit.variogram(vg, vgm(psill = 4500, model = "Sph", range = 100000, 
                                  nugget = 0), fit.method = 1)
plot(vg, vg.model, main = "Residual variogram model (var1 ~ var2)")
Krige(fo, vg.model, xlim, ylim)
cv <- RunCrossValidation(fo, pts, vg.model = vg.model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")
PlotBubble(cv$cv, "residual", ply , xlim , ylim, main = "Residuals")

# KED in a local neighbourhood:
cv <- RunCrossValidation(fo, pts, vg.model = vg.model, nmax = 50)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")
}

\keyword{misc}
