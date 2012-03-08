\name{RunCrossValidation}

\alias{RunCrossValidation}

\title{Run Crossvalidation}

\description{
Cross validation function for kriging. This is a wrapper around
\code{\link[gstat]{krige.cv}} in the \pkg{gstat} package.
}

\usage{
RunCrossValidation(formula, obs, vg.model, nmax = Inf, nfold = nrow(obs))
}

\arguments{
\item{formula}{formula; defines the dependent variable as a linear model of
  independent variables.}
\item{obs}{SpatialPointsDataFrame; data at observation sites.}
\item{vg.model}{variogramModel; variogram model of dependent variable defined
  by a call to \code{\link{vgm}}.}
\item{nmax}{numeric; for local kriging, the number of nearest observations
  that should be used for a kriging prediction or simulation, where nearest
  is defined in terms of the space of the spatial locations. By default,
  all observations are used.}
\item{nfold}{integer; if larger than 1, then apply n-fold cross validation;
  if \code{nfold} equals \code{nrow(obs)} (the default), apply leave-one-out
  cross validation.}
}

\value{
Returns a list with components:
\item{cv}{SpatialPointsDataFrame; columns of prediction and prediction variance
  of cross validated data points, observed values, residuals, zscore (residual
  divided by kriging standard error), and fold.}
\item{me}{numeric; mean error, ideally 0.}
\item{mspe}{numeric; mean squared prediction error, ideally small.}
\item{cor.obs.pred}{numeric; correlation of observed and predicted, ideally 1.}
\item{cor.pred.res}{numeric; correlation of predicted and residual, ideally 0.}
}

\author{J.C. Fisher}

\seealso{
\code{\link[gstat]{krige.cv}}
}

\examples{
data(ESRP_WaterLevels_2008)

# Linear fitted variogram model
vg <- variogram(var1 ~ 1, ESRP_WaterLevels_2008)
vgm.lin <- fit.variogram(vg, vgm(model = "Lin", nugget = 0))
print(plot(vg, vgm.lin))

# Cross validation: ordinary kriging with linear model
cv.ok <- RunCrossValidation(var1 ~ 1, ESRP_WaterLevels_2008, vg.model = vgm.lin,
                            nmax = 20)
cv.ok$me
cv.ok$mspe
cv.ok$cor.obs.pred
cv.ok$cor.pred.res

# Spherical variogram model
vg <- variogram(var1 ~ var2, ESRP_WaterLevels_2008)
vgm.sph <- vgm(psill = 4500, model = "Sph", range = 100, nugget = 0)
print(plot(vg, vgm.sph))

# Cross validation: universal kriging with spherical model
cv.uk <- RunCrossValidation(var1 ~ var2, ESRP_WaterLevels_2008,
                            vg.model = vgm.sph, nmax = 20)
cv.uk$me
cv.uk$mspe
cv.uk$cor.obs.pred
cv.uk$cor.pred.res

# Plot residuals
data(ESRP_SpatialDomain)
PlotBubble(cv.uk$cv, "residual", main = "Residuals", ply = ESRP_SpatialDomain,
           xlim = c(-115.25, -111.5), ylim = c(42.25, 44.5))
PlotBubble(cv.uk$cv, "residual", main = "Residuals", ply = ESRP_SpatialDomain,
           xlim = c(-113.3, -112.2), ylim = c(43.3, 44.0))
}

\keyword{misc}
