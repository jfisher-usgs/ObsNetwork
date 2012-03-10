\name{RunCrossValidation}

\alias{RunCrossValidation}

\title{Run Crossvalidation}

\description{
Cross validation function for kriging; a wrapper around
\code{\link[gstat]{krige.cv}} in the \pkg{gstat} package.
This function may be used to make an informed decision as to which
mapped surface provides a more accurate prediction based on
cross-validation statistics.
}

\usage{
RunCrossValidation(formula, pts, vg.model, nmax = Inf, nfold = nrow(pts))
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
data(ESRP_SpatialDomain)

# Ordinary Kriging (OK):
vg <- variogram(var1 ~ 1, ESRP_WaterLevels_2008)
vg.model <- fit.variogram(vg, vgm(model = "Lin", nugget = 0))
print(plot(vg, vg.model, main = "Linear variogram model (var1 ~ 1)"))
cv <- RunCrossValidation(var1 ~ 1, ESRP_WaterLevels_2008, vg.model = vg.model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

# Check for the presence of a detectable spatial trend or "drift" in the data
lm.trend <- lm(var1 ~ x + y, data = ESRP_WaterLevels_2008)
summary(lm.trend)

# Universal Kriging (UK):
vg <- variogram(var1 ~ x + y, ESRP_WaterLevels_2008)
vg.model <- fit.variogram(vg, vgm(model = "Lin", nugget = 0))
print(plot(vg, vg.model, main = "Residual variogram model (var1 ~ x + y)"))
cv <- RunCrossValidation(var1 ~ x + y, ESRP_WaterLevels_2008,
                         vg.model = vg.model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

# Check for the presence of a correlation with an auxiliary predictor
lm.trend <- lm(var1 ~ var2, data = ESRP_WaterLevels_2008)
summary(lm.trend)
plot(var1 ~ var2, ESRP_WaterLevels_2008,
     main = "Correlation plot and fitted regression model")
abline(lm.trend)

# Kriging with External Drift (KED):
vg <- variogram(var1 ~ var2, ESRP_WaterLevels_2008)
vg.model <- vgm(psill = 4500, model = "Sph", range = 100, nugget = 0)
print(plot(vg, vg.model, main = "Residual variogram model (var1 ~ var2)"))
cv <- RunCrossValidation(var1 ~ var2, ESRP_WaterLevels_2008,
                         vg.model = vg.model)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

# KED in a local neighbourhood:
cv <- RunCrossValidation(var1 ~ var2, ESRP_WaterLevels_2008,
                         vg.model = vg.model, nmax = 50)
cat(cv$me, cv$mspe, cv$cor.op, cv$cor.pr, "\n")

# Plot residuals from cross-validation
PlotBubble(cv$cv, "residual", main = "Residuals", ply = ESRP_SpatialDomain,
           xlim = c(-115.25, -111.50), ylim = c(42.25, 44.50))
PlotBubble(cv$cv, "residual", main = "Residuals", ply = ESRP_SpatialDomain,
           xlim = c(-113.30, -112.20), ylim = c(43.30, 44.00))
}

\keyword{misc}
