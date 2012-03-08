\name{RunCrossValidation}

\alias{RunCrossValidation}

\title{Run Crossvalidation}

\description{
Something.
}

\usage{
RunCrossValidation(formula, obs, vg.model, nmax = Inf, nfold = nrow(obs))
}

\arguments{
\item{formula}{something}
\item{obs}{something}
\item{vg.model}{something}
\item{nmax}{something}
\item{nfold}{something}
}

\details{
Something...
}

\value{
Something
}

\author{J.C. Fisher}

\seealso{
\code{\link[gstat]{krige.cv}}
}

\examples{
data(ESRP_WaterLevels_2008)

vg.model <- vgm(psill=4200, model="Sph", range=80, nugget=0)
cv <- RunCrossValidation(var1~var2, ESRP_WaterLevels_2008,
                         vg.model = vg.model, nmax = 20)

cv$me # mean error, ideally 0
cv$mspe # mean square prediction error, ideally small
cv$cor.obs.pred # correlation observed and predicted, ideally 1
cv$cor.pred.res # correlation predicted and residual, ideally 0

data(ESRP_SpatialDomain)
PlotBubble(cv$cv, "residual", main="Residuals", ply = ESRP_SpatialDomain,
           xlim = c(-115.25, -111.5), ylim = c(42.25, 44.5))
PlotBubble(cv$cv, "residual", main="Residuals", ply = ESRP_SpatialDomain,
           xlim = c(-113.3, -112.2), ylim = c(43.3, 44.0))
}

\keyword{misc}
