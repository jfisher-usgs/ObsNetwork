\name{PlotBubble}

\alias{PlotBubble}

\title{Create a Bubble Plot of Spatial Data}

\description{
Bubble plot of a spatial points data frame object.
This is a wrapper around \code{\link{bubble}} in the \pkg{sp} package.
}

\usage{
PlotBubble(pts, zcol, ply, xlim = bbox(ply)[1, ], ylim = bbox(ply)[2, ],
           main = "", gr.type = "windows", gr.file = NULL)
}

\arguments{
\item{pts}{SpatialPointsDataFrame; spatial data at observation sites.}
\item{zcol}{character; z-variable column name.}
\item{ply}{SpatialPolygonsDataFrame; polygon to include in layout.}
\item{xlim}{numeric; vector of length 2 giving left and right limits for
  the x-axis.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for
  the y-axis.}
\item{main}{character; main title to be placed on top.}
\item{gr.type}{character; the name of a graphics driver, either
  "pdf", "postscript", "png", or "windows".}
\item{gr.file}{character; the name of a file for writing the graphics,
               only used if \code{gr.type} is not "windows".}
}

\value{
Returns (or plots) the bubble plot. Values plotted in the key include the
five quantiles: minimum, 25th percentile, median, 75th percentile, and maximum.
}

\author{J.C. Fisher}

\seealso{
\code{\link{bubble}}, \code{\link{OpenGraphicsDevice}}
}

\examples{
data(ESRP_WaterLevels_2008)
data(ESRP_SpatialDomain)

PlotBubble(ESRP_WaterLevels_2008, "acy", ESRP_SpatialDomain,
           main = "Measurment error")
PlotBubble(ESRP_WaterLevels_2008, "sd", ESRP_SpatialDomain,
           xlim = c(-113.3, -112.2), ylim = c(43.3, 44.0),
           main = "Standard deviation")
}

\keyword{hplot}
