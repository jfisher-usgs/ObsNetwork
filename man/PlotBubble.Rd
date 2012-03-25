\name{PlotBubble}

\alias{PlotBubble}

\title{Create a Bubble Plot of Spatial Data}

\description{
Bubble plot for spatial data with attributes.
This is a wrapper around \code{\link[sp]{bubble}} in the \pkg{sp} package.
}

\usage{
PlotBubble(pts, zcol, ply, xlim = bbox(ply)[1, ], ylim = bbox(ply)[2, ],
           main = "", gr.type = "windows", gr.file = NULL)
}

\arguments{
\item{pts}{SpatialPointsDataFrame; data at observation sites.}
\item{zcol}{character; z-variable column name in attribute table of \code{pts}.}
\item{ply}{SpatialPolygonsDataFrame; polygon to include in layout.}
\item{xlim}{numeric; vector of length 2 giving left and right limits for
  the x-axis.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for
  the y-axis.}
\item{main}{character; main plot title to be placed on top.}
\item{gr.type}{character; the name of a graphics driver, either
  "postscript", "pdf", "png", or the default "windows".}
\item{gr.file}{character; the path of a file for writing the graphics,
               only used if \code{gr.type} is not "windows".}
}


\details{
The spatial polygon \code{ply} is transformed to the map projection
and datum of the observation sites, \code{pts}.
}

\value{
Returns (or plots) the bubble plot. Values plotted in the key include the
five quantiles: minimum, 25th percentile, median, 75th percentile, and maximum.
}

\author{J.C. Fisher}

\seealso{
\code{\link[sp]{bubble}}, \code{\link{OpenGraphicsDevice}}
}

\examples{
data(ESRP_WaterLevels_2008)
data(ESRP_Boundary)

PlotBubble(ESRP_WaterLevels_2008, "acy", ESRP_Boundary,
           main = "Measurment error")
PlotBubble(ESRP_WaterLevels_2008, "sd", ESRP_Boundary,
           xlim = c(178000, 257500), ylim = c(202000, 272000),
           main = "Standard deviation")
}

\keyword{hplot}
